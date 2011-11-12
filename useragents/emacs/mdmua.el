;;; mdmua - maildir user agent -*- lexical-binding: t -*-
;; Copyright (C) 2009-2011 Nic Ferrier

;; A simple emacs user agent for md, the stateless maildir user agent.

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 5th October 2009
;; Version: 0.72
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an EmacsLISP user agent based around my 'md' command, a
;; stateless unix program for reading and managing mail in maildirs.
;;
;; mdmua is mainly a collection of async processing functions, all
;; processing the output of 'md'.
;;
;; 'md' can be found in pypi here: http://pypi.python.org/pypi/md

;;; Development Notes
;;
;; 2011-10-04 Problems with the folder view - I started off using
;; property lists for carrying the folder state, but I think that was
;; a mistake. It would be better to use record objects.

;;; Source code
;;
;; mdmua's source code can be found here:
;;   http://github.com/nicferrier/md

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    mdmua--private-function
;;
;; for private functions and for private variables.


;;; Code:

(require 'json)

(eval-when-compile (require 'cl))

(defvar mdmua-buffer '()
  "The buffer where everything is rendered.")

(defvar mdmua-mode-map '())

(defvar mdmua-md-bin-path "md"
  "Where md can be found.")


(defvar mdmua-folders nil
  "List of folders in the mdmua list buffer. Buffer local.")

(defvar mdmua-pulling nil
  "Whether the current mdmua list buffer is pulling or not. Buffer local.")

(defcustom mdmua-maildir "~/.maildirs/nic"
  "Where the maildir can be found."
  :group 'md)

;; Utility functions

(defvar mdmua--command-log (get-buffer-create "*mdmua-command-log*")
  "Just a log of commands.

We don't manage this at all currently but it's a very useful
debugging tool so it's good to keep around.")

(defun mdmua-util--strip (str)
  "Standard line break removal"
  (or (and (string-match "\\(.*\\)\n" str)
           (match-string 1 str))
      str))

(defun mdmua-util--buffer-lines ()
  "A simple tool to return a list of all the lines in a buffer."
  (let ((lst '()))
    (save-excursion
      (goto-char (point-min))
      (let ((pos (point)))
        (while (not (eobp))
          (forward-visible-line 1)
          (setq lst (append lst
                            (list
                             (mdmua-util--strip
                              (buffer-substring-no-properties pos (point))))))
          (setq pos (point)))
        ))
    lst))


(defmacro with-mdmua-shell-command (command buffer &rest sentinel-cond)
  "Executes the COMMAND with the SENTINEL-COND callback.

This is a with-process-shell-command .

BUFFER is passed directly to 'start-process-shell-command'.

This version does not interpolate the correct md binary. It just
presumes you are executing a sane shell command.

SENTINEL-COND is the inner part of a cond to match the
signal.

For example:

  (with-mdmua-command \"md ls\" somebuf
    ((equal signal \"finished\\n\")
     ;; Clean up?
     (with-current-buffer (get-buffer \"* log *\")
       (insert \"some text\\\n\")))
    )

Additionally, the current buffer is set to the buffer of the process."
  (declare (indent defun))
  (let ((bufvar (make-symbol "buf"))
        (cmdvar (make-symbol "cmd"))
        (procvar (make-symbol "proc"))
        (sentinel-cb (make-symbol "sentinel"))
        )
    `(let* ((,bufvar ,buffer)
            (,cmdvar ,command))
       ;; Update the command log
       (with-current-buffer mdmua--command-log
         (insert ,cmdvar "\n"))
       ;; Now start the shell process
       (let ((,procvar (start-process-shell-command "mdmua" ,bufvar ,cmdvar)))
         ;; Capture the sentinel
         (let ((,sentinel-cb (lambda (process signal)
                               (with-current-buffer (process-buffer process)
                                 (cond ,@sentinel-cond)))))
           ;; This makes the standard form for error checking hard.
           ;;(funcall ,sentinel-cb ,procvar "__BEGIN__\n")
           (set-process-sentinel ,procvar ,sentinel-cb)
           )))
    )
  )

(defmacro with-mdmua-command (command buffer &rest sentinel-cond)
  "Run an mdmua COMMAND with the md binary.

See 'with-mdmua-shell-command' which this uses but just ensures the
correct mdmua binary is used from 'mdmua-md-bin-path' and with the
configured 'mdmua-maildir'."
  (declare (indent defun))
  (let ((cmdvar (make-symbol "cmd")))
    `(let ((,cmdvar ,command))
       (with-mdmua-shell-command
         (format "%s -M %s %s"
                 mdmua-md-bin-path
                 (expand-file-name mdmua-maildir)
                 ,cmdvar)
         ,buffer
         ,@sentinel-cond))))


;; Main program

(defun mdmua--msg-render (msg folder)
  "Format and propertize the MSG in the FOLDER."
  (propertize
   (format "% 22s  %30s  %s"
           (or (aget msg 'date)
               "0-00-00")
           (or (elt (aget msg 'from) 0)
               (elt (aget msg 'from) 1)
               "nobody@nowhere")
           (or (aget msg 'subject)
               " "))
   'key (aget msg 'key)
   'folder folder
   ;; This should be done with syntax instead of
   ;; directly
   'face
   (let* ((flags (aget msg 'flags))
          (flags-list (string-to-list flags)))
     (cond
      ((member ?T flags-list)
       '(foreground-color . "Red"))
      ((member ?S flags-list)
       '(foreground-color . "Black"))
      (t
       '(foreground-color . "Blue"))))))

(defun mdmua-list (folder &optional callback)
  "List the messages in FOLDER.

Makes a hidden buffer for the folder with the intention that the
buffer will act as a cache for the folder.

If CALLBACK is provided it is called with the buffer when all is
done."
  (interactive "MFolder: ")
  (let* ((folderref (if (equal folder "INBOX") "" folder))
         (lstcommand (format "lisp -r %s" folderref))
         (lstbuf (generate-new-buffer (format " *mdmua-list-%s* " folder)))
         (outbuf (generate-new-buffer (format " *mdmua-list-buf-%s* " folder)))
         (do-switch (called-interactively-p 'interactive)))
    (with-mdmua-command lstcommand lstbuf
      ((equal signal "finished\n")
       (goto-char (point-min))
       (loop while (not (eobp))
             do
             (condition-case nil
                 (let ((msg (json-read)))
                   (when msg
                     (with-current-buffer outbuf
                       (insert (mdmua--msg-render msg folder) "\n"))))
               (error
                (forward-line)
                "")))
       (sort-lines nil (point-min) (point-max))
       (kill-buffer lstbuf)
       ;; Call the continuation?
       (when callback
         (funcall callback outbuf))
       ;; Now we move the contents of the outbuf
       (when do-switch
         (switch-to-buffer outbuf))))
    ;; Return the buffer
    outbuf))

(defun mdmua-folders (&optional buffer)
  "List the folders into a buffer."
  (interactive)
  (let ((folderbuf (or buffer (get-buffer-create "*mdmua-folders*")))
        (do-show (called-interactively-p 'interactive)))
    ;; Setup the buffer to be empty
    (with-current-buffer folderbuf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only 't))
    (with-mdmua-command "lsfolders" folderbuf
      ((equal signal "finished\n")
       (let ((inhibit-read-only t))
         (goto-char (point-min))
         ;; Search for folders and mark them up with properties
         (loop
          while (re-search-forward "^\\([A-Za-z0-9_-]+\\)$" nil t)
          do
          (let ((folder-name (match-string 1)))
            (put-text-property
             (line-beginning-position)
             (line-end-position)
             'folder-name folder-name)))
         ;; Reverse sort the folders
         (sort-lines t (point-min) (point-max))
         ;; Add the INBOX
         (insert (propertize "INBOX" 'folder-name "INBOX") "\n")
       (when do-show
         (switch-to-buffer folderbuf)))))))

(defun mdmua-open-folder (folder-name &optional folder-buffer)
  "Open the specified FOLDER-NAME.

Currently we expect the folder to be at point.

Optionally look in the specified buffer."
  (interactive
   (list
    (get-text-property (point) 'folder-name)
    (current-buffer)))
  (let ((folder-buf (or folder-buffer (current-buffer))))
    (mdmua-list
     folder-name
     (lambda (buffer)
       (with-current-buffer folder-buffer
         (save-excursion
           (let ((inhibit-read-only t))
             (goto-char (point-min))
             (re-search-forward (concat "^" folder-name "$") nil t)
             (insert "\n")
             (insert-buffer-substring buffer)
             (kill-buffer buffer))))))))

(defun mdmua-close-folder (folder-name)
  "Close the specified FOLDER-NAME."
  (interactive
   (list
    (get-text-property (point) 'folder-name)))
  (save-excursion
    (forward-line)
    (let* ((p (point))
           (next-line-is-folder
            (progn
              (goto-char (line-beginning-position))
              (looking-at "^[A-Za-z0-9_-]+$"))))
      (when (not next-line-is-folder)
        (let ((inhibit-read-only t))
          (delete-region
           p
           (next-single-property-change p 'folder-name)))))))

(defun mdmua ()
  "MD based mail user agent."
  (interactive)
  (setq mdmua-buffer (get-buffer-create "mdmua"))
  (switch-to-buffer mdmua-buffer)
  (mdmua-folders mdmua-buffer)
  (when (not (local-variable-p 'mdmua-pulling))
    (make-local-variable 'mdmua-pulling)
    (setq mdmua-pulling "no"))
  ;; Got to define a mode-map
  (unless mdmua-mode-map
    (progn
      (setq mdmua-mode-map (make-sparse-keymap))
      (define-key mdmua-mode-map "\r" 'mdmua-open)
      (define-key mdmua-mode-map "\C-F" 'mdmua-open-full)
      (define-key mdmua-mode-map ">" 'mdmua-next-folder)
      (define-key mdmua-mode-map "<" 'mdmua-prev-folder)
      (define-key mdmua-mode-map "d" 'mdmua-trash-message)
      (define-key mdmua-mode-map "r" 'mdmua-on-list)
      (define-key mdmua-mode-map "R" 'mdmua-on-regex)
      (define-key mdmua-mode-map "p" 'mdmua-pull)))
  (use-local-map mdmua-mode-map))

(defun mdmua-open ()
  "Open (or close) the thing at point.

Either a folder or a message.

If we try to open an opened folder, we close it instead."
  (interactive)
  ;; Test to see if we're opening a folder or not
  (if (save-excursion
        ;;(goto-char (point-min))
        (goto-char (line-beginning-position))
        (re-search-forward "^[^ ]" (line-end-position) 't))
      (if (save-excursion
            (forward-line)
            (goto-char (line-beginning-position))
            (looking-at "^[A-Za-z0-9_-]+"))
          (call-interactively 'mdmua-open-folder)
        (call-interactively 'mdmua-close-folder))
    (call-interactively 'mdmua-open-message)))

;; Mandling key'd messages

(defun mdmua-pastebuffer-key (key)
  "Puts the KEY in the pastebuffer.

Once in the pastebuffer you can find the message with other
means."
  (interactive
   (list
    (plist-get (text-properties-at (point)) 'key)))
  (with-temp-buffer
    (insert key)
    (clipboard-kill-ring-save (point-min) (point-max))))

(defun mdmua-message-list-get (folder-buffer folder-name key)
  "Retrieve the message object specified by FOLDER-NAME and KEY."
  (let ((msglist
         (plist-get
          (cdr (assoc folder-name (with-current-buffer folder-buffer
                                    mdmua-folders)))
          :messages)))
    (catch 'foo
      (mapcar
       (lambda (msg)
         (if (equal key (plist-get msg :key))
             (progn
               (throw 'foo msg))))
       msglist))))


;; Pull handling

(defun mdmua-pull ()
  "Run the mdmua pull"
  (interactive)
  (if (with-current-buffer "mdmua"
        (not (equal mdmua-pulling " mdmua:done")))
      (let ((pull-buf (get-buffer-create "* mdmua-pull *")))
        (with-current-buffer "mdmua"
          (setq mdmua-pulling "running")
          (setq mdmua-pull-display-string " mdmua:running")
          )
        (with-mdmua-shell-command "mdpullall" pull-buf
          ((equal signal "finished\n")
           ;; We can now refresh the data
           (with-current-buffer "mdmua"
             (setq mdmua-pulling "done")
             (setq mdmua-pull-display-string " mdmua:done")
             )
           (kill-buffer (process-buffer process)))))))


(defvar mdmua-pull-display-string " mdmua:done"
  ;; FIXME Why does this need a space when it is added to the
  ;; global-mode-string list???
  "The string to show whether the mdmua is being pulled or not")

(defun mdmua-pull-display ()
  "Set the mode line to display when we are pulling"
  (interactive)
  (setq global-mode-string
        (append
         global-mode-string
         (list 'mdmua-pull-display-string))))


;; Message funcs
(require 'qp)

;; The keymap for the message view mode
(defvar mdmua-message--keymap-initializedp nil
  "Is the MDMUA message view mode map initialized yet?

If you want to debug the mode map you can set this back to nil
and it should get reinitialized next time you make the mode.")

;; Hooks for the message mode
(defvar mdmua-message-mode-hook nil
  "The MDMUA message mode hooks.")

(defun mdmua-message-fill ()
  "Allow filling of a paragraph even when read only.

MDMUA message buffers are read only but paragraphs are sometimes
not formatted properly so we provide this command to allow you to
fill them.

Also causes the buffer to be marked not modified."
  (interactive)
  (let ((buffer-read-only nil))
    (fill-paragraph)
    (set-buffer-modified-p nil)))

(define-derived-mode mdmua-message-mode
  message-mode  ;; parent
  "MDMUA Message"  ;; name
  "MDMUA Msg \\{mdmua-message-mode-map}" ;; docstring
  (unless mdmua-message--keymap-initializedp
    (define-key mdmua-message-mode-map "\C-ca" 'message-reply)
    (define-key mdmua-message-mode-map "\C-cw" 'message-wide-reply)
    (define-key mdmua-message-mode-map "F" 'mdmua-message-fill)
    (define-key mdmua-message-mode-map "p" 'mdmua-message-open-part)
    (setq mdmua-message--keymap-initializedp 't))
  ;;set the mode as a non-editor mode
  (put 'mdmua-message-mode 'mode-class 'special)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start paragraph-separate)
  ;;setup the buffer to be read only
  ;; (make-local-variable 'buffer-read-only)
  (setq buffer-read-only 't)
  (set-buffer-modified-p nil)
  ;;run the mode hooks
  (run-hooks 'mdmua-message-mode-hook))


(defvar mdmua-message--struct '()
  "Contains the part structure of the message. Buffer local.")

(defun mdmua-open-message (key &optional no-render)
  "Open the message with KEY.

Specify the prefix arg to NO-RENDER the message.  This can be
useful while we're developing mdmua."
  (interactive
   (list
    (plist-get (text-properties-at (point)) 'key)
    current-prefix-arg))
  (let ((msgbuf (get-buffer-create (format "* mdmua-message-channel-%s *" key)))
        (structbuf (get-buffer-create
                    (format "* mdmua-message-struct-channel-%s *" key))))
    (if (get-buffer key)
        (switch-to-buffer key)
      (with-mdmua-command (format "text %s" key) msgbuf
        ((equal signal "finished\n")
         (rename-buffer key)
         ;; This is a trick from imapua - deals with dodgy line endings
         (subst-char-in-region (point-min) (point-max) ?\r ?\ )
         (mdmua-message-mode)
         (with-mdmua-command (format "struct -j %s" key) structbuf
           ;; Need to process the struct into something we can display
           ((equal signal "finished\n")
            (let ((partlist (save-excursion
                              (goto-char (point-min))
                              (json-read))))
              (kill-buffer structbuf)
              (with-current-buffer msgbuf
                (make-local-variable 'mdmua-message--struct)
                (setq mdmua-message--struct partlist))
              (switch-to-buffer msgbuf)
              (goto-char (point-min))))))
        ;; else
        ('t
         (message "mdmua open message got signal %s" signal)
         (display-buffer msgbuf))))))

(defun mdmua-open-full (key)
  "Open the whole file of the message"
  (interactive
   (list
    (plist-get (text-properties-at (point)) 'key)))
  (let ((buf (get-buffer-create (format "* mdmua-message-channel-%s *" key))))
    (with-mdmua-command (format "file %s" key) buf
      ;; The sentinel code
      ((equal signal "finished\n")
       (switch-to-buffer (process-buffer process))
       (rename-buffer key)
       (mdmua-message-mode)
       (goto-char (point-min)))
      ;; else
      ('t
       (message "mdmua open message got signal %s" signal)
       (display-buffer (process-buffer process))))))

(defvar mdmua-mailcaps-read nil
  "Signal whether the mailcaps have been read.")

(defun mdmua-message-open-part (message-key part-number)
  "Open the specified PART-NUMBER from the specified MESSAGE-KEY.

Uses the mailcap library to find the viewer for the specified
part and tries to open it with an asynchronous shell command,
piping the part raw straight from md, something like this:

 md rawpart -p 2 INBOX#231237612736 | viewer -

Where the part being viewed is number 2 and the viewer specified
in the mailcaps is 'viewer'.

See 'mailcap-parse-mailcaps' for more information on mailcaps.

In the near future I want this to support in built Emacs viewers
via some mailcap syntax, for example:

  text/xml; firefox %s; emacs-mdmua=emacs nxml-mode %s

which would allow us to prioritize an Emacs specific viewer over
a generic one, maybe even asking the user.

This can be done like this:

 (mailcap-mime-info type \"emacs-mdmua\")

Then people can provide specific support for 'mdmua'."
  (interactive
   (let* ((l mdmua-message--struct)
          (collection
           ;; FIXME
           ;; need to check that we have a viewer for each of these parts
           (loop for e from 1 to (length l)
                 collect (let ((n (- e 1)))
                           (cons (format "%s {%d}" (elt l n) n) n))))
          (msg-key (buffer-name))
          (completion (completing-read  "Which part: " collection)))
     (unless mdmua-mailcaps-read
       (mailcap-parse-mailcaps)
       (setq mdmua-mailcaps-read t))
     (if (not (assoc completion collection))
         (list msg-key completion)
       (save-match-data
         (if (string-match
              "[^ ]+ {\\([0-9]+\\)}" completion)
             (list msg-key (string-to-number (match-string 1 completion))))))))
  (let* ((type (elt mdmua-message--struct part-number))
         (command (mailcap-mime-info type))
         (qkey (format "%s--%s" message-key part-number)) ; the qualified key
         (partbuf (get-buffer-create
                  (format "* mdmua-message-channel-%s *" qkey))))
    (if (not command)
        (error "Mdmua needs a command to read a part %s" type))
    (if (get-buffer qkey)
        (switch-to-buffer qkey)
      ;; FIXME - really I want to allow editing the command
      (with-mdmua-command
        (format "rawpart -p %s %s | %s" ; ensure the command is async
                part-number
                message-key
                (format command "-")) ; 'command' will have %s in it
        partbuf
        ;; sentinel commands
        ((equal signal "finished\n")
         (message
          "mdmua - finished viewing part %s of %s"
          part-number
          message-key))))))


;; Folder funcs

(defun mdmua-mark-regex (regex)
  "Mark a message for some other operation"
  (interactive "Mregex to match message lines: ")
  (save-excursion
    (mdmua-prev-folder)
    (let ((inhibit-read-only 't))
      (while (re-search-forward regex nil 't)
        (add-text-properties
         (point-at-bol)
         (point-at-eol)
         `(marked t
                  face (foreground-color . "green")))))))

(defun mdmua-trash-message (message buffer)
  "Delete the specified MESSAGE in the specified BUFFER.

When called interactively, the message on the current line."
  (interactive
   (list
    (plist-get (text-properties-at (point)) 'key)
    (current-buffer)))
  (let ((work-buf (get-buffer-create "*mdmua-work*")))
    (with-current-buffer buffer
      (with-mdmua-command
        (concat "rm " message)
        work-buf
        ((equal signal "finished\n")
         (message "mdmua: deleted %s" message))))))

(defun mdmua-prev-folder()
  (interactive)
  (beginning-of-line)
  (re-search-backward "^[^ ]" nil 't))

(defun mdmua-next-folder()
  (interactive)
  (end-of-line)
  (re-search-forward "^[^ ]" nil 't))


;; Message lists

(defvar mdmua-on-list-history '()
  "The history for the command with list and regex commands.")

(defvar mdmua-on-regex-history '()
  "The history for the regex with the regex command.")

(defun mdmua-on-list (md-buffer md-command messages)
  "Execute the MD-COMMAND on each of the MESSAGES.

Interactively this operates on the region or gives an error if
there is not active region."
  (interactive
   (if (not (use-region-p))
       (error "mdmua: no active region to find messages")
     (let* ((buffer (current-buffer)))
       (list
        buffer
        (read-from-minibuffer "Command: " nil nil nil 'mdmua-on-list-history)
        ;; Now try and get a list of the messages
        (with-current-buffer buffer
          (save-excursion
            (let ((end (region-end)))
              (goto-char (region-beginning))
              (loop
               until (>= (point) end)
               collect
               (let ((key
                      (plist-get (text-properties-at (point)) 'key)))
                 (forward-line 1)
                 key)))))))))
  (let ((workbuf (get-buffer-create "*mdmua-work*")))
    (loop for msg in messages
          do (with-mdmua-command
               (format "%s %s" md-command msg) ; command to send
               workbuf ; buffer to use
               ((equal signal "finished\n")
                (message "mdmua: done %s on %s" md-command msg))))))

(defun mdmua-on-regex (md-buffer md-command regex)
  "Execute the MD-COMMAND on any message that matches REGEX."
  (interactive
   (let* ((buffer (current-buffer)))
     (list
      buffer
      (read-from-minibuffer "Command: " nil nil nil 'mdmua-on-list-history)
      (read-from-minibuffer "Regex: " nil nil nil 'mdmua-on-regex-history))))
  (mdmua-on-list
   md-buffer
   md-command
   (with-current-buffer md-buffer
     (save-excursion
       (if (use-region-p)
           (goto-char (region-beginning))
         (goto-char (point-min)))
       (or
        (loop
         while
         (re-search-forward
          regex
          (if (use-region-p)
              (region-end)
            (point-max))
          t)
         collect
         (let ((key
                (plist-get (text-properties-at (point)) 'key)))
           (forward-line 1)
           key))
        (error "mdmua: no messages match '%s'" regex))))))

;;; end of mdmua.el
