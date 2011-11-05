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
      (beginning-of-buffer)
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

(defun mdmua--string-to-plist-symbol (symbol)
  "Convert a string to a plist symbol"
  (car (read-from-string (concat ":" symbol))))

(defun mdmua--alist-to-plist (alist)
  "Convert an alist to a plist"
  (let ((plist '()))
    (dolist (el alist plist)
      (setq plist
            (append plist
                    (list (mdmua--string-to-plist-symbol (symbol-name (car el)))
                          (cdr el)))))))

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

(defun mdmua ()
  (interactive)
  (setq mdmua-buffer (get-buffer-create "mdmua"))
  (switch-to-buffer mdmua-buffer)
  (make-local-variable 'mdmua-folders)
  (setq buffer-read-only 't)
  (setq mdmua-folders '("INBOX"))
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
      (define-key mdmua-mode-map "p" 'mdmua-pull))
    )
  (use-local-map mdmua-mode-map)
  (mdmua-folder-list))

(defun mdmua-open ()
  (interactive)
  ;; Test to see if we're opening a folder or not
  (if (save-excursion 
	(beginning-of-line)
	(re-search-forward "^[^ ]" (line-end-position) 't))
      (call-interactively 'mdmua-open-folder)
    (call-interactively 'mdmua-open-message)))

;; Mandling key'd messages

(defun mdmua-pastebuffer-key (key)
  """Puts the key in the pastebuffer so you can find the message with other means"""
  (interactive (list 
		(plist-get (text-properties-at (point)) 'key)))
  (with-temp-buffer 
    (insert key)
    (clipboard-kill-ring-save (point-min) (point-max))))

(defun mdmua-message-list-get (folder-buffer folder-name key)
  "Retrieve the message object specified by folder-name and key."
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
      (progn
        (with-current-buffer "mdmua"
          (setq mdmua-pulling "running")
          (setq mdmua-pull-display-string " mdmua:running")
          )
        (with-mdmua-shell-command "mdpullall" (get-buffer-create "* mdmua-pull *")
          ((equal signal "finished\n")
           ;; We can now refresh the data
           (with-current-buffer "mdmua"
             (setq mdmua-pulling "done")
             (setq mdmua-pull-display-string " mdmua:done")
             )
           (kill-buffer (process-buffer process))
           ))
        )
    )
  )


(defvar mdmua-pull-display-string " mdmua:done"
  ;; TODO
  ;; WHY does this need a space when it is added to the global-mode-string list???
  "The string to show whether the mdmua is being pulled or not")

(defun mdmua-pull-display ()
  "Set the mode line to display when we are pulling"
  (interactive)
  (setq global-mode-string (append global-mode-string (list 'mdmua-pull-display-string)))
  )

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
    (set-buffer-modified-p nil)
    ))

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
  """Open the message with key.

Specify the prefix arg to not-render the message. This can be
useful while we're developing mdmua"""
  (interactive (list 
		(plist-get (text-properties-at (point)) 'key)
		current-prefix-arg))
  (let ((msgbuf (get-buffer-create (format "* mdmua-message-channel-%s *" key)))
        (structbuf (get-buffer-create (format "* mdmua-message-struct-channel-%s *" key))))
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
                              (beginning-of-buffer)
                              (json-read))))
              (kill-buffer structbuf)
              (with-current-buffer msgbuf
                (make-variable-buffer-local 'mdmua-message--struct)
                (setq mdmua-message--struct partlist))
              (switch-to-buffer msgbuf)
              (beginning-of-buffer))
            ))
         )
        ;; else
        ('t
         (message "mdmua open message got signal %s" signal)
         (display-buffer msgbuf))))))

(defun mdmua-open-full (key)
  "Open the whole file of the message"
  (interactive (list 
		(plist-get (text-properties-at (point)) 'key)))
  (let ((buf (get-buffer-create (format "* mdmua-message-channel-%s *" key))))
    (with-mdmua-command (format "file %s" key) buf
      ;; The sentinel code
      ((equal signal "finished\n")
       (switch-to-buffer (process-buffer process))
       (rename-buffer key)
       (mdmua-message-mode)
       (beginning-of-buffer))
      ;; else
      ('t
       (message "mdmua open message got signal %s" signal)
       (display-buffer (process-buffer process))))))

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
     (if (not (assoc completion collection))
         (list msg-key completion)
       (save-match-data 
         (if (string-match 
              "[^ ]+ {\\([0-9]+\\)}" completion)
             (list msg-key (string-to-int (match-string 1 completion))))))))
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
         (message "mdmua - finished viewing part %s of %s" part-number message-key))))))


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


(defun mdmua--sentinel-trash (proc signal)
  (cond
   ((equal signal "finished\n")
    ;; Get the message from the folder buffer's message store
    (let ((message 
	   (with-current-buffer (process-buffer proc)
	     (mdmua-message-list-get folder-buffer 
				     folder-name 
				     message-key))))
      ;; Now change the message stored there
      (plist-put message :flags (concat "T" (plist-get message :flags)))
      ;; And now update the text
      (mdmua--render (mdmua-folders-list mdmua-folders))
      (let ((pos (text-property-any 
		  (point-min)
		  (point-max)
		  'key 
		  (plist-get message :key))))
	(goto-char pos)
	(next-line))
      )
    (kill-buffer (process-buffer proc)))))

(defun mdmua-trash-marked ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((pos (next-single-property-change (point) 'marked))
	  (marked '()))
      (while pos
	(setq marked 
	      (append marked (list  
			      (cons
			       (plist-get (text-properties-at pos) 'folder)
			       (plist-get (text-properties-at pos) 'key)))))
	(setq pos (next-single-property-change pos 'marked)))
      ;; need to transform (folder-name . message-key) into message-key
      (let ((keys 
             (mapconcat 
              (lambda (x)(cdr x)) marked " ")))
        (let ((buf (current-buffer))
              (proc 
               (start-process-shell-command 
                ;; TODO:::
                ;; rewrite md so it can take message lists on stdin
                ;; then send trashed messages on stdin
                "mdmua" "mdmua-channel" mdmua-md-bin-path "trash" message)))
          (set-process-sentinel proc 'mdmua--sentinel-trash)
          (with-current-buffer (process-buffer proc)
            (make-local-variable 'trash-info)
            (seq trash-info `(:folder-buffer ,buf
                                             :message-key ,message 
                                             :folder-name ,folder))))))))

(defun mdmua-trash-message-x (message folder)
  "Delete the specified messages

bWhen called interactively the message on the current line."
  (interactive (list 
		(plist-get (text-properties-at (point)) 'key)
		(plist-get (text-properties-at (point)) 'folder)))
  (let ((buf (current-buffer))
	(proc 
	 (start-process-shell-command 
	  "mdmua" "mdmua-channel" mdmua-md-bin-path "trash" message)))
    (set-process-sentinel proc 'mdmua--sentinel-trash)
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'trash-info)
      (setq trash-info `(:folder-buffer ,buf
				       :message-key ,message 
				       :folder-name ,folder)))))
  
(defun mdmua-prev-folder()
  (interactive)
  (beginning-of-line)
  (re-search-backward "^[^ ]" nil 't))

(defun mdmua-next-folder()
  (interactive)
  (end-of-line)
  (re-search-forward "^[^ ]" nil 't))


;; Message lists

(defun mdmua-message-render (message)
  "Render a message in a list.
Called repeatedly by mdmua--render for all messages in open folders"
  (propertize
   (format "% 22s  %30s  %s\n"
	   (or (plist-get message :date)

	       "0-00-00")
	   (or (elt (plist-get message :from) 0)
	       (elt (plist-get message :from) 1)
	       "nobody@nowhere")
	   (or (plist-get message :subject)
	       " "))
   'key (plist-get message :key)
   'folder (plist-get message :folder)
   ;; This should be done with syntax instead of directly
   'face (cond
	  ((member ?T (string-to-list (plist-get message :flags)))
	   '(foreground-color . "Red"))
	  ((member ?S (string-to-list (plist-get message :flags)))
	   '(foreground-color . "Black"))
	  ('t
	   '(foreground-color . "Blue")))))

(defun mdmua--render (folders)
  "Render the state of the folders.
This takes the state of mdmua from the list of folders and causes
it to be rendered to the buffer

The list of folders maintained by mdmua is called:

  mdmua-folders

and is buffer local for every mdmua buffer.

This list is the model of the mailstore. If we can make multiple
mdmua-foders we can have multiple mailstores.

The structure is:

  association-list of:
    folder-names -> property lists
      each property-list
       :messages -> property list of message
         each message
           :subject
           :date
           :key
           :flags

So to retrieve the 3rd message from the INBOX:

 (elt (plist-get 
          (cdr (assoc \"INBOX\" mdmua-folders)) 
          :messages)
      3)

Obviously with this structure it isn't easy to find a particular
key."
  ;; This is quite a naive folder render
  ;; ... it takes no account of existing display
  (condition-case nil
      (let ((inhibit-read-only 't))
	(save-excursion
	  (delete-region (point-min) (point-max))
	  (setq mdmua-folders
		(mapcar 
		 (lambda (folder-name)
		   ;; Render a folder
		   (insert (propertize
			    (concat folder-name "\n")
			    'folder-name folder-name))
		   ;; Rebuild the assoc item for the folder
		   (let ((folder-obj (assoc folder-name mdmua-folders)))
		     (if (plist-get (cdr folder-obj) :open)
			 (progn
			   (mapc (lambda (msg)
                                   (if (plist-get msg :key)
                                       (insert (mdmua-message-render msg))))
				 (plist-get (cdr folder-obj) :messages))
			   `(,folder-name 
			     . (:open 't :messages ,(plist-get (cdr folder-obj) :messages))))
		       ;; Else
		       `(,folder-name . (:open nil :messages '()))))
		   )
		 folders))))
    (error nil))
  )


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
        (read-from-minibuffer "Command: ")
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
      (read-from-minibuffer "Command: ")
      (read-from-minibuffer "Regex: "))))
  (mdmua-on-list
   md-buffer
   md-command
   (with-current-buffer md-buffer
     (save-excursion
       (goto-char (region-beginning))
       (or
        (loop
         while (re-search-forward
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

(defun mdmua-folders-list (folder-struct)
  "Return just the folders from the folder struct"
  (mapcar (lambda (v) (car v)) folder-struct))

(defun mdmua-folder-list ()
  "List the messages in a folder."
  (interactive)
  (let ((folderbuf (get-buffer-create "* mdmua-folders *")))
    (with-mdmua-command "lsfolders" folderbuf
      ((equal signal "finished\n")
       ;; we need to read the lines in the buffer and put them into the 
       ;; user's display buffer's folder list
       ;; ... and then repaint
       (let ((folders (mdmua-util--buffer-lines)))
         (setq folders (cons "INBOX" folders))
         (with-current-buffer "mdmua"
           (mdmua--render folders)
           (kill-buffer folderbuf)))
       )
      ('t
       (message "mdmua: something went wrong listing folders?")))))

(defun mdmua-close-folder (folder-name)
  (interactive (list (get-text-property (point) 'folder-name)))
  (let ((folder-obj (assoc folder-name mdmua-folders)))
    (plist-put (cdr folder-obj) :open nil)
    (mdmua--render (mapcar (lambda (x) (car x)) mdmua-folders))))

(defun mdmua-open-folder (folder-name)
  "Open the folder FOLDER-NAME.

When called interactively this expects to be located on a line
with the folder on it."
  (interactive (list (get-text-property (point) 'folder-name)))
  (let ((folder-obj (assoc folder-name mdmua-folders)))
    (if (plist-get (cdr folder-obj) :open)
	(call-interactively 'mdmua-close-folder)
      (mdmua-list folder-name))))

(defun mdmua-list (folder)
  "List the messages in a folder."
  (interactive)
  (let* ((folderref (if (equal folder "INBOX") "" folder))
         (lstcommand (format "lisp -r %s" folderref))
         (lstbuf (get-buffer-create (format "* mdmua-list-%s *" folder))))
    (with-mdmua-command lstcommand lstbuf
      ((equal signal "finished\n")
       ;; We need to read the lines in the buffer and put them into the 
       ;; user's display buffer's in a sort of repaint
       (let ((msg-lines (mdmua-util--buffer-lines)))
         (with-current-buffer "mdmua"
           (let* ((folder-list (copy-sequence mdmua-folders))
                  (folder-obj (assoc folder folder-list)))
             ;; Change the folders message list and set the open flag to true
             (plist-put (cdr folder-obj)
                        :messages
                        (mapcar (lambda (line)
                                  (condition-case nil
                                      (let ((json (json-read-from-string line)))
                                        (mdmua--alist-to-plist json))
                                    (error nil)))
                                msg-lines))
             (plist-put (cdr folder-obj) :open 't)
             (mdmua--render (mdmua-folders-list folder-list)))
           (kill-buffer lstbuf)))))))

;;; End
