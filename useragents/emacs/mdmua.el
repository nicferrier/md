;; mdmua
;; maildir mail user agent
;; Nic Ferrier, March 2009

;; A simple emacs user agent for md, the stateless maildir user agent.

(require 'json)

(defvar mdmua-buffer '()
  "The buffer where everything is rendered.")

(defvar mdmua-mode-map '())

(defvar mdmua-md-bin-path "md"
  "Where md can be found.")


(defcustom mdmua-maildir "~/.maildirs/nic"
  "Where the maildir can be found."
  :group 'md)

;; Utility functions

(defun mdmua-util-strip (str)
  "Standard line break removal"
  (or (and (string-match "\\(.*\\)\n" str) 
	   (match-string 1 str))
      str))

(defun mdmua-util-buffer-lines ()
  "A simple tool to return a list of all the lines in a buffer."
  (let ((lst '()))
    (save-excursion
      (beginning-of-buffer)
      (let ((pos (point)))
	(while (not (eobp))
	  (forward-visible-line 1)
	  (setq lst (append lst 
			    (list 
			     (mdmua-util-strip
			      (buffer-substring-no-properties pos (point))))))
	  (setq pos (point)))
	))
    lst))

(defun mdmua-string-to-plist-symbol (symbol)
  "Convert a string to a plist symbol"
  (car (read-from-string (concat ":" symbol))))

(defun mdmua-alist-to-plist (alist)
  "Convert an alist to a plist"
  (let ((plist '()))
    (dolist (el alist plist)
      (setq plist 
	    (append plist 
		    (list (mdmua-string-to-plist-symbol (symbol-name (car el)))
			  (cdr el)))))))

(defun mdmua--command (command &optional buffer)
  "Run the specified command with mdmua and using the channel as a buffer unless buffer is not nil"
  (let* ((buf (if buffer buffer (get-buffer-create "*mdmua-channel*" )))
         (proc 
          (start-process-shell-command 
           "mdmua" 
           buf
           (format "%s -M %s %s" 
                   mdmua-md-bin-path 
                   (expand-file-name mdmua-maildir) 
                   command))))
    proc))


;; Main program

(defun mdmua ()
  (interactive)
  (setq mdmua-buffer (get-buffer-create "mdmua"))
  (switch-to-buffer mdmua-buffer)
  ;; Got to define a mode-map
  (make-local-variable 'mdmua-folders)
  (setq buffer-read-only 't)
  (setq mdmua-folders '("INBOX"))
  (unless mdmua-mode-map
    (progn
      (setq mdmua-mode-map (make-sparse-keymap))
      (define-key mdmua-mode-map "\r" 'mdmua-open)
      (define-key mdmua-mode-map ">" 'mdmua-next-folder)
      (define-key mdmua-mode-map "<" 'mdmua-prev-folder)
      (define-key mdmua-mode-map "d" 'mdmua-trash-message))
    )
  (use-local-map mdmua-mode-map)
  (mdmua-folder-list)
  )

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


;; Message funcs
(require 'qp)
(defun mdmua-message-display (details)
  "Actually displays a message part.

details is a props list
:text being the text
:key being the key"
  (switch-to-buffer (get-buffer-create (plist-get details :key)))
  (buffer-disable-undo)
  (if (not (plist-get details :no-render))
      (let ((content (plist-get details :text)))
        (auto-fill-mode 1)
        (insert 
         (quoted-printable-decode-string
          (replace-regexp-in-string "\r" "" content)))    
        (message-mode)
        (local-set-key "\C-ca" 'message-reply)
        (message-sort-headers)))
  (beginning-of-buffer)
  (setq buffer-read-only 't)
  (set-buffer-modified-p nil))

(defun mdmua--sentinel-gettext (process signal)
  (cond
   ((equal signal "finished\n")
    (mdmua-message-display 
     (with-current-buffer (process-buffer process)
       (plist-put 
	struct
	:text (buffer-substring (point-min) (point-max)))))
    (kill-buffer (process-buffer process))
    )
   ;; else
   ('t
    (message "mdmua open message got signal %s" signal)
    (display-buffer (process-buffer process))
   )
  ))

(defun mdmua-open-message (key &optional no-render)
  """Open the message with key.

Specify the prefix arg to not-render the message. This can be
useful while we're developing mdmua"""
  (interactive (list 
		(plist-get (text-properties-at (point)) 'key)
		current-prefix-arg))
  (let* ((buf (get-buffer-create "mdmua-message-channel"))
         (proc (mdmua--command (format "text %s" key) buf)))
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'struct)
      (setq struct
            `(:key ,key :no-render ,no-render))
      )
    (set-process-sentinel proc 'mdmua--sentinel-gettext)
    ))

(defun mdmua-open-full (key)
  "Open the whole file of the message"
  (interactive (list 
		(plist-get (text-properties-at (point)) 'key)))
  (let* ((buf (get-buffer-create "mdmua-message-channel"))
         (proc (mdmua--command (format "file %s" key) buf)))
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'struct)
      (setq struct
            `(:key ,key :no-render ,nil))
      )
    (set-process-sentinel proc 'mdmua--sentinel-gettext)
    )
  )

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
      (mdmua-render (mdmua-folders-list mdmua-folders))
      (let ((pos (text-property-any 
		  (point-min)
		  (point-max)
		  'key 
		  (plist-get message :key))))
	(goto-char pos)
	(next-line))
      )
    (kill-buffer (process-buffer proc))
    )))

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
                                             :folder-name ,folder)))
          )))))

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
    (set-process-sentinel proc 'mdmua-sentinel-trash)
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'trash-info)
      (seq trash-info `(:folder-buffer ,buf
				       :message-key ,message 
				       :folder-name ,folder)))
    ))

(defun mdmua-trash-message (message folder)
  "Delete the specified messages

When called interactively the message on the current line."
  (interactive (list 
		(plist-get (text-properties-at (point)) 'key)
		(plist-get (text-properties-at (point)) 'folder)))
  (let* ((buf (get-buffer-create "mdmua-message-channel"))
         (proc (mdmua--command 
                (format "lisp -r %s" (if (equal folder "INBOX") "" folder))
                buf
                )))
    (set-process-sentinel proc 'mdmua-sentinel-trash)
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'folder-buffer)
      (make-local-variable 'folder-name)
      (make-local-variable 'message-key)
      (setq folder-buffer buf)
      (setq folder-name folder)
      (setq message-key message))
    ))

  
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
Called repeatedly by mdmua-render for all messages in open folders"
  (propertize
   (format "% 22s  %30s  %s\n"
	   (or (plist-get message :date)

	       "0-00-00")
	   (or (elt (plist-get message :from) 0)
	       (elt (plist-get message :from) 1)
	       "nobody@nowhere")
	   (or (plist-get message :subject)
	       " ")
	   )
   'key (plist-get message :key)
   'folder (plist-get message :folder)
   ;; This should be done with syntax instead of directly
   'face (cond
	  ((member ?T (string-to-list (plist-get message :flags)))
	   '(foreground-color . "Red"))
	  ((member ?S (string-to-list (plist-get message :flags)))
	   '(foreground-color . "Black"))
	  ('t
	   '(foreground-color . "Blue"))))
   )

(defun mdmua-render (folders &optional process)
  "mdmua's render function
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
key.
"
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
    (if process
	(kill-buffer (process-buffer process))))

(defun mdmua--sentinel-folders (process signal)
  (cond
   ((equal signal "finished\n")
    ;; we need to read the lines in the buffer and put them into the 
    ;; user's display buffer's folder list
    ;; ... and then repaint
    (let ((folders 
           (with-current-buffer (process-buffer process)
             (mdmua-util-buffer-lines))))
      (setq folders (cons "INBOX" folders))
      (mdmua-render folders process))
    ))
  )

(defun mdmua-folders-list (folder-struct)
  "Return just the folders from the folder struct"
  (mapcar (lambda (v) (car v)) folder-struct))

(defun mdmua-folder-list ()
  "List the messages in a folder."
  (interactive)
  (let ((buf (get-buffer-create "*mdmua-folders*"))
        (proc 
         (start-process-shell-command 
          "mdmua" 
          "*mdmua-folders*"
          (format "%s -M %s lsfolders"  mdmua-md-bin-path (expand-file-name mdmua-maildir)))))
    (set-process-sentinel proc 'mdmua--sentinel-folders)
    ))

(defun mdmua-close-folder (folder-name)
  (interactive (list (get-text-property (point) 'folder-name)))
  (let ((folder-obj (assoc folder-name mdmua-folders)))
    (plist-put (cdr folder-obj) :open nil)
    (mdmua-render (mapcar (lambda (x) (car x)) mdmua-folders))))

(defun mdmua-open-folder (folder-name)
  "Open the folder.
When called interactively this expects to be located on a line with the folder on it."
  (interactive (list (get-text-property (point) 'folder-name)))
  (message "mdmua-open-folder: %s" folder-name)
  (let ((folder-obj (assoc folder-name mdmua-folders)))
    (if (plist-get (cdr folder-obj) :open)
	(call-interactively 'mdmua-close-folder)
      (mdmua-list folder-name))))

(defun mdmua--sentinel-list (process signal)
  (cond
   ((equal signal "finished\n")
    ;; We need to read the lines in the buffer and put them into the 
    ;; user's display buffer's in a sort of repaint
    (let ((message-lines 
	   (with-current-buffer (process-buffer process)
	     (mdmua-util-buffer-lines)))
	  (folder-obj (assoc 
		       (with-current-buffer (process-buffer process)
			 folder-name)
		       mdmua-folders)))
      ;; Change the folders message list and set the open flag to true
      (plist-put (cdr folder-obj)
		 :messages
		 (mapcar (lambda (line)
			   (condition-case nil
                               (let ((json (json-read-from-string line)))
                                 (mdmua-alist-to-plist json))
			     (error nil)
			     ))
			 message-lines))
      (plist-put (cdr folder-obj) :open 't)
      (mdmua-render (mapcar (lambda (x) (car x)) mdmua-folders) process)
    ))))

(defun mdmua-list (folder)
  "List the messages in a folder."
  (interactive)
  (let ((proc (mdmua--command (format "lisp -r %s" (if (equal folder "INBOX") "" folder)))))
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'folder-name)
      (setq folder-name folder))
    (set-process-sentinel proc 'mdmua--sentinel-list)
    ))


;; End
