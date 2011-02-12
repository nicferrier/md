

(defvar mdel-mode-map '())

(defvar mdel-md-bin-path "md"
  "Where md can be found.")

(defcustom mdel-maildir "~/.maildirs/nic"
  "Where the maildir can be found."
  :group 'md)

(defun mdel--command (command &optional buffer)
  "Run a command with the specified buffer and do sensible interpolation."
  (let* ((buf (if buffer buffer (get-buffer-create "*mdel-channel*")))
         (proc
          (start-process-shell-command
           "mdel"
           buf
           (format "%s -M %s %s"
                   mdel-md-bin-path
                   (expand-file-name mdel-maildir)
                   command))))
    proc))


(defun mdel--render-json (json)
  "Pitch the json into the current buffer"
  (insert (format "% 22s  %30s  %s\n"
                  (or (cdr (assoc 'date json)) "0-00-00")
                  (or (elt (cdr (assoc 'from json)) 0)
                      (elt (cdr (assoc 'from json)) 1) "nobody@nowhere")
                  (or (cdr (assoc 'subject json)) " ")
                  )))

(defvar mdel-filter-data nil
  "This is a temporary variable used to assist in the parsing of the json.")

(defun mdel--filter (proc data)
  (with-current-buffer (process-buffer proc)
    (let ((lines (split-string data "\n")))
      (dolist (line lines)
        (let ((str (if mdel-filter-data
                       (concat mdel-filter-data line)
                     line)))
          (condition-case e
              (if (mdel--render-json (json-read-from-string str))
                  (setq mdel-filter-data nil))
            (error (setq mdel-filter-data str))))))))

(defun mdel--sentinel-list (process signal)
  (message signal)
  )

(defun mdel-list (folder)
  (interactive "Mfolder:")
  (let* ((folder-name (if (equal folder "INBOX") "" folder))
         (proc (mdel--command 
                (format "lisp -r %s" folder-name)
                (format "*mdel-folder-%s*" folder))))
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'mdel-folder-name)
      (setq mdel-folder-name folder-name)
      ;; This is private to the filter function
      (make-local-variable 'mdel-filter-data)
      (setq mdel-filter-data nil)
      (save-excursion 
        (delete-region (point-min) (point-max))))
    (set-process-sentinel proc 'mdel--sentinel-list)
    (set-process-filter proc 'mdel--filter)
    ))

;; End
