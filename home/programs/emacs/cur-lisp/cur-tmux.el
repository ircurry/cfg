;;; cur-tmux.el --- Integrating Emacs with a tmux

;;; Commentary:

;;; Code:

(defvar cur-tmux-list-sessions-command
  "tmux ls"
  "Command used to list out current sessions in multiplexer.")

(defvar cur-tmux-emacs-session-name
  "emacs"
  "The name of the session Emacs will try to connect to and manipulate.")

(defun cur-tmux--remove-ansi-escape-colors (str)
  "Remove ansi escape color codes from STR."
  (replace-regexp-in-string "\\[.*?m" "" str))

(defun cur-tmux--list-session-names ()
  "List the names of all active tmux sessions."
  (let* ((session-list (string-split
                        (shell-command-to-string cur-tmux-list-sessions-command) "\n"))
         (session-names (mapcar (lambda (session-value)
                                  (car (string-split session-value ":")))
                                session-list)))
    (remove "" session-names)))

(defun cur-tmux-emacs-session-p ()
  "Return t of `cur-tmux-emacs-session-name' is an active session."
  (if (member cur-tmux-emacs-session-name (cur-tmux--list-session-names))
      t
    nil))

(provide 'cur-zellij)
;;; cur-tmux.el ends here
