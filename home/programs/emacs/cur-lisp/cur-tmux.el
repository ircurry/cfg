;;; cur-tmux.el --- Integrating Emacs with a tmux

;;; Commentary:

;;; Code:

(require 'projectile)

(defvar cur-tmux-session-name
  "emacs"
  "The name of the session Emacs will try to connect to and manipulate.")

(defvar cur-tmux-default-command
  "fish"
  "The default command to run when creating a new window.")

(defun cur-tmux--remove-ansi-escape-colors (str)
  "Remove ansi escape color codes from STR."
  (replace-regexp-in-string "\\[.*?m" "" str))

(defun cur-tmux--list-session-names ()
  "List the names of all active tmux sessions."
  (let* ((session-names (string-split
                        (with-temp-buffer
                          (cur-tmux--exec-command '("ls" "-F" "#{session_name}") t)
                          (buffer-string)) "\n" t)))
    (remove nil session-names)))

(defun cur-tmux-emacs-session-p ()
  "Return t of `cur-tmux-session-name' is an active session."
  (if (member cur-tmux-session-name (cur-tmux--list-session-names))
      t
    nil))

(defun cur-tmux--create-session (&optional command)
  "Create session named after `cur-tmux-session-name' if one does not exist.
Will return nil if session exists and will return t if session is
created successfully.  Will error if tmux is not found or if tmux
returns an error value.
If COMMAND is not nil, it will be passed as the initial shell command to tmux."
  (if (cur-tmux-emacs-session-p)
      nil
    (let* ((exit-code (cond
                       (command (call-process "tmux" nil nil nil
                                              "new" "-d" "-s" cur-tmux-session-name command))
                       (t (call-process "tmux" nil nil nil
                                        "new" "-d" "-s" cur-tmux-session-name)))))
      (if (= exit-code 0)
          t
        (error "Tmux was unable to create a new session with that name")))))

(defun cur-tmux--exec-command-with-code (args &optional buffer dir)
  "Call tmux with ARGS, output to BUFFER and return exit code.
ARGS must be a list strings that corespond to tmux flags and arguments.

If DIR is not nil, set the directory to of the tmux window to DIR."
  (let ((default-directory (if dir
                               (expand-file-name dir)
                             default-directory)))
    (apply 'call-process "tmux" nil buffer nil args)))

(defun cur-tmux--exec-command (args &optional buffer dir)
  "Call `cur-tmux--exec-command' with ARGS, BUFFER, and DIR, return t or nil.
Nil is returned if there is an error and t is returned if there is no error.
ARGS must be a list strings that corespond to tmux flags and arguments."
  (let ((exit-code (cur-tmux--exec-command-with-code args buffer dir)))
    (if (> exit-code 0)
        nil
      t)))

(defun cur-tmux--exec-command-err-on-err (args &optional buffer dir)
  "Call `cur-tmux--exec-command' with ARGS, BUFFER and DIR, `error' on error."
  (let* ((exit-code (cur-tmux--exec-command-with-code args buffer dir)))
    (if (= exit-code 0)
        t
      (error "Tmux terminated with exit code %d" exit-code))))

(defun cur-tmux--new-window (window-name &optional command dir)
  "New window named WINDOW-NAME, executing COMMAND if it is non nil.
If DIR is not nil, command is executed in DIR."
  (let* ((args (list "new-window" "-n" window-name "-t" cur-tmux-session-name))
         (args* (if command
                    (append args (list command))
                  args)))
    (cur-tmux--exec-command-err-on-err args* nil dir)))

(defun cur-tmux--list-windows (&optional index)
  "List windows in `cur-tmux-session-name'.
If INDEX is not nil, output the with format \"#{window_index}: #{window_name}\"."
  (let* ((args (list "list-windows" "-t" cur-tmux-session-name "-F" ))
         (args* (if index
                    (append args '("#{window_index}: #{window_name}"))
                  (append args '("#{window_name}")))))
    (split-string
     (with-temp-buffer
       (cur-tmux--exec-command-err-on-err args* t)
       (buffer-string))
     "\n" t)))

(defun cur-tmux--get-window-number (window)
  "Get the number associated with WINDOW from `cur-tmux-session-name' session."
  (car
   (rassoc
    (list window)
    (let* ((window-list (cur-tmux--list-windows t))
           (window-alist (mapcar (lambda (line)
                                   (string-split line "[ :]+" t))
                                 window-list)))
      ;; (rassoc window window-alist)
      window-alist))))

(defun cur-tmux-window-exists-p (window &optional index)
  "Check if WINDOW is a window in `cur-tmux-session-name'.
If INDEX is not nil, check if there is a window named WINDOW with number INDEX.
The behavior is similar to that of `member'."
  (cond
   (index (if (member (format "%d: %s" index window) (cur-tmux--list-windows index))
              t
            nil))
   (t (if (member window (cur-tmux--list-windows))
          t
        nil))))

(defun cur-tmux--switch-to-window (window)
  "Switch to WINDOW in `cur-tmux-session-name' session.
If WINDOW is a string, look up and select the first window number
named WINDOW
If WINDOW is a number, access the window with that number."
  (cond ((numberp window)
         (cur-tmux--exec-command-err-on-err
          (list "selectw" "-t"
                (concat cur-tmux-session-name (format "%d" window)))))
        ((stringp window)
         (cur-tmux--exec-command-err-on-err
              (list "selectw" "-t"
                    (concat cur-tmux-session-name
                            ":" (cur-tmux--get-window-number project-name)))))
        (t
         (error "WINDOW is not a valid type"))))

(defun cur-tmux-switch-add-project-window ()
  "Switch or add window coresponding to current project.
The name of the window is the value of `projectile-project-name'.
When switching, the first window with that name is chosen."
  (interactive)
  (when (not (cur-tmux-emacs-session-p))
    (cur-tmux--create-session))
  (when (projectile-project-p)
    (let ((project-name (projectile-project-name))
          (project-dir (projectile-project-root)))
      (cond ((cur-tmux-window-exists-p project-name)
             (cur-tmux--switch-to-window project-name))
            (t
             (cur-tmux--new-window project-name cur-tmux-default-command project-dir))))))

(defun cur-tmux-new-window (window command &optional directory)
  "Create a new window called WINDOW and run COMMAND.
If DIRECTORY is not nil, run COMMAND in DIRECTORY."
  (interactive
   (let ((win (read-string "Window Name: " nil nil "scratch"))
         (cmd (read-shell-command "Command: "))
         (dir (read-directory-name "Directory: " nil default-directory)))
     (list win cmd dir)))
  (cur-tmux--create-session)
  (if (not (equal command ""))
      (cur-tmux--new-window window command directory)
    (cur-tmux--new-window window cur-tmux-default-command directory)))

(provide 'cur-tmux)
;;; cur-tmux.el ends here
