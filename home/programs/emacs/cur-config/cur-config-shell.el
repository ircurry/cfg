;; ===Vterm===
(use-package vterm
  :ensure nil
  :commands (vterm cur/meow-vterm-other-window cur/meow-vterm)
  :bind (:map vterm-mode-map
         ("C-c C-k" . cur/vterm-kill)
         :map cur/sub-leader-keymap
         ("C-RET" . cur/meow-vterm-other-window)
         ("RET"   . cur/meow-vterm))
  :init
  (defun cur/meow-vterm ()
    (interactive)
    (vterm)
    (meow-insert-mode))
  (defun cur/meow-vterm-other-window ()
    (interactive)
    (vterm-other-window)
    (meow-insert-mode))
  (defun cur/vterm-kill ()
    (interactive)
    (when (equal (buffer-name) "*vterm*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer-and-window))))
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

;; ===Eat===
(use-package eat
  :config
  (eat-eshell-mode 1))

;; ===Eshell===
(use-package eshell
  :init
  (defun cur/eshell-prompt ()
    (concat
     (propertize "[" 'face 'ansi-color-red)
     (propertize (eshell/whoami) 'face 'ansi-color-yellow)
     (propertize "@" 'face 'ansi-color-green)
     (propertize (system-name) 'face 'ansi-color-blue)
     " "
     (propertize (concat (eshell/pwd)) 'face 'ansi-color-magenta)
     (propertize "]" 'face 'ansi-color-red)
     (propertize "$ " 'face 'bold)))
  (defun eshell/ff (&optional file)
    "Eshell alias to open FILE. Will call `find-file' interactively if no file is
specified."
    (cond (file
           (find-file file))
          (t
           (call-interactively 'find-file))))
  (defun eshell/dir (&optional dir)
    "Eshell alias to open `dired' at DIR. Will call `dired' on current directory
if no directory is specified"
    (cond (dir
           (dired dir))
          (t
           (dired "."))))
  :custom
  (eshell-prompt-regexp "^\\[[^]]*\\]\\[?[[:digit:]]*\\]?[#$λ] " "Regex for custom eshell prompt")
  (eshell-prompt-function 'cur/eshell-prompt "Set custom prompt for eshell"))

;; ===Zoxide===
(use-package zoxide)

(provide 'cur-config-shell)
