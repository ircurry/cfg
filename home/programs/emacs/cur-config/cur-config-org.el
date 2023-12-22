;;; cur-config-org.el --- configurations for org-mode

;;; Commentary:

;;; Code:
;; ===Org-Mode===
(use-package org
  :ensure nil
  :defer t
  :hook (org-mode . cur/org-mode-setup)
  :init
  (defun cur/org-mode-setup ()
    (org-indent-mode 1)
    (variable-pitch-mode 0)
    (visual-line-mode 1))
  (defun cur/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))
  :custom
  (org-ellipsis " ▾" "Readable ellipsis")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-window-setup 'current-window "Have org-agenda pop up in the current window")
  :config
  (load-library "find-lisp")
  ;; (setq org-agenda-files (find-lisp-find-files "~/dox/agenda" "\.org$"))
  (setq org-agenda-time-grid '((daily today require-timed)
                               (400 600 800 1000 1200 1400 1600 1800 2000 2200)
                               "......" "----------------"))
  (setq org-format-latex-options '(:foreground "#e5e9e9" :scale 3.0))
  (cur/org-font-setup))

;; ===Org Bullets===
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("" "●" "○" "●" "○" "●" "○" "●" "○")))
;;  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; ===Org Tempo and SRC Blocks===
(use-package org-tempo
  :after org
  :config
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("tex" . "src latex"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("java" . "src java"))
  (add-to-list 'org-structure-template-alist '("elv" . "src elvish")))

;; ===Org Babel===
(use-package ob
  :after org)
  ;; :init
  ;; (defun cur/org-babel-tangle-config ()
  ;;   (when (string-equal (file-name-directory (buffer-file-name))
  ;;                       (expand-file-name user-emacs-directory))
  ;;     ;; Dynamic scoping to the rescue
  ;;     (let ((org-confirm-babel-evaluate nil))
  ;;       (org-babel-tangle))))
  ;; 
  ;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cur/org-babel-tangle-config))))

(provide 'cur-config-org)
;;; cur-config-org.el ends here
