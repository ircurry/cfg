;; ===Org-Mode===
(use-package org
  :ensure nil
  :defer 3
  :hook (org-mode . cur/org-mode-setup)
  :bind ( :map org-mode-map
          ("C-S-h" . outline-promote)
          ("C-S-j" . outline-move-subtree-down)
          ("C-S-k" . outline-move-subtree-up)
          ("C-S-l" . outline-demote))
  :init
  (defun cur/org-mode-setup ()
    (org-indent-mode 1)
    (variable-pitch-mode 0)
    (visual-line-mode 1)
    (flyspell-mode 1))
  :custom
  (org-ellipsis " ▾" "Readable ellipsis")
  (org-adapt-indentation nil)
  (org-special-ctrl-a/e nil)
  (org-M-RET-may-split-line '((default . nil)))
  (org-hide-emphasis-markers nil)
  (org-hide-macro-markers nil)
  (org-hide-leading-stars nil)
  (org-agenda-start-with-log-mode t)
  (org-src-window-setup 'plain) ; don't override `display-buffer-alist'
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-window-setup 'current-window "Have org-agenda pop up in the current window")
  (org-imenu-depth 4)
  (org-edit-src-content-indentation 0)
  :config
  (load-library "find-lisp")
  ;; (setq org-agenda-files (find-lisp-find-files "~/dox/agenda" "\.org$"))
  (setq org-agenda-time-grid '((daily today require-timed)
                               (400 600 800 1000 1200 1400 1600 1800 2000 2200)
                               "......" "----------------"))
  (setq org-format-latex-options '(:foreground "#e5e9e9" :scale 1.0)))
;; (cur/org-font-setup))

;; ===Org Tempo and SRC Blocks===
(use-package org-tempo
  :after org
  :demand t
  :custom
  (org-structure-template-alist
   '(("s" . "src")
     ("e" . "src emacs-lisp")
     ("t" . "src emacs-lisp :tangle FILENAME")
     ("T" . "src text :tangle FILENAME")
     ("P" . "src text :tangle ./packages.txt :padline no")
     ("x" . "export")
     ("X" . "example")
     ("q" . "quote")
     ("v" . "verse"))))

;; ===Org Babel===
(use-package ob
  :after org)
;; TODO: fix this so I can still auto tangle on saving
;; :init
;; (defun cur/org-babel-tangle-config ()
;;   (when (string-equal (file-name-directory (buffer-file-name))
;;                       (expand-file-name user-emacs-directory))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))
;; 
;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cur/org-babel-tangle-config))))

(use-package flyspell
  :demand t
  :bind ( :map flyspell-mode-map
          ("C-." . nil)
          ("C-," . nil)))

(provide 'cur-config-org)
