;; ===Hydra===
(use-package hydra
  :demand t)

;; ===Meow Setup===
(use-package meow
  :after (hydra)
  :demand t
  :preface
  (defvar cur/sub-leader-keymap
    (let ((map (make-sparse-keymap)))
      map)
    "The keymap for quick command/function execution.")
  (defvar cur/toggle-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for commands that change settings from the leader key.")
  (defvar cur/alignment-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for commands that change alignment and cursor display.")
  (defvar cur/register-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for commands that use registers.")
  (defun cur/kmacro-toggle ()
    (interactive)
    (cond
     (defining-kbd-macro
      (call-interactively #'meow-end-kmacro))
     (t
      (call-interactively #'meow-start-kmacro))))
  (defhydra cur/window (:hint nil)
    "
^Movement^            ^Splitting^               ^Manipulation
^--------^------------^---------^---------------^--------------
_h_: left             _1_: only window          _H_: swap left
_j_: down             _2_: split vertical       _J_: swap down
_k_: up               _3_: split horizontal     _K_: swap up
_l_: right            _0_: close window         _L_: swap right
_o_: other window
"
    ("RET" ignore "finished" :exit t)
    ("w" other-window "other window" :exit t)
    ("." delete-other-windows "only window" :exit t)
    ("f" find-file-other-window "file in new window" :exit t)
    ("v" split-window-right "split vertical" :exit t)
    (";" split-window-below "split horizontal" :exit t)
    ("c" delete-window "delete window" :exit t)

    ("o" other-window)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("1" delete-other-windows)
    ("2" split-window-right)
    ("3" split-window-below)
    ("0" delete-window)

    ("H" windmove-swap-states-left)
    ("J" windmove-swap-states-down)
    ("K" windmove-swap-states-up)
    ("L" windmove-swap-states-right))
  (defhydra cur/goto (:hint nil)
    "
_g_:  goto line         _a_:  beginning of line     _,_: buffer start     _n_: next buffer hydra
_s_: to indentation     _e_:  end of line           _._: buffer end       _p_: prev buffer hydra
"
    ("g" meow-goto-line :exit t)
    ("a" move-beginning-of-line :exit t)
    ("e" move-end-of-line :exit t)
    ("m" back-to-indentation :exit t)
    ("s" back-to-indentation :exit t)
    ("," beginning-of-buffer :exit t)
    ("." end-of-buffer :exit t)
    ("n" cur/hydra-buffer-cycle/next-buffer :exit t)
    ("p" cur/hydra-buffer-cycle/previous-buffer :exit t))
  (defun meow-setup ()
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; ===Top Row===
     '("1" . delete-other-windows)
     '("2" . split-window-right)
     '("3" . split-window-below)
     ;'("4" . )
     ;'("5" . )
     ;'("6" . )
     ;'("7" . )
     ;'("8" . )
     ;'("9" . )
     '("0" . "C-x 0") ; delete window

     ;; ===2nd Row===
     ;'("TAB"  . )
     '("q" . meow-comment) ; comment dwim
     '("w" . cur/window/body)
     ;'("e" . )
     (cons "r" cur/register-map)
     (cons "t" cur/toggle-map)
     ;'("y" . )
     '("u" . "C-u")   ; universal argument
     ;'("i" . )
     '("o" . "C-x o") ; other window
     (cons "p" project-prefix-map)
     ;'("-" . )

     ;; ===3rd Row===
     ;'("<escape>" . )
     ;'("a" . )
     '("s" . "C-x C-s") ; save buffer
     '("d" . kill-current-buffer)
     '("f" . "C-x C-f") ; find file
     ;'("g" . ) ; C-M- map
     ;'("h" . ) ; C-h map
     ;'("j" . )
     ;'("k" . )
     ;'("l" . )
     (cons ";" cur/sub-leader-keymap)
     '("RET"   . "M-x")

     ;; ===4th Row===
     (cons "z" cur/alignment-map)
     ;'("x" . ) ; C-x map
     ;'("c" . ) ; C-c map
     ;'("v" . )
     '("b" . "C-x b")
     ;'("n" . )
     ;'("m" . ) ; M- map
     '("," . meow-beginning-of-thing)
     '("." . meow-end-of-thing)
     '("/" . rg)
     ;'("'"  . )
     )
    (meow-normal-define-key
     ;; ===Top Row===
     '("1" . meow-expand-1)
     ;'("!" . )
     '("2" . meow-expand-2)
     ;'("@" . )
     '("3" . meow-expand-3)
     ;'("#" . )
     '("4" . meow-expand-4)
     ;'("$" . )
     '("5" . meow-expand-5)
     ;'("%" . )
     '("6" . meow-expand-6)
     ;'("^" . )
     '("7" . meow-expand-7)
     ;'("&" . )
     '("8" . meow-expand-8)
     ;'("*" . )
     '("9" . meow-expand-9)
     ;'("(" . )
     '("0" . meow-expand-0)
     ;'(")" . )

     ;; ===2nd Row===
     ;'("TAB"  . )
     ;'("BTAB" . )
     '("q" . kmacro-end-and-call-macro)
     '("Q" . cur/kmacro-toggle)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("r" . meow-replace)
     '("R" . meow-query-replace)
     '("t" . meow-till)
     '("T" . meow-till-expand)
     '("y" . meow-save)
     '("Y" . meow-clipboard-save)
     '("u" . meow-undo)
     ;'("U" . )
     '("i" . meow-insert)
     '("I" . meow-open-below)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-clipboard-yank)
     '("-" . negative-argument)
     ;'("_" . )

     ;; ===3rd Row===
     '("<escape>" . keyboard-quit)
     '("a" . meow-append)
     '("A" . meow-open-above)
     '("s" . repeat)
     ;'("S" . )
     '("d" . meow-kill)
     '("D" . meow-c-k)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '(";" . meow-reverse)
     '(":" . meow-page-up)
     '("RET" . "C-c")
     ;'("S-RET" . )

     ;; ===4th Row===
     '("z" . meow-pop-selection)
     ;'("Z" . )
     '("x" . meow-line)
     '("X" . meow-line-expand)
     '("c" . meow-change)
     ;'("C" . )
     (cons "v" goto-map)
     ;'("V" . )
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("n" . meow-search)
     '("N" . meow-visit)
     '("m" . meow-join)
     ;'("M" . )
     '("," . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '("." . meow-inner-of-thing)
     '(">" . meow-end-of-thing)
     (cons "/" search-map)
     '("?" . meow-page-down)
     '("'"  . embark-act)
     '("\"" . embark-dwim)))
  :init
  (defhydra cur/hydra-buffer-cycle (:timeout 4)
    "tab through buffers"
    ("n" next-buffer "next buffer")
    ("p" previous-buffer "previous buffer")
    ("f" nil "finished" :exit t)
    ("RET" nil "finished" :exit t))
  :custom
  (meow-mode-state-list
   '((authinfo-mode . normal)
     (beancount-mode . normal)
     (bibtex-mode . normal)
     (cider-repl-mode . normal)
     (cider-test-report-mode . normal)
     (cider-browse-spec-view-mode . motion)
     (cargo-process-mode . normal)
     (conf-mode . normal)
     (deadgrep-edit-mode . normal)
     (deft-mode . normal)
     (diff-mode . normal)
     (dired-mode . motion)
     (eat-mode . insert)
     (ediff-mode . motion)
     (eshell-mode . insert)
     (gud-mode . normal)
     (haskell-interactive-mode . normal)
     (help-mode . motion)
     (helpful-mode . normal)
     (json-mode . normal)
     (jupyter-repl-mode . normal)
     (mix-mode . normal)
     (occur-edit-mode . normal)
     (pass-view-mode . normal)
     (prog-mode . normal)
     (py-shell-mode . normal)
     (restclient-mode . normal)
     (telega-chat-mode . normal)
     (term-mode . normal)
     (text-mode . normal)
     (vterm-mode . insert)
     (Custom-mode . normal))
   "Default meow states for modes")
  (meow-selection-command-fallback
   '((meow-change . meow-change-char)
     (meow-kill . meow-C-d)
     (meow-cancel-selection . keyboard-quit)
     (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char))
   "Meow fallback commands")
  (meow-keypad-describe-delay 0.0 "No delay in keypad help popup")
  (meow-cheats-layout meow-cheatsheet-layout-qwerty "Meow qwerty layout for the cheatsheet")
  (meow-keypad-leader-dispatch nil)
  :bind ( :map cur/sub-leader-keymap
          ("C-l" . ibuffer)
          ("C-b" . bookmark-set-no-overwrite)
          :map cur/register-map
          ("C-j" . jump-to-register)
          ("C-s" . point-to-register)
          ("C-." . point-to-register)
          ("C-y" . copy-to-register)
          ("C-p" . insert-register)
          ("C-w" . window-configuration-to-register)
          ("C-n" . number-to-register)
          ("C-+" . increment-register)
          ("C-q" . kmacro-to-register)
          :map goto-map
          ("e a" . first-error)
          ("e n" . next-error)
          ("e p" . previous-error)
          (","   . xref-go-back)
          ("d"   . xref-find-definitions)
          ("r"   . xref-find-references)
          :map search-map
          ("n" . meow-visit)
          ("s" . isearch-forward)
          ("r" . isearch-backward))
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package cur-meow
  :after (meow)
  :bind ( :map cur/sub-leader-keymap
          ("C-n" . cur-meow-toggle-temp-normal-motion)))

(provide 'cur-config-bindings)
