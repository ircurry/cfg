;;; cur-gruvbox-theme.el --- My custom gruvbox theme

;;; Commentary:

;;; Code:

;; (require 'autothemer)

;; (add-to-list 'custom-theme-load-path (locate-user-emacs-file "/home/recur/cfg/home/programs/emacs/themes"))

(defun cur-theme-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

(defun cur-theme-blend (color1 color2 alpha)
  ""
  (if (and (string-prefix-p "#" color2) (string-prefix-p "#" color2))
      (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
             (cl-loop for it    in (cur-theme-name-to-rgb color1)
                      for other in (cur-theme-name-to-rgb color2)
                      collect (+ (* alpha it) (* other (- 1 alpha)))))
    (error "No \"#\" prefix for \"%s\" and \"%s\"" color2 color2)))

(defun cur-theme-lighten (color alpha)
  ""
  (cur-theme-blend color "#FFFFFF" (- 1 alpha)))

(defun cur-theme-darken (color alpha)
  ""
  (cur-theme-blend color "#000000" (- 1 alpha)))

(deftheme cur-gruvbox
  "Asdf.")

(let* (
      ;; Normal colours
      (red        "#cc241d")
      (green      "#98971a")
      (yellow     "#d79921")
      (blue       "#458588")
      (purple     "#b16286")
      (aqua       "#689d6a")
      (orange     "#d65d0e")

      ;; Bright colours
      (light-red        "#fb4934")
      (light-green      "#b8bb26")
      (light-yellow     "#fabd2f")
      (light-blue       "#83a598")
      (light-purple     "#d3869b")
      (light-aqua       "#8ec07c")
      (light-orange     "#fe8019")

      ;; Colored Background
      (bg-red    (cur-theme-blend "#cc241d" "#282828" 0.1)) ; red
      (bg-green  (cur-theme-blend "#b8bb26" "#282828" 0.1)) ; light-green
      (bg-yellow (cur-theme-blend "#fabd2f" "#282828" 0.1)) ; light-yellow
      (bg-blue   (cur-theme-blend "#83a598" "#282828" 0.1)) ; light-blue
      (bg-purple (cur-theme-blend "#d3869b" "#282828" 0.1)) ; light-purple
      (bg-aqua   (cur-theme-blend "#8ec07c" "#282828" 0.1)) ; light-aqua
      (bg-orange (cur-theme-blend "#fe8019" "#282828" 0.1)) ; light-orange

      ;; Pale Colors
      (pale-red    (cur-theme-blend "#cc241d" "#ebdbb2" 0.6))
      (pale-green  (cur-theme-blend "#b8bb26" "#ebdbb2" 0.6))
      (pale-yellow (cur-theme-blend "#fabd2f" "#ebdbb2" 0.6))
      (pale-blue   (cur-theme-blend "#83a598" "#ebdbb2" 0.6))
      (pale-purple (cur-theme-blend "#d3869b" "#ebdbb2" 0.6))
      (pale-aqua   (cur-theme-blend "#8ec07c" "#ebdbb2" 0.6))
      (pale-orange (cur-theme-blend "#fe8019" "#ebdbb2" 0.6))
      
      ;; Background colours
      (bg-sp1   "#0d1011")
      (bg0h     "#1d2021") ;; Darkest
      (bg0      "#282828") ;; Default bg
      (bg0s     "#32302f")
      (bg1      "#3c3836")
      (bg2      "#504945")
      (bg3      "#665c54")
      (bg4      "#7c6f64")
      (grey     "#928374") ;; Lightest

      ;; Foreground colours
      (fg4     "#a89984") ;; Darkest
      (fg3     "#bdae93")
      (fg2     "#d5c4a1")
      (fg1     "#ebdbb2") ;; Default fg
      (fg0     "#fbf1c7") ;; Lightest
      )
  (custom-theme-set-faces
   'cur-gruvbox
   `(default             ((t (:foreground ,fg1 :background ,bg0))))
   `(success             ((t (:foreground ,light-green))))
   `(warning             ((t (:foreground ,light-yellow))))
   `(error               ((t (:foreground ,light-red))))
   `(link                ((t (:foreground ,light-aqua :underline t :weight bold))))
   `(link-visited        ((t (:foreground ,aqua :underline t :weight bold))))
   `(cursor              ((t (:background ,fg0))))
   `(fringe              ((t (:distant-foreground ,fg1 :background ,bg3))))
   `(region              ((t (:background ,bg3))))
   `(highlight           ((t (:foreground ,bg-sp1 :distant-foreground ,fg0 :background ,light-yellow))))
   `(hl-line             ((t (:background ,bg1))))
   `(header-line         ((t (:inherit (mode-line)))))
   `(vertical-border     ((t (:foreground ,bg0 :background ,bg0))))
   `(secondary-selection ((t (:background ,bg2))))
   `(query-replace       ((t (:inherit isearch))))
   `(minibuffer-prompt   ((t (:foreground ,aqua))))
   `(tooltip             ((t (:foreground ,fg1 :background ,bg0h))))
   `(shadow              ((t (:foreground ,bg4))))

   ;; Font Lock Faces

   `(font-lock-builtin-face           ((t (:foreground ,light-orange))))
   `(font-lock-comment-face           ((t (:foreground ,grey :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face               ((t (:foreground ,fg2))))
   `(font-lock-function-name-face     ((t (:foreground ,light-green))))
   `(font-lock-function-call-face     ((t (:inherit font-lock-function-name-face))))
   `(font-lock-keyword-face           ((t (:foreground ,light-red))))
   `(font-lock-preprocessor-face      ((t (:foreground ,fg1 :weight bold))))
   `(font-lock-string-face            ((t (:foreground ,light-green))))
   `(font-lock-type-face              ((t (:foreground ,light-yellow))))
   `(font-lock-constant-face          ((t (:foreground ,light-purple))))
   `(font-lock-variable-name-face     ((t (:foreground ,light-blue))))
   `(font-lock-warning-face           ((t (:inherit warning))))
   `(font-lock-negation-char-face     ((t (:foreground ,fg1 :weight bold))))
   `(font-lock-number-face            ((t (:foreground ,light-purple))))

   ;; Mode Line

   `(mode-line           ((t (:background ,bg1))))
   `(mode-line-active    ((t (:inherit mode-line))))
   `(mode-line-emphasis  ((t (:foreground ,light-yellow :distant-foreground ,bg0))))
   `(mode-line-inactive  ((t (:foreground ,bg3 :background ,bg0s))))
   `(mode-line-buffer-id ((t (:weight bold))))

   ;; Line Number

   `(line-number              ((t (:foreground ,bg4 :background ,bg0 :inherit default :slant italic :weight normal))))
   `(line-number-current-line ((t (:foreground ,light-yellow :background ,bg1 :inherit (hl-line default) :slant italic :weight normal))))

   ;; Isearch

   `(isearch         ((t (:foreground ,bg0h :background ,light-orange))))
   `(isearch-fail    ((t (:foreground ,fg0 :background ,light-red))))
   `(isearch-group-1 ((t (:foreground ,bg0h :background ,light-orange))))
   `(isearch-group-2 ((t (:foreground ,bg0h :background ,orange))))
   `(lazy-highlight  ((t (:foreground ,bg0h :background ,light-yellow))))
    
;;; Minibuffer and Completions

   ;; Match and Completions

   `(match                   ((t (:foreground ,pale-orange :background ,bg-orange))))
   `(completions-common-part ((t (:foreground ,pale-aqua :background ,bg-aqua))))

   ;; Orderless

   `(orderless-match-face-0 ((t (:foreground ,pale-blue :background ,bg-blue))))
   `(orderless-match-face-1 ((t (:foreground ,pale-red :background ,bg-red))))
   `(orderless-match-face-2 ((t (:foreground ,pale-green :background ,bg-green))))
   `(orderless-match-face-3 ((t (:foreground ,pale-yellow :background ,bg-yellow))))
    
   ;; Vertico

   `(vertico-current         ((t (:background ,bg2))))
   `(vertico-group-separator ((t (:strike-through t :inherit (shadow)))))
   `(vertico-group-title     ((t (:slant italic :inherit (shadow)))))
   `(vertico-multiline       ((t (:inherit (shadow)))))

   ;; Vertico Posframe

   `(vertico-posframe                 ((t (:inherit (default)))))
   `(vertico-posframe-border          ((t (:background ,grey))))
   `(vertico-posframe-border-2        ((t (:background ,light-red))))
   `(vertico-posframe-border-3        ((t (:background ,light-green))))
   `(vertico-posframe-border-4        ((t (:background ,light-blue))))
   `(vertico-posframe-border-fallback ((t (:background ,light-yellow))))
    
    
;;; Terminal and Shell Colors
    
   ;; Ansi Colors

   `(ansi-color-black          ((t (:foreground ,bg0h :background ,bg0h))))
   `(ansi-color-red            ((t (:foreground ,light-red :background ,light-red))))
   `(ansi-color-green          ((t (:foreground ,light-green :background ,light-green))))
   `(ansi-color-yellow         ((t (:foreground ,light-yellow :background ,light-yellow))))
   `(ansi-color-blue           ((t (:foreground ,light-blue :background ,light-blue))))
   `(ansi-color-magenta        ((t (:foreground ,light-purple :background ,light-purple))))
   `(ansi-color-cyan           ((t (:foreground ,light-aqua :background ,light-aqua))))
   `(ansi-color-white          ((t (:foreground ,fg4 :background ,fg4))))
   `(ansi-color-bright-black   ((t (:foreground ,grey :background ,grey))))
   `(ansi-color-bright-red     ((t (:foreground ,light-red :background ,light-red))))
   `(ansi-color-bright-green   ((t (:foreground ,light-green :background ,light-green))))
   `(ansi-color-bright-yellow  ((t (:foreground ,light-yellow :background ,light-yellow))))
   `(ansi-color-bright-blue    ((t (:foreground ,light-blue :background ,light-blue))))
   `(ansi-color-bright-magenta ((t (:foreground ,light-purple :background ,light-purple))))
   `(ansi-color-bright-cyan    ((t (:foreground ,light-aqua :background ,light-aqua))))
   `(ansi-color-bright-white   ((t (:foreground ,fg1 :background ,fg1))))

;;; Programming Auxiliary
    
   ;; Paren Mode

   `(show-paren-match            ((t (:background ,bg4))))
   `(show-paren-match-expression ((t (:inherit show-paren-match))))
   `(show-paren-mismatch         ((t (:foreground ,fg0 :background ,light-red :weight bold))))
    
   ;; Rainbow Delimiters

   `(rainbow-delimiters-base-face       ((t (:inherit default))))
   `(rainbow-delimiters-base-error-face ((t (:foreground ,light-red :background ,bg0h))))
   `(rainbow-delimiters-unmatched-face  ((t (:foreground ,bg0h :background ,light-red :weight bold))))
   `(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-base-unmatched-face))))
   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,light-orange))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,light-red))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,light-green))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,light-blue))))
   `(rainbow-delimiters-depth-5-face    ((t (:foreground ,light-aqua))))
   `(rainbow-delimiters-depth-6-face    ((t (:foreground ,light-purple))))
   `(rainbow-delimiters-depth-7-face    ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-8-face    ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-9-face    ((t (:foreground ,green))))

;;; Languages

   ;; 
    

;;; Internet Modes

   ;; Elpher

   `(elpher-binary                     ((t (:inherit (elpher-unknown)))))
   `(elpher-gemini                     ((t (:foreground ,light-aqua :weight normal :inherit (link)))))
   `(elpher-gemini-heading1            ((t (:foreground ,light-red :height 1.8 :inherit (default)))))
   `(elpher-gemini-heading2            ((t (:foreground ,light-yellow :height 1.4 :inherit (default)))))
   `(elpher-gemini-heading3            ((t (:foreground ,light-orange :height 1.2 :inherit (default)))))
   `(elpher-gemini-preformatted        ((t (:foreground ,grey :inherit (fixed-pitch)))))
   `(elpher-gemini-preformatted-toggle ((t (:inherit (button)))))
   `(elpher-gemini-quoted              ((t (:inherit (italic)))))
   `(elpher-html                       ((t (:inherit (elpher-other-url)))))
   `(elpher-image                      ((t (:inherit (elpher-other-url)))))
   `(elpher-index                      ((t (:foreground ,light-red))))
   `(elpher-info                       ((t (:inherit (elpher-text)))))
   `(elpher-margin-brackets            ((t (:inherit (bold)))))
   `(elpher-margin-key                 ((t (:inherit (shadow)))))
   `(elpher-other-url                  ((t (:foreground ,bg4 :slant italic))))
   `(elpher-search                     ((t (:foreground ,light-yellow))))
   `(elpher-telnet                     ((t (:inherit (elpher-unknown)))))
   `(elpher-text                       ((t (:inherit (bold)))))
   `(elpher-unknown                    ((t (:foreground ,bg4))))

   ;; Gemini

   `(gemini-heading-face-1    ((t (:foreground ,light-red :height 1.8 :inherit (default)))))
   `(gemini-heading-face-2    ((t (:foreground ,light-yellow :height 1.4 :inherit (default)))))
   `(gemini-heading-face-3    ((t (:foreground ,light-orange :height 1.2 :inherit (default)))))
   `(gemini-heading-face-rest ((t (:inherit (bold)))))
   `(gemini-preformatted-face ((t (:foreground ,grey :inherit (fixed-pitch)))))
   `(gemini-quote-face        ((t (:inherit (italic)))))
   `(gemini-ulist-face        ((t (:inherit (default)))))

;;; Org

   ;; Org General
   `(org-code      ((t (:foreground ,light-orange :inherit org-block))))
   `(org-drawer    ((t (:foreground ,(cur-theme-lighten light-aqua 0.4)))))
   `(org-ellipsis  ((t (:foreground ,light-orange))))
   `(org-formula   ((t (:foreground ,light-green))))
   `(org-meta-line ((t (:foreground ,grey))))
   `(org-table     ((t (:foreground ,light-aqua))))
   `(org-verbatim  ((t (:foreground ,light-yellow))))

   ;; Outline N and Org Heading Levels
   `(outline-1 ((t (:foreground ,light-purple :weight bold :extend t))))
   `(outline-2 ((t (:foreground ,light-aqua :weight bold :extend t))))
   `(outline-3 ((t (:foreground ,light-green :weight bold :extend t))))
   `(outline-4 ((t (:foreground ,(cur-theme-lighten light-purple 0.2) :weight bold :extend t))))
   `(outline-5 ((t (:foreground ,(cur-theme-lighten aqua 0.25) :weight bold :extend t))))
   `(outline-6 ((t (:foreground ,(cur-theme-lighten light-purple 0.4) :weight bold :extend t))))
   `(outline-7 ((t (:foreground ,(cur-theme-lighten aqua 0.5) :weight bold :extend t))))
   `(outline-8 ((t (:foreground ,(cur-theme-lighten light-aqua 0.6) :weight bold :extend t))))
   
   ;; Org Blocks
   `(org-block            ((t (:background ,bg1 :extend t :inherit (default)))))
   `(org-block-begin-line ((t (:foreground ,grey :inherit (org-block)))))
   `(org-block-end-line   ((t (:inherit (org-block-begin-line)))))
   `(org-quote     ((t (:foreground ,fg2  :inherit (italic org-block)))))

   `(nerd-icons-blue       ((t (:foreground ,light-blue))))
   `(nerd-icons-blue-alt   ((t (:foreground ,light-aqua))))
   `(nerd-icons-cyan       ((t (:foreground ,light-aqua))))
   `(nerd-icons-cyan-alt   ((t (:foreground ,light-aqua))))
   `(nerd-icons-dblue      ((t (:foreground ,blue))))
   `(nerd-icons-dcyan      ((t (:foreground ,aqua))))
   `(nerd-icons-dgreen     ((t (:foreground ,(cur-theme-darken light-green 0.3)))))
   `(nerd-icons-dmaroon    ((t (:foreground ,(cur-theme-darken red 0.3)))))
   `(nerd-icons-dorange    ((t (:foreground ,(cur-theme-darken light-orange 0.3)))))
   `(nerd-icons-dpink      ((t (:foreground ,(cur-theme-lighten light-red 0.15)))))
   `(nerd-icons-dpurple    ((t (:foreground ,(cur-theme-darken light-purple 0.3)))))
   `(nerd-icons-dred       ((t (:foreground ,(cur-theme-darken light-red 0.3)))))
   `(nerd-icons-dsilver    ((t (:foreground ,(cur-theme-lighten grey 0.1)))))
   `(nerd-icons-dyellow    ((t (:foreground ,(cur-theme-darken yellow 0.3)))))
   `(nerd-icons-green      ((t (:foreground ,light-green))))
   `(nerd-icons-lblue      ((t (:foreground ,(cur-theme-lighten light-blue 0.3)))))
   `(nerd-icons-lcyan      ((t (:foreground ,(cur-theme-lighten light-aqua 0.3)))))
   `(nerd-icons-lgreen     ((t (:foreground ,(cur-theme-lighten light-green 0.3)))))
   `(nerd-icons-lmaroon    ((t (:foreground ,(cur-theme-lighten red 0.3)))))
   `(nerd-icons-lorange    ((t (:foreground ,(cur-theme-lighten light-orange 0.3)))))
   `(nerd-icons-lpink      ((t (:foreground ,(cur-theme-lighten light-red 0.55)))))
   `(nerd-icons-lpurple    ((t (:foreground ,(cur-theme-lighten light-purple 0.3)))))
   `(nerd-icons-lred       ((t (:foreground ,(cur-theme-lighten light-red 0.3)))))
   `(nerd-icons-lsilver    ((t (:foreground ,(cur-theme-lighten grey 0.7)))))
   `(nerd-icons-lyellow    ((t (:foreground ,(cur-theme-lighten yellow 0.3)))))
   `(nerd-icons-maroon     ((t (:foreground ,red))))
   `(nerd-icons-orange     ((t (:foreground ,light-orange))))
   `(nerd-icons-pink       ((t (:foreground ,(cur-theme-lighten light-red 0.35)))))
   `(nerd-icons-purple     ((t (:foreground ,light-purple))))
   `(nerd-icons-purple-alt ((t (:foreground ,(cur-theme-blend light-purple grey 0.15)))))
   `(nerd-icons-red        ((t (:foreground ,light-red))))
   `(nerd-icons-red-alt    ((t (:foreground ,(cur-theme-blend light-red grey 0.15)))))
   `(nerd-icons-silver     ((t (:foreground ,(cur-theme-lighten grey 0.45)))))
   `(nerd-icons-yellow     ((t (:foreground ,yellow))))
   )
  )

(provide-theme 'cur-gruvbox)
;;; cur-gruvbox-theme.el ends here
