(deftheme tm42-tos
  "Created 2026-06-29.")

(let ((default-bg "#04002b")
      (default-fg "#6498e3")
      (hl-fg "#1F233D")
      (hl-bg "#7EADDD")
      (comment-fg "#252c70"))
  (custom-theme-set-faces
   'tm42-tos
   `(default
     ((t (:inherit nil :extend nil :stipple nil
                   :background ,default-bg
                   :foreground ,default-fg))))

   `(font-lock-comment-face
     ((t (:inherit (default)
                   :foreground ,comment-fg))))
   '(font-lock-comment-delimiter-face
     ((default (:inherit (font-lock-comment-face)))))
   '(whitespace-space ((t :foreground "#151b52")))

   '(font-lock-constant-face
     ((t (:inherit (default)))))
   '(font-lock-string-face
     ((t (:slant italic :inherit (default)))))

   '(font-lock-keyword-face
     ((t (:inherit (default) :weight bold))))
   '(font-lock-builtin-face
     ((t (:inherit (font-lock-keyword-face)))))
   '(font-lock-variable-name-face
     ((t (:inherit (default) :foreground "#2A5AC6"))))
   '(font-lock-type-face
     ((t (:inherit (font-lock-keyword-face)))))

   `(region
     ((t (:extend t
                  :foreground ,hl-fg
                  :background ,hl-bg))))
   '(font-lock-function-name-face
     ((t (:inherit (region) :weight bold))))

   '(match
     ((t (:inherit (region)))))

   `(mode-line
     ((t (:weight normal :box (:line-width (1 . 7)
                                           :color ,hl-bg
                                           :style nil)
                  :inherit (region)))))
   '(mode-line-buffer-id
     ((t (:weight bold))))
   '(mode-line-emphasis
     ((t (:weight bold))))
   `(mode-line-highlight
     ((((supports :box t)
        (class color grayscale)
        (min-colors 88))
       (:box (:line-width (2 . 2)
                          :color ,hl-bg
                          :style released-button)))))
   '(mode-line-inactive
     ((t (:inherit (mode-line)))))
   '(which-func
     ((t (:inherit (mode-line)))))
   '(tab-bar ((t (:inherit mode-line-inactive :box nil))) t)
   '(tab-bar-tab ((t (:inherit mode-line))))
   '(tab-bar-tab-inactive ((t (:inherit tab-bar))))


   ))

;; (custom-theme-set-faces
;;  'tm42-tos
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#00005f" :foreground "#bdd6ff" ...))))
;;  '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
;;  '(minibuffer-prompt ((t (:foreground "#bdd6ff"))))
;;  '(highlight ((t (:foreground "#000000" :background "#ffffff"))))
;;  '(region ((t (:extend t :foreground "#000000" :background "#ffffff"))))
;;  '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
;;  '(secondary-selection ((t (:background "#949494" :inherit (region)))))
;;  '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
;;  '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
;;  '(font-lock-builtin-face ((t (:inherit (font-lock-keyword-face)))))
;;  '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
;;  '(font-lock-comment-face ((t (:foreground "#01354a" :inherit (default)))))
;;  '(font-lock-constant-face ((t (:inherit (default)))))
;;  '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
;;  '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
;;  '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
;;  '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
;;  '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
;;  '(font-lock-function-name-face ((t (:foreground "#000000" :background "#00a7e7"))))
;;  '(font-lock-keyword-face ((t (:foreground "#6cc8eb" :inherit (default)))))
;;  '(font-lock-negation-char-face ((t nil)))
;;  '(font-lock-number-face ((t nil)))
;;  '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
;;  '(font-lock-operator-face ((t (:inherit (font-lock-punctuation-face)))))
;;  '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
;;  '(font-lock-property-name-face ((t (:inherit (font-lock-punctuation-face)))))
;;  '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
;;  '(font-lock-punctuation-face ((t (:inherit (font-lock-keyword-face)))))
;;  '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
;;  '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
;;  '(font-lock-string-face ((t (:slant italic :inherit (default)))))
;;  '(font-lock-type-face ((t (:inherit (default)))))
;;  '(font-lock-variable-name-face ((t (:inherit (default)))))
;;  '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
;;  '(font-lock-warning-face ((t (:inherit (error)))))
;;  '(button ((t (:inherit (link)))))
;;  '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line :position nil) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line :position nil) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line :position nil) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line :position nil) :foreground "cyan")) (t (:inherit (underline)))))
;;  '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
;;  '(fringe ((t (:inherit (default)))))
;;  '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line :position nil) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line :position nil) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line :position nil) :box nil :inverse-video nil :foreground "white" :background "black"))))
;;  '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
;;  '(mode-line ((t (:weight normal :box (:line-width (1 . 7) :color "#000000" :style nil) :foreground "#ffffff" :background "#000000"))))
;;  '(mode-line-buffer-id ((t (:weight bold))))
;;  '(mode-line-emphasis ((t (:weight bold))))
;;  '(mode-line-highlight ((((supports :box t) (class color grayscale) (min-colors 88)) (:box (:line-width (2 . 2) :color "grey40" :style released-button))) (t (:inherit (highlight)))))
;;  '(mode-line-inactive ((t (:foreground "#949494" :inherit (mode-line)))))
;;  '(isearch ((t (:inherit (region)))))
;;  '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
;;  '(lazy-highlight ((t (:inherit (secondary-selection)))))
;;  '(match ((((class color) (min-colors 88) (background light)) (:background "khaki1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
;;  '(next-error ((t (:inherit (region)))))
;;  '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'tm42-tos)
