(deftheme minimal-tron
  "Created 2024-07-09.")

(let ((accent-1 "#FF7DBB")   ; pink
      (active-bg "#3D5666")  ; blue-grey
      (active-fg "#CBECFF")  ; brighter blue-grey
      (default-1 "#8Fd4FF") ; light blue
      (class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'minimal-tron
   '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#B0CCDC" :background "#000000" :stipple nil :inherit nil))))
   `(cursor ((t (:background "#B0CCDC"))))
   `(fixed-pitch ((t (:family "Monospace"))))
   `(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
   `(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
   `(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
   `(minibuffer-prompt ((t (:weight normal :foreground "#4BB5BE"))))
   `(highlight ((t (:underline t))))
   `(region ((t (:extend nil :background "#2B4255"))))
   `(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
   `(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
   `(trailing-whitespace ((t (:background ,accent-1))))

   ;; FONT LOCK
   `(font-lock-builtin-face ((t (:foreground ,default-1))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:slant italic :foreground "#5A7387"))))
   `(font-lock-constant-face ((t (:foreground ,default-1))))
   `(font-lock-doc-face ((t (:foreground "#6A8397"))))
   `(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
   `(font-lock-function-name-face ((t (:foreground ,accent-1 :weight normal))))
   `(font-lock-keyword-face ((t (:weight normal :foreground ,default-1))))
   `(font-lock-negation-char-face ((t (:foreground "#BBF0EF"))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground "#B0CCDC"))))
   `(font-lock-type-face ((t (:foreground "#387AAA"))))
   `(font-lock-variable-name-face ((t (:foreground "#B0CCDC"))))
   `(font-lock-warning-face ((t (:foreground ,accent-1 :background "#0E1926"))))

   ;; ORG
   `(org-level-1 ((t (:foreground ,accent-1))))
   `(org-level-2 ((t (:foreground ,accent-1))))
   `(org-level-3 ((t (:foreground ,accent-1))))
   `(org-level-4 ((t (:foreground ,accent-1))))
   `(org-level-5 ((t (:foreground ,accent-1))))
   `(org-level-6 ((t (:foreground ,accent-1))))
   `(org-level-7 ((t (:foreground ,accent-1))))
   `(org-level-8 ((t (:foreground ,accent-1))))

   `(button ((t (:inherit (link)))))
   `(link ((t (:underline (:color foreground-color :style line) :foreground "#BBF0EF"))))
   `(link-visited ((t (:foreground "violet" :inherit (link)))))
   `(fringe ((t (:foreground ,active-fg))))
   `(header-line ((t (:underline (:color foreground-color :style line) :inverse-video nil :inherit (mode-line)))))
   `(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
   `(mode-line ((t (:weight normal :foreground ,active-fg :background ,active-bg))))
   `(mode-line-buffer-id ((t (:weight normal :foreground ,active-fg))))
   `(mode-line-emphasis ((t (:foreground "#B0CCDC"))))
   `(mode-line-highlight ((t (:weight normal :box nil :foreground ,default-1))))
   `(mode-line-inactive ((t (:weight normal :foreground "#90ACBC" :background "#1E1E1E"))))
   `(tab-bar ((t (:inherit mode-line-inactive))))
   `(tab-bar-tab-inactive ((t (:inherit mode-line-inactive))))
   `(tab-bar-tab ((t (:inherit mode-line))))
   `(isearch ((t (:weight normal :foreground "#FFFFFF" :background "#2B4255"))))
   `(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
   `(lazy-highlight ((t (:foreground "#BBCCDD" :background "#1B324B"))))
   `(match ((((class color) (min-colors 88) (background light)) (:background "khaki1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))
   `(which-func ((t (:inherit nil))))
   `(outline-1 ((t (:foreground "gray"))))
   `(outline-2 ((t (:foreground "gray"))))
   `(outline-3 ((t (:foreground ,accent-1))))
   `(outline-4 ((t (:foreground "gray"))))
   `(outline-5 ((t (:foreground "gray"))))
   `(outline-6 ((t (:foreground "gray"))))
   `(outline-7 ((t (:foreground "gray"))))
   `(outline-8 ((t (:foreground "gray"))))
   ))

(provide-theme 'minimal-tron)