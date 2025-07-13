(deftheme minimal-tron-light
  "Light version of the minimal-tron theme, preserving its minimal Tron-like aesthetic.")

(let ((accent-1 "#FF7DBB")   ; same pink
      (active-bg "#E5F0F7")  ; light blue-grey
      (active-fg "#3D5666")  ; original dark bg becomes fg
      (default-1 "#387AAA") ; darker blue for contrast on light bg
      (bg "#FAFBFC")         ; very light background
      (fg "#2A3C4B")         ; darker text
      (comment "#81909C")    ; bluish gray for comments
      (doc "#9AA7B4")
      (highlight-bg "#DDEAF3")
      (region-bg "#CFE5F1")
      (class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'minimal-tron-light
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,active-fg))))
   `(minibuffer-prompt ((t (:foreground "#007BA7"))))
   `(region ((t (:background ,region-bg))))
   `(highlight ((t (:background ,highlight-bg))))
   `(trailing-whitespace ((t (:background ,accent-1))))
   `(shadow ((t (:foreground "#A0A0A0"))))

   ;; Font lock
   `(font-lock-builtin-face ((t (:foreground ,default-1))))
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
   `(font-lock-doc-face ((t (:foreground ,doc))))
   `(font-lock-function-name-face ((t (:foreground ,accent-1))))
   `(font-lock-keyword-face ((t (:foreground ,default-1))))
   `(font-lock-string-face ((t (:foreground "#5C6D7E"))))
   `(font-lock-constant-face ((t (:foreground ,default-1))))
   `(font-lock-type-face ((t (:foreground "#005F87"))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,accent-1 :background "#FBE3EF"))))

   ;; Org
   `(org-level-1 ((t (:foreground ,accent-1 :weight bold))))
   `(org-level-2 ((t (:foreground ,default-1))))
   `(org-level-3 ((t (:foreground ,accent-1))))
   `(org-level-4 ((t (:foreground "#A48FB0"))))

   ;; Mode line
   `(mode-line ((t (:foreground ,active-fg :background ,active-bg))))
   `(mode-line-inactive ((t (:foreground "#8AA2B0" :background "#EDEDED"))))
   `(mode-line-buffer-id ((t (:weight bold :foreground ,accent-1))))
   `(mode-line-emphasis ((t (:foreground ,fg))))

   ;; Tab bar
   `(tab-bar ((t (:inherit mode-line-inactive))))
   `(tab-bar-tab ((t (:inherit mode-line))))
   `(tab-bar-tab-inactive ((t (:inherit mode-line-inactive))))

   ;; Search
   `(isearch ((t (:foreground "#FFFFFF" :background "#7AB3D0"))))
   `(lazy-highlight ((t (:background "#CCEAF9"))))

   ;; Links
   `(link ((t (:foreground "#005BBB" :underline t))))
   `(link-visited ((t (:foreground "#8844BB" :underline t))))

   ;; Fringe, outline, etc.
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(outline-1 ((t (:foreground ,accent-1))))
   `(outline-2 ((t (:foreground ,default-1))))
   `(outline-3 ((t (:foreground "#A48FB0"))))
   `(outline-4 ((t (:foreground "#7A8896"))))
   ))

(provide-theme 'minimal-tron-light)
