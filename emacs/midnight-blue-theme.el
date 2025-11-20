;;; midnight-blue-theme.el --- TODO                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Varun Malladi

;; Author: Varun Malladi <varun.malladi@gmail.com>
;; Keywords: TODO

;; The theme is built around two colors: "midnight" and "blue". The standard scheme
;; is a midnight background with a blue foreground (e.g. for text).  "Heading-like"
;; elements are displayed in a complementary scheme: blue background and midnight
;; foreground. We find that blue provides sufficient contrast to midnight to be
;; easily legible, but not so much as to be uncomfortable to read in low-light
;; environments (as white would be).
;;
;; The choice of which elements to treat as "heading-like" is crucial. Particularly
;; in a minimalist theme like this, they can structure what may otherwise be a
;; monotonous wall of text.  They are most useful when perusing thorugh large amounts
;; of text / vertical space at a time, during which indentation and varying
;; foreground colors are less noticeable than varying background colors.
;; - In programming modes, function names (font-lock-function-name-face) work well.
;; - In text modes, headers are already instrinsic elements.  However, applying the
;;   same styling to all headers may not differentiate heading levels or
;;   subordinance. We should therefore apply this style only to top-level headers,
;;   which we note may be defined absolutely or relative to the window content.
;;
;; The cursor is your waypoint is deserves to be stand out the most amongst other
;; elements. Given the midnight background, the high-contrast white background when
;; applied to the cursor is highly distinct. Since the cursor itself takes up minimal
;; space on the screen, we don't find the high-contrast to be straining on the
;; eyes. A natural extension of the cursor is the selection, which we style with a
;; white background and midnight foreground.
;;
;; Literals, such as strings and numbers, are italicized. This is to emphasize their
;; distinct function in code but also not to distinguish them so much. This is
;; because we want to distinguish them within the line or function one is reading,
;; but not within the broader structure of the file/buffer.
;;
;; Font size is the only clear stylistic element that can achieve
;;   this, but it is not available in terminal environments. But that is only useful
;;   for determining the relationships between headings themselves, not the bodies.
;;   Therefore, the most important thing is indentation.

(defvar tm42-midnight-blue/midnight "#000000")
(defvar tm42-midnight-blue/blue "#00a7e7")
(defvar tm42-midnight-blue/white "#ffffff")
(defvar tm42-midnight-blue/off-white "#949494")

(defface tm42-midnight-blue/default-face
  `((t (:foreground
        ,tm42-midnight-blue/blue
        :background
        ,tm42-midnight-blue/midnight)))
  "TODO")

(defface tm42-midnight-blue/inverted-face
  `((t (:foreground
        ,tm42-midnight-blue/midnight
        :background
        ,tm42-midnight-blue/blue)))
  "TODO")

(deftheme midnight-blue
  "TODO")

(custom-theme-set-faces
 'midnight-blue
 `(default ,(custom-face-get-current-spec 'tm42-midnight-blue/default-face) t)
 `(fringe ((t (:inherit default))) t)

 ;; --- CURSOR
 `(highlight ((t (:background
                  ,tm42-midnight-blue/white
                  :foreground ,tm42-midnight-blue/midnight)))
             t)
 `(region ((t (:background
               ,tm42-midnight-blue/white
               :foreground ,tm42-midnight-blue/midnight)))
          t)
 `(secondary-selection ((t (:inherit
                            region
                            :background ,tm42-midnight-blue/off-white)))
                       t)
 '(isearch ((t (:inherit region))) t)
 '(lazy-highlight ((t (:inherit secondary-selection))) t)

 ;; --- MODE LINE
 `(mode-line ((t (:weight
                  normal
                  :foreground ,tm42-midnight-blue/white
                  :background ,tm42-midnight-blue/midnight
                  :box (:line-width (1 . 7) :color ,tm42-midnight-blue/midnight))))
             t)
 '(mode-line-inactive ((t (:inherit mode-line :foreground "#949494"))) t)
 '(which-func ((t (:foreground unspecified))) t)

 ;; --- TAB BAR
 '(tab-bar ((t (:inherit mode-line-inactive :box nil))) t)
 '(tab-bar-tab ((t (:inherit mode-line))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar))))

 ;; --- FONT LOCK
 ;; Literals:
 '(font-lock-string-face ((t (:inherit default :slant italic))))
 '(font-lock-comment-face ((t (:inherit default :foreground "#01354a"))) t)
 ;; Heading-likes:
 `(font-lock-function-name-face ,(custom-face-get-current-spec 'tm42-midnight-blue/inverted-face) t)
 `(eshell-prompt ,(custom-face-get-current-spec 'tm42-midnight-blue/inverted-face) t)
 ;; PL structure:
 '(font-lock-keyword-face ((t (:inherit default :foreground "#6cc8eb"))) t)
 '(font-lock-builtin-face ((t (:inherit font-lock-keyword-face))) t)
 '(font-lock-punctuation-face ((t (:inherit font-lock-keyword-face))) t)
 '(font-lock-bracket-face ((t (:inherit font-lock-punctuation-face))) t)
 '(font-lock-operator-face ((t (:inherit font-lock-punctuation-face))) t)
 ;; Overriding special styling:
 '(font-lock-variable-name-face ((t :inherit default)) t)
 '(font-lock-type-face ((t (:inherit default))) t)
 '(font-lock-constant-face ((t (:inherit default))) t)

 ;; --- ORG
 `(org-level-1 ,(custom-face-get-current-spec 'tm42-midnight-blue/inverted-face) t)

 ;; --- DIFF / EDIFF
 ;; Base diff colors - subtle, low-contrast backgrounds
 '(diff-added ((t (:background "#002a1a" :foreground unspecified))) t)
 '(diff-removed ((t (:background "#2a0a0a" :foreground unspecified))) t)
 '(diff-changed ((t (:background "#1a1a2a" :foreground unspecified))) t)
 '(diff-refine-added ((t (:background "#007a4a" :foreground unspecified))) t)
 '(diff-refine-removed ((t (:background "#7a2a2a" :foreground unspecified))) t)
 '(diff-refine-changed ((t (:background "#3a3a6a" :foreground unspecified))) t)

 ;; Ediff faces - consistent with diff
 '(ediff-current-diff-A ((t (:background "#2a0a0a" :foreground unspecified))) t)
 '(ediff-current-diff-B ((t (:background "#002a1a" :foreground unspecified))) t)
 '(ediff-current-diff-C ((t (:background "#1a1a2a" :foreground unspecified))) t)
 '(ediff-fine-diff-A ((t (:background "#7a2a2a" :foreground unspecified))) t)
 '(ediff-fine-diff-B ((t (:background "#007a4a" :foreground unspecified))) t)
 '(ediff-fine-diff-C ((t (:background "#3a3a6a" :foreground unspecified))) t)
 '(ediff-even-diff-A ((t (:background "#0a0a0a" :foreground unspecified))) t)
 '(ediff-even-diff-B ((t (:background "#0a0a0a" :foreground unspecified))) t)
 '(ediff-even-diff-C ((t (:background "#0a0a0a" :foreground unspecified))) t)
 '(ediff-odd-diff-A ((t (:background "#0a0a0a" :foreground unspecified))) t)
 '(ediff-odd-diff-B ((t (:background "#0a0a0a" :foreground unspecified))) t)
 '(ediff-odd-diff-C ((t (:background "#0a0a0a" :foreground unspecified))) t))

(provide-theme 'midnight-blue)

