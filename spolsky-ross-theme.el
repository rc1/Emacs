;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ---------------------------
;;
;; Spolsky : A dark color theme
;;
;; ----------------------------

;; Notes:
;; Standard faces: http://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html


(unless (>= 25 emacs-major-version)
  (error "requires Emacs 24 or later."))

(deftheme spolsky-ross  "An edited dark color theme for Emacs based on Sublime Text 2")

(custom-theme-set-variables
  'spolsky-ross
  '(linum-format " %7i "))

(let ((*background*         "#2a2b2f")
      (*comments*           "#8C8C8C")
      (*constant*           "#9cd6d4")
      (*current-line*       "#151515")
      (*cursor-underscore*  "#EEDC82")
      (*keywords*           "#cfff7d")

      ;; Sidebar line numbers
      (*line-number*        "#161A1F")
      (*line-fg*            "#666")

      (*type-face*          "#66D9EF")
      (*method-declaration* "#badeff")
      (*mode-inactive-bg*   "#222")
      (*mode-line-line*     "#EDECEC")
      (*mode-line-bg*       "#222") ;; "#EDECEC" "#F2F2F2" "#52532f" "#000000"
      (*mode-line-fg*       "#00ffff") ;; "#00ffff" "#EEDC82"
      (*mode-inactive-fg*   "#555")
      (*mode-id-fg*         "#FFFFFF") ;; "#FFFFFF"
      (*mode-id-bg*         "#FF0FFF")
      (*prompt*             "#FFFF00")
      (*normal*             "#DEDEDE")
      (*number*             "#31f8ff")
      (*operators*          "#FF80F4")
      (*warning*            "#FF6C60")
      (*regexp*             "#ff679f")
      (*string*             "#fffd31")
      (*variable*           "#FF80F4")
      (*visual-selection*   "#555")

      (*sub-function*       "##9ab2c8")
      (*sub-function-light  "#2a2b2f")

      (*level-1*            "#fa67ff")
      (*level-2*            "#ff7c67")
      (*level-3*            "#fffa67")
      (*level-4*            "#92ff67")
      (*level-5*            "#67fffa")
      (*level-6*            "#6772ff")

      (*light-ui*           "#313131")
      (*rc1-fn-arg*         "#dfffba")
      (*rc1-fn-ar1g*         "#c7baff"))

  (custom-theme-set-faces
   'spolsky-ross

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background* :foreground, *normal*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line* :underline t))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection*))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground, *constant*))))
   `(font-lock-doc-face ((t (:foreground, *string*))))
   `(font-lock-doc-string-face ((t (:foreground, *string*))))
   `(font-lock-function-name-face ((t (:foreground, *method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground, *keywords*))))
   `(font-lock-negation-char-face ((t (:foreground, *warning*))))
   `(font-lock-number-face ((t (:foreground, *number*))))
   `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *type-face*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *warning*))))

   ;; GUI
   `(fringe ((t (:background, *light-ui*))))
   `(linum ((t (:background, *line-number* :foreground, *line-fg*))))
   `(minibuffer-prompt ((t (:foreground, *prompt*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg* :overline t))))
   `(mode-line-inactive ((t (:background, *mode-inactive-bg* :foreground, *mode-inactive-fg*))))
   `(mode-line-buffer-id ((t (:background, *mode-id-bg* :foreground, *mode-id-fg*))))
   `(cursor ((t (:background, *cursor-underscore*))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *background*)))) ;; between splits

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *warning* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *keywords* :foreground, *normal* :weight bold))))

   ;; search
   `(isearch ((t (:background, *regexp* :foreground, *visual-selection*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *visual-selection*))))

   ;; js2-mode
   `(js2-function-param ((t (:foreground, *rc1-fn-ar1g*))))
   `(js2-function-call ((t (:foreground, *rc1-fn-arg*))))

   ;; Org-mode
   `(org-level-1 ((t (:foreground, *level-1*))))
   `(org-level-2 ((t (:foreground, *level-2*))))
   `(org-level-3 ((t (:foreground, *level-3*))))
   `(org-level-4 ((t (:foreground, *level-4*))))
   `(org-level-5 ((t (:foreground, *level-5*))))
   `(org-level-6 ((t (:foreground, *level-6*))))

   ))



;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'spolsky-ross)

;; Local Variables:
;; no-byte-compile: t
;; End:
