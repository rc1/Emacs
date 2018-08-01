;; # Packages
(setq package-list '(ido flx-ido multiple-cursors flycheck ace-jump-mode rainbow-delimiters auto-complete ido-vertical-mode less-css-mode yaml-mode projectile imenu-anywhere sws-mode rainbow-mode js2-mode skewer-mode nyan-mode flycheck js2-refactor yasnippet markdown-mode undo-tree nodejs-repl projectile-speedbar highlight-indent-guides jade-mode))
;; ## Requires Emacs' Package functionality
(require 'package)
;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Initialise the package system.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("b07089ce370c09e056fb867cfdd1f49164d3ca04668fba7e4ed011536c0890ec" default))))

;; ## Auto Install Packages
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; nstall the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; # Mac like keyboard shortcuts

;; ## Super Meta etc
(setq mac-option-modifier 'meta) ;; M
(setq mac-command-modifier 'super) ;; s
(setq ns-function-modifier 'control)
;; There is also 'hyper ;; H

;; Scrolling
(setq jit-lock-defer-time 0.5)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; ## Copy Paste Cut

(delete-selection-mode 1)

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

;; ## Force tab indetation
(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))


;; http://stackoverflow.com/questions/13186811/emacs-extending-expanding-region-so-that-it-embraces-whole-lines
(defun :my-bol-at (point) (interactive)
  (save-excursion
    (goto-char point)
    (point-at-bol)))

(defun :my-eol-at (point) (interactive)
  (save-excursion
    (goto-char point)
    (point-at-eol)))

(defun expand-region-to-whole-line () (interactive)
  (when (region-active-p)
    (let ((beg (region-beginning)) (end (region-end)))
      (goto-char (:my-bol-at beg))
      (set-mark (point))
      (goto-char (:my-eol-at end)))))

;; # Yasnippet
;; Not adding this via elpa so there
;; are no snippets by default
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-installed-snippets-dir nil) ;; This should disable preinstalled snippets
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        ;; "some other place"
       ))

;; # Ido Setup
(require 'ido)
(require 'flx-ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq id-everywhere t)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; If don't want to use the flx's highlights
;; (setq flx-ido-use-faces nil)
(setq ido-ubiquitous-mode t)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
;; Use up down keys to move through options
;; and left right to move through directories, yay!
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; # Speedbar
(custom-set-variables '(speedbar-show-unknown-files t))
(require 'projectile-speedbar)

;; # Recent files
;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
(require 'recentf)
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
;; enable recent files mode.
(recentf-mode t)
; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
;; link with ido
(defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
      (message "Aborting")))

;; # Better Buffer
(defalias 'list-buffers 'ibuffer)

;; # The Bell
;; (setq ring-bell-function #'ignore)
(setq visible-bell 'bottom) ;; 'top-bottom
;; (setq ring-bell-function (lambda () (message "*beep*")))

;; # Sessions
;; Save the state of emacs on exit
;; It needs to be save in the Aquamacs
;; preferences location?
(desktop-save-mode 1)

;; # JS2 Mode
;; To use it as a major mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Hook into node.js in shell
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(setq js2-highlight-level 3)

;; Linter
(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; # C++
(setq-default c-basic-offset 4)

;; # Recent files
;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
(require 'recentf)
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
;; enable recent files mode.
(recentf-mode t)
; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
;; link with ido
(defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
      (message "Aborting")))

;; # Auto Complete
(require 'auto-complete-config)
(setq
 ac-fuzzy-complete t
 ac-auto-start 2
 ac-auto-show-menu 0.0
 ac-delay 0.1
 ac-delay 0.2)

;; # Comment line of region
;; Instead of undo
(defun comment-or-uncomment-region-or-line()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning) end (region-end))
          (save-excursion
            (setq beg (progn (goto-char beg) (line-beginning-position))
                  end (progn (goto-char end) (line-end-position)))))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

;; # Multiple Cursors
(require 'multiple-cursors)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; # Projectile
(require 'projectile)
(projectile-global-mode)
;; (add-hook 'ruby-mode-hook 'projectile-on)

;; # JS2 Mode
;; To use it as a major mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json5\\'" . js2-mode))
;; Hook into node.js in shell
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; To customise it
;;    M-x customize-group RET js2-mode RET

;; # C++
(setq-default c-basic-offset 4)

;; # Mode line in header
;; Command to toggle the display of the mode-line as a header
(defvar-local header-line-format nil)
(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (set-window-buffer nil (current-buffer)))
(add-hook 'text-mode-hook 'mode-line-in-header)
(add-hook 'prog-mode-hook 'mode-line-in-header)

;; # Artistic Style
;;
;; Adds spaces between:
;; + function arguments
;; + operators
;;
;; astyle needs to be installed `brew install astyle`
(defun astyle-this-buffer (pmin pmax)
  (interactive "r")
  (shell-command-on-region pmin pmax
                           "astyle --pad-oper --pad-paren-in" ;; add options here...
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*") t))

;; # Line Moveing

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; # Frame loading and saving
(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
        )

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file)))))

(defun load-framegeometry ()
  "Loads ~/.emacs.d/framegeometry which should load the previous frame's geometry."
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file))))

;; Special work to do ONLY when there is a window system being used
(if window-system
    (progn
      (add-hook 'after-init-hook 'load-framegeometry)
      (add-hook 'kill-emacs-hook 'save-framegeometry)))

;; # Ace Jump Mode
;; Demo: http://dl.dropboxusercontent.com/u/3254819/AceJumpModeDemo/AceJumpDemo.h
(require 'ace-jump-mode)
;; C-c SPC ace-jump-word-mode
;; C-u C-c SPC ace-jump-char-mode
;; C-u C-u C-c SPC ace-jump-line-mode

;; # Rainbow Mode

;; # Rainbow Delimeters
(require 'rainbow-delimiters)
;; ## Customize the colors
;; ;; Source: http://ergoemacs.org/misc/emacs_rainbow-delimiters-mode.html
;; ;; Set the colours
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))) t)
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))) t)
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#00ff00"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#00fff6"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#aaff00"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#f600ff"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#ffff00"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#00ffea"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#ff0600"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#f6ff00"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2"))) t))

;; # Auto complete
(require 'auto-complete)
(global-auto-complete-mode t)

;; # File reloading
(global-auto-revert-mode t)

;; # Linum
;; (setq linum-format 'dynamic)
(require 'linum)
(add-hook 'linum-before-numbering-hook '(lambda () (setq linum-format "%4d ")))
;; (add-hook 'linum-before-numbering-hook '(lambda () ))

;; Org Mouse
(load "~/.emacs.d/org-mouse.el")
(require 'org-mouse)

;; # Line Highlight
;; (global-hl-line-mode 1)
(load "~/.emacs.d/highline.el")
(require 'highline)
(highline-mode 1)
(global-highline-mode t)
(set-face-background 'highline-face "#121214")

;; # Nyan Mode
;; This is also how to enable a mode
;; for all programming langauges they inherit
;; for prog-mode
(add-hook 'prog-mode-hook 'nyan-mode)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; # Clojure Repl (Cider)
;; Stop it opening in a new window
(setq cider-repl-pop-to-buffer-on-connect nil)

;; # Move around more quickly
;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; ## Saner deleteing back
(defun rc1-stepped-delete-back ()
  (interactive)
  (let ((here (point)))
    (forward-whitespace -1)
    (delete-region (point) here)))

;; ## Indetation highlighing
;; (load "~/.emacs.d/highlight-indentation")
;; (add-hook 'python-mode-hook 'highlight-ntation-mode)
;; (add-hook 'js2-mode-hook 'highlight-indentation-mode)
;; (add-hook 'less-css-hook 'highlight-indentation-mode)
;; (add-hook 'jade-hook 'highlight-indentation-mode)
;; (set-face-background 'highlight-indentation-face "#000000")
;; (set-face-background 'highlight-indentation-current-column-face "#000000")
;; (set-face-stipple 'highlight-indentation-face (list 2 2 (string 1 2)))
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character) ;; 'column 'fill
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-to-list 'auto-mode-alist '("\\.jade$\\'" . highlight-indent-guides-mode))
(set-face-background 'highlight-indent-guides-odd-face "#373C63")
(set-face-background 'highlight-indent-guides-even-face "#304852")
(set-face-foreground 'highlight-indent-guides-character-face "#373C63")

;; # Indentation
(setq-default indent-tabs-mode nil)
;; (add-hook 'prog-mode-hook
;;     '(setq-default indent-tabs-mode nil))

;; # New lines and indentatiom
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; # Pasting
(delete-selection-mode 1)

;; # Window Resizing
;; Up and down may conflict

(defun shrink-window-horizontally-fast ()
  (interactive)
  (shrink-window-horizontally 3))
(defun enlarge-window-horizontally-fast ()
  (interactive)
  (enlarge-window-horizontally 3))

;; Window title
(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

;; Toolbar
(if window-system
    (tool-bar-mode -1))

;; Searching
;; Keep the search highlighted
(setq lazy-highlight-cleanup nil)
;; to clean up
;; M-x lazy-highlight-cleanup

;; Style
(require 'rainbow-mode)
(set-default 'cursor-type 'bar)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
;; (set-face-background 'fringe "#313131")
(load "~/.emacs.d/spolsky-ross-theme.el")
(load-theme `spolsky-ross t)

;; Mark moving
;; Ref: http://stackoverflow.com/a/3399064/179015
(defun unpop-to-mark-command ()
  "Unpop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (interactive)
  (let ((num-times (if (equal last-command 'pop-to-mark-command) 2
                     (if (equal last-command 'unpop-to-mark-command) 1
                       (error "Previous command was not a (un)pop-to-mark-command")))))
    (dotimes (x num-times)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (+ 0 (car (last mark-ring))) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (mark t)))
      (deactivate-mark))))

;; JS2 Mode & Fly Check
(add-hook 'js2-mode-hook 'flycheck-mode)
(require 'js2-mode)
;; Disable js2 mode's syntax error highlighting by default...
(setq-default js2-mode-show-parse-errors nil
            js2-mode-show-strict-warnings nil)
;; ... but enable it if flycheck can't handle javascript
(autoload 'flycheck-get-checker-for-buffer "flycheck")
(defun sanityinc/disable-js2-checks-if-flycheck-active ()
(unless (flycheck-get-checker-for-buffer)
  (set (make-local-variable 'js2-mode-show-parse-errors) t)
  (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
(add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

;; JS2 Mode Shorthand
;; https://github.com/howardabrams/dot-files/blob/master/emacs-javascript.org
(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)(" ;; \\(function *\\
              (0 (progn (compose-region (match-beginning 1) (match-end 1) "#" ) ;;ƒ
                        nil)))))

;; Dired File Opening on OSX
(defun ergoemacs-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))) ) ) )

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
        )
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Shortcuts
;; To find the shortcut do: C-h k then the keypress

;; Clipboard, not the emacs way
;; (global-set-key (kbd "s-S-c") 'pbcopy)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-S-v") 'pbpaste)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-S-x") 'pbcut)
(global-set-key (kbd "s-x") 'kill-region)
;; Undo/redo
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'redo)
;; Selection
(global-set-key (kbd "s-a") 'mark-whole-buffer)
;; Framewindow, buffer
(global-set-key (kbd "s-{") 'previous-buffer) ;; this should be different
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-multiframe-window)
(global-set-key (kbd "M-]") 'next-multiframe-window)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
;; Framesize
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<S-C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<S-C-left>") 'shrink-window-horizontally-fast)
(global-set-key (kbd "<S-C-right>") 'enlarge-window-horizontally-fast)
;; Indentation
(global-set-key (kbd "s-[") (lambda () (interactive) (expand-region-to-whole-line) (shift-left 4)))
(global-set-key (kbd "s-]") (lambda () (interactive) (expand-region-to-whole-line) (shift-right 4)))
(global-set-key (kbd "M-s-i") 'astyle-this-buffer)
;; Moving text
(global-set-key [C-s-up] 'move-text-up)
(global-set-key [C-s-down] 'move-text-down)
;; Ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-char-mode)
;; Open Recent
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; Commenting
;;(global-unset-key (kbd "C-/")) <-- for aquamacs
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
;; Open Recent
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; Text size
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
;; Align
(global-set-key (kbd "C-c a") 'align-regexp)
;; Line numbers (linum)
(global-set-key (kbd "s-l") 'linum-mode)
;; New lines
(global-set-key (kbd "S-RET") 'end-of-line-and-indented-new-line)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Selection
(global-set-key (kbd "s-e") 'expand-region-to-whole-line)
;; Deleteing
(global-set-key "\M-\d" 'rc1-stepped-delete-back)
;; Better ido file
;; (global-set-key (kbd "M-SPC") 'ido-recentf-open)
;; Imenu-anywhere
(global-set-key (kbd "C-.") 'imenu-anywhere)
;; Projectile
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c b") 'projectile-switch-to-buffer)
;; Auto Complete
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;; Unbind the minimise (suspend-frame key)
(global-unset-key "\C-z")
;; Make save all just save current file
(global-set-key (kbd "C-x s") 'save-buffer)
;; Move to previous mark
(global-set-key (kbd "s-.") 'pop-to-mark-command)
(global-set-key (kbd "s->") 'unpop-to-mark-command)
;; js-2 refactor short menu
(js2r-add-keybindings-with-prefix "C-c C-m")
;; Dired file opening
(defun rc1/dired-keys ()
  "Modify keymaps used by `dired-mode'."
  (local-set-key (kbd "C-o") 'ergoemacs-open-in-external-app))
(add-hook 'dired-mode 'rc1/dired-keys)
;; Speedbar
(global-set-key (kbd "<f12>") 'projectile-speedbar-open-current-buffer-in-tree)

