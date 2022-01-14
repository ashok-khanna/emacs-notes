;;; Credits to BAYES on #EMACS

; Set up MELPA bleeding-edge repository ----------------------------------------
;OB; Set up MELPA bleeding-edge repository
;; https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; Set up use-package -----------------------------------------------------------
;; Set up use-package
;; https://github.com/jwiegley/use-package/blob/a7422fb8ab1baee19adb2717b5b47b9c3812a84c/README.md#getting-started
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
    (add-to-list 'load-path "<path where use-package is installed>")
      (require 'use-package))
;; Ensure that all packages are installed globally
;; https://github.com/jwiegley/use-package/blob/a7422fb8ab1baee19adb2717b5b47b9c3812a84c/README.md#package-installation
(require 'use-package-ensure)
(setq use-package-always-ensure t)

; Update packages automatically ------------------------------------------------
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

; Profile initialization time --------------------------------------------------
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

; Auto save and backup----------------------------------------------------------
;; https://emacs.grym.io/#org95d343b
(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

(unless (file-exists-p emacs-autosave-directory)
    (make-directory emacs-autosave-directory))

(setq auto-save-file-name-transforms `((".*"
					,emacs-autosave-directory t)))

(setq auto-save-timeout 20
      auto-save-interval 20)

(setq auto-save-default t)

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory)))

(setq kept-new-versions 10
      kept-old-verisons 0)

(setq delete-old-versions t)

(setq backup-by-copying t)

(setq vc-make-backup-files t)

(use-package backup-each-save
	     :hook (after-save . backup-each-save))

; Auto revert buffer when file is modified -------------------------------------
(global-auto-revert-mode 1)

;; Remove some buffers ---------------------------------------------------------
;; https://unix.stackexchange.com/a/152151/88701
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;;(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; Speed bar -------------------------------------------------------------------
(use-package  sr-speedbar)

;; Key bindings ----------------------------------------------------------------
(use-package key-chord
  :init
  (key-chord-mode t)
  (key-chord-define-global "//" 'sr-speedbar-toggle)
  )

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; File navigation -------------------------------------------------------------
(ffap-bindings)

;; Window navigation -----------------------------------------------------------
;; split windows evenly by default
(setq window-resize-pixelwise  t)

;; tab-bar-mode
;; https://www.emacswiki.org/emacs/TabBarMode
(global-set-key [M-left] 'previous-buffer)
(global-set-key [M-right] 'next-buffer)

;; workgroups2 save layout to disk
(use-package workgroups2)

;; ace-window
(use-package ace-window
  :bind (("C-<tab>" . ace-window))
  :config (setq aw-scope 'frame))
;;  :bind ("M-o" . ace-window))

;; treemacs
(use-package treemacs
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag))
  )

;; Buffer navigation -----------------------------------------------------------
(use-package ibuffer
	     :bind ([f12] . ibuffer))
;; Hide some entries by default
;; https://www.emacswiki.org/emacs/IbufferMode#h5o-2
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Custom occur query ----------------------------------------------------------
(defun occur-quick-layout()
  "Show all entries with four or more dashes."
  (interactive)
  (occur "----")
  (switch-to-buffer "*Occur*")
  )

(global-set-key (kbd "C-c o") 'occur-quick-layout)

;; -----------------------------------------------------------------------------
(use-package helpful
  :bind
  (:map global-map
	("C-c C-d"   . #'helpful-at-point))
  )

;; Helm ------------------------------------------------------------------------
(use-package helm)

;; Indentation settings --------------------------------------------------------
;; Use spaces rather than tab as default
(setq-default indent-tabs-mode nil)

;; Paren settings --------------------------------------------------------------
;; https://www.emacswiki.org/emacs/ShowParenMode
(use-package paren
  :init (show-paren-mode t)
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (setq show-paren-delay 0)
  )

;; Expand region ---------------------------------------------------------------
;;; https://github.com/magnars/expand-region.el
;;; https://elpa.gnu.org/packages/expand-region.html
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Auto completion -------------------------------------------------------------
(use-package company
  :init (global-company-mode)
  )

;; Add column line -------------------------------------------------------------
; https://emacs.stackexchange.com/a/50583
(global-display-fill-column-indicator-mode 1)
(setq-default display-fill-column-indicator-column 80)

;; Auto wrap lines -------------------------------------------------------------
;; auto line wrap
;; https://www.emacswiki.org/emacs/AutoFillMode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Clipboard settings ----------------------------------------------------------
;; https://unix.stackexchange.com/a/14935/88701
(use-package xclip
  :init (xclip-mode)
  )

;; Parenthesis settings --------------------------------------------------------
; Activate electric pairs by default
(electric-pair-mode t)

(use-package smartparens
  :init (smartparens-mode)
  )

(use-package rainbow-mode
  :init (rainbow-mode)
  )

;; Flycheck --------------------------------------------------------------------
;; https://www.flycheck.org/en/latest/user/installation.html#use-package
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-global-modes '(not ess-r-mode))
  )

;; Flyspell --------------------------------------------------------------------
(use-package flyspell)
;; flyspell without a mouse
;; https://www.emacswiki.org/emacs/FlySpell#h5o-7
(global-set-key (kbd "<f8>") 'ispell-word)

;; Highlight keywords ----------------------------------------------------------
(use-package hl-todo
  :init (global-hl-todo-mode)
  :config
  (setq hl-todo-exclude-modes '(list org-mode))
  (setq hl-todo-keyword-faces
	'(("todo.*"   . "#FF0000")
	  ("fixme.*"  . "#FF0000")
	  ("TODO"   . "#FF0000")
	  ("FIXME"  . "#FF0000")
	  ("DEBUG"  . "#A020F0")
	  ("GOTCHA" . "#FF4500")
	  ("STUB"   . "#1E90FF"))
	)
  )

;; Rainbow delimiters ----------------------------------------------------------
;; (use-package rainbow-delimiters
;;   :init (require 'rainbow-delimiters)
;;   :init (rainbow-delimiters-mode))

;; Bulk editing ----------------------------------------------------------------
(use-package wgrep)

;; Search settings -------------------------------------------------------------
;; show number of matches
(use-package anzu
  :init (global-anzu-mode t))

;; Edit settings ---------------------------------------------------------------
(use-package iedit)

;; Which key -------------------------------------------------------------------
;; show keybind options
(use-package which-key
  :init (which-key-mode))

;; graphviz dot settings -------------------------------------------------------
(use-package graphviz-dot-mode)

;; Git settings ----------------------------------------------------------------
(use-package magit
  :bind(("C-x g" . magit-status))
  :init (setq magit-status-margin '(t "%d %b %y %H:%M"
  magit-log-margin-width t 24))
  :config (setq magit-log-section-commit-count 25) )

(defun madit-status() (interactive)
       (defun magit-dit-filter (env)
         "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
         (let ((home (expand-file-name "~/")))
           (let ((gitdir (expand-file-name "~/.dotfiles/")))
             (push (format "GIT_WORK_TREE=%s" home) env)
             (push (format "GIT_DIR=%s" gitdir) env)))
         env)

       (advice-add 'magit-process-environment
                   :filter-return 'magit-dit-filter)
       (magit-status)
       )

;; gitignore settings ----------------------------------------------------------
(use-package gitignore-mode)

;; Org settings ----------------------------------------------------------------
(setq org-support-shift-select 'always)

;; export html when hitting F9
(add-hook 'org-mode-hook
	  (lambda() (local-set-key (kbd "<f9>") 'org-html-export-to-html)))

;; support code block syntax highlighting in html files
(use-package htmlize)

;; fix wrong source block background
;; https://emacs.stackexchange.com/q/3374
(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

;; Markdown settings------------------------------------------------------------
;;

;; CSV settings ----------------------------------------------------------------
(use-package csv-mode
  :custom ((csv-align-style 'auto))
  :hook ((csv-mode csv-align-mode) ; Always align
	 (csv-mode csv-header-lines)) ; Always show header line
  )

;; Shell settings---------------------------------------------------------------
;; shellcheck maybe?

;; LaTeX settings --------------------------------------------------------------
;; Latex Mode https://tex.stackexchange.com/a/209509
(defun my-LaTeX-mode()
  (add-to-list 'TeX-view-program-list '("Zathura" "zathura %o"))
  (setq TeX-view-program-selection '((output-pdf "Zathura")))
  )

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; Latex preview pane
(use-package latex-preview-pane)

;; AucTeX settings -------------------------------------------------------------
;; (use-package auctex
;;   :custom ((TeX-auto-save t)
;; 	   (TeX-parse-self t)
;; 	   (TeX-master nil)
;; 	   (reftex-plug-into-auctex t)
;; 	   (font-latex-fontify-script nil))
;;   :hook ((LaTeX-mode . my-LaTeX-mode)
;; 	 (LaTeX-mode . flyspell-mode)
;; 	 (LaTeX-mode . LaTeX-math-mode)
;; 	 (LaTeX-mode . turn-on-reftex)
;; 	 (LaTeX-mode . rainbow-delimiters-mode))
;;   )

(use-package tex
  :ensure auctex
  :custom ((TeX-auto-save t)
	   (TeX-parse-self t)
	   (TeX-master nil)
	   (reftex-plug-into-auctex t)
	   (font-latex-fontify-script nil)
           (LaTeX-electric-left-right-brace t)
           )
  :hook ((LaTeX-mode . my-LaTeX-mode)
	 (LaTeX-mode . flyspell-mode)
	 (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-fold-mode)
	 (LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . rainbow-delimiters-mode))
)

;; Polymode settings -----------------------------------------------------------
;; https://emacs.stackexchange.com/q/47842
;; https://polymode.github.io/installation/
     (use-package poly-markdown
       :ensure t)

;; https://github.com/vspinu/polymode
(use-package polymode
  :diminish (poly-org-mode
             poly-markdown-mode
             poly-noweb+r-mode
             poly-noweb+r-mode
             poly-markdown+r-mode
             poly-rapport-mode
             poly-html+r-mode
             poly-brew+r-mode
             poly-r+c++-mode
             poly-c++r-mode)
;  :init
;  (require 'poly-R)
;  (require 'poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))
  )

;; ESS settings ----------------------------------------------------------------
(use-package ess
  :custom ((ess-plain-first-buffername nil)
	   (ess-ask-about-transfile nil))
  )


;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html
(defun goR()(interactive)
    (setq display-buffer-alist
	  `(
	    ("\\.R$" ; R source code window
	     (display-buffer-reuse-window display-buffer-in-direction)
	     (inhibit-same-window . nil)
	     (direction . left)
             (window-width . 0.33)
             (window-height . 1.00)
	     (dedicated . nil)
             (reusable-frames . nil))
	    ("^\\*R Dired" ; File list window
             (display-buffer-reuse-window display-buffer-in-side-window)
             (side . right)
             (window-width . 0.33)
             (window-height . 0.33)
	     (dedicated . t)
             (slot . 2)
             (reusable-frames . nil))
	    ("^magit" ; Magit
             (display-buffer-reuse-window display-buffer-in-side-window)
             (side . right)
             (window-width . 0.33)
             (window-height . 0.33)
	     (dedicated . t)
             (slot . 2)
             (reusable-frames . nil))
            ("^\\*R" ; R process window
             (display-buffer-reuse-window display-buffer-in-side-window)
             (side . right)
             (window-width . 0.33)
             (window-height . 0.33)
             (dedicated . t)
	     (slot . 0)
	     (reusable-frames . nil))
            ("^\\*Help" ; R help window
             (display-buffer-reuse-window display-buffer-in-side-window)
             (side . right)
             (window-width . 0.33)
             (window-height . 0.33)
	     (dedicated . t)
	     (slot . 1)
             (reusable-frames . nil))
	    )
	  )
    (setq ess-ask-for-ess-directory nil)

    (dolist (file (file-expand-wildcards
		   (concat default-directory "/*.R")))
      (find-file file))
;;  (let ((files (file-expand-wildcards (concat default-directory "/*.R")))) (
;;  (while files
;;    (find-file (car files))
;;    (setq files (cdr files)))))
    (R)
    (ess-rdired)
    (magit)
    )

;;Customizations by Custom -----------------------------------------------------
;; Other customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine 'luatex)
 '(TeX-view-program-list '(("Zathura" ("zathura %o") "")))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(desktop-load-locked-desktop t)
 '(desktop-restore-forces-onscreen nil t)
 '(desktop-restore-frames t)
 '(esup-depth 0)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(helpful latex-preview-pane wgrep pdf-tools sr-speedbar key-chord ox-twbs ox htmlize markdown-preview-mode ess xclip workgroups2 which-key use-package treemacs smartparens rainbow-mode poly-markdown magit iedit hl-todo helm gitignore-mode flycheck expand-region csv-mode company backup-each-save auctex anzu))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

; (add-to-list 'desktop-path default-directory)

(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
	  (lambda ()
	    (frameset-restore
	     desktop-saved-frameset
	     :reuse-frames (eq desktop-restore-reuses-frames t)
	     :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
	     :force-display desktop-restore-in-current-display
	         :force-onscreen desktop-restore-forces-onscreen)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal))))
 '(font-latex-math-face ((t (:foreground "yellow"))))
 '(font-latex-script-char-face ((t (:foreground "olive drab"))))
 '(font-latex-sectioning-5-face ((t (:foreground "#ff7983"))))
 '(font-latex-sedate-face ((t (:foreground "yellow"))))
 '(show-paren-match ((t (:background "#282a36" :foreground "#def" :underline t :weight extra-bold)))))

;  Custom functions ------------------------------------------------------------
;; Insert commented line separator
;; Credits: pjb from #emacs at libera.chat
(defun insert-commented-line-separator (label)
  "Insert a commented line separator with a custom LABEL."
  (interactive "sLabel: ")
  (insert
   (let ((text label))
     (format "%s %s %s" comment-start text
             (make-string
              (-
               (- display-fill-column-indicator-column 3) (length text)) ?-)))))

(global-set-key (kbd "C-c s") 'insert-commented-line-separator)

;; Custom theme-----------------------------------------------------------------
;; Dracula-dark (customized)
;; https://draculatheme.com/emacs/
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(load-theme 'dracula t)

;; Export to HTML with useful anchors ------------------------------------------
;; https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
(eval-when-compile
  (require 'easy-mmode)
  (require 'ox))

(use-package ox
  :ensure nil
  :config
  (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
    "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
    :global t
    (if unpackaged/org-export-html-with-useful-ids-mode
        (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
      (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

  (defun unpackaged/org-export-get-reference (datum info)
    "Like `org-export-get-reference', except uses heading titles instead of random numbers."
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
          (let* ((crossrefs (plist-get info :crossrefs))
                 (cells (org-export-search-cells datum))
                 ;; Preserve any pre-existing association between
                 ;; a search cell and a reference, i.e., when some
                 ;; previously published document referenced a location
                 ;; within current file (see
                 ;; `org-publish-resolve-external-link').
                 ;;
                 ;; However, there is no guarantee that search cells are
                 ;; unique, e.g., there might be duplicate custom ID or
                 ;; two headings with the same title in the file.
                 ;;
                 ;; As a consequence, before re-using any reference to
                 ;; an element or object, we check that it doesn't refer
                 ;; to a previous element or object.
                 (new (or (cl-some
                           (lambda (cell)
                             (let ((stored (cdr (assoc cell crossrefs))))
                               (when stored
                                 (let ((old (org-export-format-reference stored)))
                                   (and (not (assoc old cache)) stored)))))
                           cells)
                          (when (org-element-property :raw-value datum)
                            ;; Heading with a title
                            (unpackaged/org-export-new-title-reference datum cache))
                          ;; NOTE: This probably breaks some Org Export
                          ;; feature, but if it does what I need, fine.
                          (org-export-format-reference
                           (org-export-new-reference cache))))
                 (reference-string new))
            ;; Cache contains both data already associated to
            ;; a reference and in-use internal references, so as to make
            ;; unique references.
            (dolist (cell cells) (push (cons cell new) cache))
            ;; Retain a direct association between reference string and
            ;; DATUM since (1) not every object or element can be given
            ;; a search cell (2) it permits quick lookup.
            (push (cons reference-string datum) cache)
            (plist-put info :internal-references cache)
            reference-string))))

  (defun unpackaged/org-export-new-title-reference (datum cache)
    "Return new reference for DATUM that is unique in CACHE."
    (cl-macrolet ((inc-suffixf (place)
                               `(progn
                                  (string-match (rx bos
                                                    (minimal-match (group (1+ anything)))
                                                    (optional "--" (group (1+ digit)))
                                                    eos)
                                                ,place)
                                  ;; HACK: `s1' instead of a gensym.
                                  (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                             (match-string 2 ,place)))
                                          (suffix (if suffix
                                                      (string-to-number suffix)
                                                    0)))
                                    (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
      (let* ((title (org-element-property :raw-value datum))
             (ref (url-hexify-string (substring-no-properties title)))
             (parent (org-element-property :parent datum)))
        (while (--any (equal ref (car it))
                      cache)
          ;; Title not unique: make it so.
          (if parent
              ;; Append ancestor title.
              (setf title (concat (org-element-property :raw-value parent)
                                  "--" title)
                    ref (url-hexify-string (substring-no-properties title))
                    parent (org-element-property :parent parent))
            ;; No more ancestors: add and increment a number.
            (inc-suffixf ref)))
        ref))))

; Export html when hitting F9
(add-hook 'org-mode-hook
	  (lambda() (unpackaged/org-export-html-with-useful-ids-mode)))
(put 'TeX-narrow-to-group 'disabled nil)


(add-to-list 'imenu-generic-expression (list nil "-{4,}$" 2))
(put 'LaTeX-narrow-to-environment 'disabled nil)
