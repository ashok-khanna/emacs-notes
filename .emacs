;;;; Emacs Settings of Ashok Khanna
;;;; Last updated 2 October 2021

;;; 1.0 Basic Settings

(setq default-directory "~/")                  ;; Ensure starting directory in buffer is correct
(setq command-line-default-directory "~/")     ;; Ensure starting directory in buffer is correct
(add-to-list 'exec-path' "/user/local/bin")    ;; Ensure Emacs works well with MacOS

(setq scroll-conservatively 101)               ;; Continuous scrolling
(setq inhibit-startup-screen t)                ;; Prevents default startup page from appearing
(setq org-startup-folded nil)                  ;; Prevent org-mode from auto collapsing headers
(find-file "~/scratch.org")                    ;; Opens scratch.org on startup (do it after org settings)

(require 'package)                             ;; Loads package.el (¯\_(ツ)_/¯)
(package-initialize)                           ;; Load Emacs Lisp packages, and activate them. The variable ‘package-load-list’ controls which packages to load.

;; Winner Mode to all undo/redo of Window Configurations

(winner-mode 1)                               ;; C-c <- to undo and C-c -> to re-do

;; Helps with issues with Melpa on Mac, can also try http://www.mirrorservice.org/sites/stable.melpa.org/packages/

(add-to-list 'package-archives '("melpa3" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))

;; Delete Selection Mode:  If you enable Delete Selection mode, a minor mode, then inserting text while the mark is active
;; causes the selected text to be deleted first. This also deactivates the mark. Many graphical applications follow this
;; convention, but Emacs does not.

(delete-selection-mode 1)

;; Quality of Life Key Bindings:

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-x C-u") 'undo)
(global-set-key (kbd "M-o") 'other-window)

;; Make C-x C-b take focus when run

(global-set-key [remap list-buffers] #'buffer-menu-other-window)

;; Dired Mode & Backup Settings: See also https://emacs.stackexchange.com/questions/15009/delete-files-to-trash-on-os-x

(setq delete-by-moving-to-trash t)                                       ;; Prevents Dired Mode from permanently deleting, but rather sends files to trash
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))              ;; Save Emacs backups files in the directory provided
(if (eq system-type 'darwin) (setq trash-directory "~/.Trash"))          ;; Send deleted files from Dired Mode to Trash on MacOS


;;; 2.0 Appearance

(setq-default left-fringe-width  10)               ;; And a small padding to the left of the frame
(setq-default right-fringe-width 10)               ;; Add a small padding to the right of the frame
(set-face-attribute 'fringe nil :background nil)   ;; Make background of fringes transparent
(setq-default cursor-type 'bar)                    ;; Change cursor type to a line
(blink-cursor-mode 0)                              ;; Turn off blinking on cursor
(toggle-scroll-bar -1)                             ;; Turn off scroll bars
(setq column-number-mode t)                        ;; Shows column number in buffer
(set-face-background 'mode-line "purple1")         ;; Changes color of active buffer's mode-line (to change color of inactive buffer mode-line-inactive

;; Remove Minor modes from Mode Line
;; Source: https://emacs.stackexchange.com/questions/3925/hide-list-of-minor-modes-in-mode-line
(setq mode-line-modes
      (mapcar (lambda (elem)
                (pcase elem
                  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                   "")
                  (t elem)))
              mode-line-modes))


;;; Not Used

;; (add-to-list 'default-frame-alist '(background-color . "wheat1"))
;; (set-face-attribute 'fringe nil :background "wheat1")
;; (set-frame-font "Courier New 14" nil t)

;;; 3.0 Custom Settings:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(completion-auto-help 'lazy)
 '(custom-enabled-themes '(wombat))
 '(display-time-mode t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(graphviz-dot-mode auctex-latexmk undo-tree golden-ratio-scroll-screen ox-gfm window-layout htmlize poly-org package-lint free-keys clhs git-commit magit paredit slime-company use-package))
 '(scheme-program-name "mit-scheme")
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(table-cell ((t (:background "gray75" :foreground "dark blue" :inverse-video nil)))))

;; Need to run this after custom themes load as they overwrite this:

(set-face-attribute 'fringe nil :background nil)   ;; Make background of fringes transparent
(set-face-background 'mode-line "purple1")         ;; Changes color of active buffer's mode-line (to change color of inactive buffer mode-line-inactive



;;; 4.0 Evil Mode Settings

;; Evil Mode

;; Download & Enable Evil

(unless (package-installed-p 'evil) (package-install 'evil))
(require 'evil)
(evil-mode 1)

;; Reset some default keybindings

(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "\C-w" nil)
    (define-key (eval map) "\M-w" nil)
    (define-key (eval map) "\C-y" nil)
    (define-key (eval map) "\C-x u" nil)
    (define-key (eval map) "\C-a" nil)
    (define-key (eval map) "\C-e" nil)
    (define-key (eval map) "\C-k" nil)
    (define-key (eval map) "\C-n" nil)
    (define-key (eval map) "\C-p" nil)
    (define-key (eval map) "\C-d" nil)
    (define-key (eval map) "\C-f" nil)
    (define-key (eval map) "\C-b" nil)))

;; Disabling Evil for EWW (to allow us to quit with q)

(add-to-list 'evil-emacs-state-modes 'eww-mode)

;; Some nicer cursors (?)

(setq evil-emacs-state-cursor '((bar . 1) "white") evil-normal-state-cursor '(box "orange1"))

;; Not Used -> Disabling Evil Mode for Lisp Modes

;; (add-to-list 'evil-emacs-state-modes 'lisp-mode)
;; (add-to-list 'evil-emacs-state-modes 'emacs-lisp-mode)

;;; 5.0 Lisp Settings

(setq pop-up-windows nil)                                  ;; Prevents slime from opening up in a pop-up
(setq slime-enable-evaluate-in-emacs t)                    ;; Allows CL to run Elisp code within Emacs
(load (expand-file-name "~/.quicklisp/slime-helper.el"))   ;; Something to do with Quicklisp / Slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")         ;; Sets SBCL as the inferior lisp to use
(setq slime-contribs '(slime-fancy))                       ;; Something cool I guess
(slime-setup)                                              ;; Ensures lisp modes always use SLIME

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t) ;; Load Paredit (?)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)                                   ;; Enable Paredit for Elisp Editing
(add-hook 'lisp-mode-hook             'enable-paredit-mode)                                   ;; Enable Paredit for CL Editing
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)                                   ;; Enable Paredit for Lisp Interaction Mode
(add-hook 'scheme-mode-hook           'enable-paredit-mode)                                   ;; Enable Paredit for Scheme Editing
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))                                ;; Enable Paredit for Slime REPL
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)                       ;; Enable Paredit for Minibuffer

;; Some Paredit / Slime / Lisp Related Keybindings

(global-set-key (kbd "M-T") (lambda () (interactive) (transpose-sexps -1)))
(global-set-key (kbd "M-c") (lambda () (interactive) (paredit-convolute-sexp)))
(global-set-key (kbd "M-D") (lambda () (interactive) (paredit-backward-kill-word)))
(global-set-key (kbd "C-S-k") (lambda () (interactive) (backward-kill-sexp)))
(define-key slime-mode-map (kbd "<tab>") 'slime-complete-symbol)
(define-key slime-repl-mode-map (kbd "M-s") 'paredit-splice-sexp)
(define-key slime-repl-mode-map (kbd "C-d") 'paredit-forward-delete)
(global-set-key (kbd "C-c s") #'(lambda () (interactive) (switch-to-buffer "*slime-repl sbcl*")))
(global-set-key (kbd "C-c a") '(lambda () (interactive)  (find-file "/Users/ashokkhanna/math/macrology/main.lisp")))

;; Some Evil Mode Rebindings for Paredit

;; (defmacro evil-paredit-equiv-append (function)
;;   `(progn
;;      (lambda ()
;;        (interactive)
;;        (evil-append 1)
;;        (,function)
;;        (evil-normal-state))))

;; (defmacro evil-paredit-equiv-insert (function)
;;   `(progn
;;      (lambda ()
;;        (interactive)
;;        (evil-insert 1)
;;        (,function)
;;        (evil-normal-state))))

;; (defun evil-lisp-normal-state-map ()
;;   (define-key evil-normal-state-local-map (kbd "gh") (evil-paredit-equiv-append paredit-forward))      ;; C-M-f    paredit-forward
;;      (define-key evil-normal-state-local-map (kbd "gj") (evil-paredit-equiv-insert paredit-backward)) ;; C-M-b    paredit-backward
;;      (define-key evil-normal-state-local-map (kbd "gd") 'paredit-forward-down)                  ;; C-M-d    paredit-forward-down
;;      (define-key evil-normal-state-local-map (kbd "gu") 'paredit-backward-up)                   ;; C-M-u    paredit-backward-up
;;      (define-key evil-normal-state-local-map (kbd "gn") 'paredit-forward-up)                    ;; C-M-n    paredit-forward-up
;;      (define-key evil-normal-state-local-map (kbd "gp") 'paredit-backward-down)                 ;; C-M-p    paredit-backward-down     
;;      (define-key evil-normal-state-local-map (kbd "gr") 'paredit-raise-sexp)                    ;; M-r      paredit-raise-sexp
;;      (define-key evil-normal-state-local-map (kbd "gi") 'paredit-splice-sexp)                   ;; M-s      paredit-splice-sexp
;;      (define-key evil-normal-state-local-map (kbd "g9") 'paredit-backward-slurp-sexp)           ;; C-(      paredit-backward-slurp-sexp    
;;      (define-key evil-normal-state-local-map (kbd "g0") 'paredit-forward-slurp-sexp)            ;; C-)      paredit-forward-slurp-sexp
;;      (define-key evil-normal-state-local-map (kbd "g[") 'paredit-backward-barf-sexp)            ;; C-{      paredit-backward-barf-sexp
;;      (define-key evil-normal-state-local-map (kbd "g]") 'paredit-forward-barf-sexp)             ;; C-}      paredit-forward-barf-sexp
;;      (define-key evil-normal-state-local-map (kbd "ga") 'beginning-of-defun)                    ;; C-M-a    beginning-of-defun
;;      (define-key evil-normal-state-local-map (kbd "gs") 'mark-sexp))                            ;; C-M-SPC  mark-sexp


;; (add-hook 'emacs-lisp-mode-hook 'evil-lisp-normal-state-map)
;; (add-hook 'lisp-mode-hook 'evil-lisp-normal-state-map)
;; (add-hook 'lisp-interaction-mode-hook 'evil-lisp-normal-state-map)
;; (add-hook 'scheme-mode-hook 'evil-lisp-norqmal-state-map)
;; (add-hook 'slime-repl-mode-hook 'evil-lisp-normal-state-map)

;; Available VIM keybindings
;; Courtesy: https://gist.github.com/romainl/1f93db9dc976ba851bbb

;; cd cm co cp cq cr cs cu cv cx cy cz
;; dc dm dq dr ds du dv dx dy dz
;; gb gc gl gs gy
;; vc vd vm vo vp vq vr vs vu vv vx vy vz
;; yc yd ym yo yp yq yr ys yu yv yx yz
;; zq


;; Enable Evil Paredit (Ensure Evil & Paredit Modes coexist nicely with each other)

(load "/Users/ashokkhanna/.emacs.d/evil-paredit.el")
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-mode-hook 'evil-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
(add-hook 'scheme-mode-hook 'evil-paredit-mode)
(add-hook 'slime-repl-mode-hook 'evil-paredit-mode)

;; Load Autologger (Elisp)

(load "/Users/ashokkhanna/autologger/el-autologger.el")

;; Load Math Mode (Elisp)

(load "~/math/math-mode.el")

;; Allow for local loading of HyperSpec in Emacs via EWW

(setq browse-url-browser-function 'eww-browse-url)
(setq common-lisp-hyperspec-root (concat "file://" (expand-file-name "~/lisp-notes/Resources/HyperSpec-7-0 3/HyperSpec/")))

;; Scheme Settings

(require 'xscheme)
(setq scheme-program-name "/usr/local/Cellar/mit-scheme/11.2/bin/mit-scheme -library /usr/local/Cellar/mit-scheme/11.2/lib/mit-scheme-x86-64-11.2")

;; Eval last Slime REPL command again

(global-set-key (kbd "C-c p") (lambda ()
				(interactive)
				(let ((command (format "%s"
						       (with-current-buffer "*slime-repl sbcl*"
							 (car slime-repl-input-history)))))
				  (slime-interactive-eval command))))



;;; 6.0 Org Mode Settings

(add-hook 'org-mode-hook 'poly-org-mode)                                   ;; Poly Mode allows for Lisp Indenting / Formatting in Babel
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))     ;; Enable LaTex for Org Babel
(org-babel-do-load-languages 'org-babel-load-languages '((lisp . t)))      ;; Enable Lisp for Org Babel

;; To insert zero width spaces:

(global-set-key (kbd "C-c z") (lambda () (interactive) (insert-char ?\u200b)))

;; Shortcuts to auto add lisp code blocks:

(defun lisp-mode-code-block ()
  (interactive)
  (org-insert-structure-template "src")
  (insert "lisp")
  (newline 3)
  (goto-char (line-beginning-position 0)))

(defun lisp-mode-code-block-region ()
  (interactive)
  (save-excursion
    (let* ((beg (region-beginning))
	   (end (region-end))
	   (code-test (buffer-substring-no-properties beg end)))
      (kill-region beg end)
      (org-insert-structure-template "src")
      (insert "lisp")
      (newline 2)
      (insert code-test)
      (newline))))

(global-set-key (kbd "C-c l") 'lisp-mode-code-block)
(global-set-key (kbd "C-c r") 'lisp-mode-code-block-region)

;; Graphviz Configurations:

(use-package graphviz-dot-mode :ensure t :config (setq graphviz-dot-indent-width 4))
(use-package company-graphviz-dot)
(setq graphviz-dot-dot-program "/usr/local/Cellar/graphviz/2.48.0/bin/dot")

;; Latex Configurations

;; Refer https://www.reddit.com/r/emacs/comments/fk72zv/org_export_pdf_pdflatex_command_not_found/

(load "auctex.el" nil t t)
(setq org-latex-pdf-process
      '("/Library/TeX/texbin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "/Library/TeX/texbin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "/Library/TeX/texbin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; To extract all code blocks from org files into their own

(setq org-babel-default-header-args '((:tangle . "yes")))   

;; Org Publish Settings

(require 'ox-publish)

(setq org-src-fontify-natively t)

(defun my-org-html-postamble (plist)
 (format "Last update : %s" (format-time-string "%d %b %Y")))
(setq org-html-postamble 'my-org-html-postamble)

(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/web/org/"
         :base-extension "org"
         :publishing-directory "~/web/"
         :publishing-function org-html-publish-to-html
         :exclude "PrivatePage.org" ;; regexp
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
	 :html-doctype "html5"
	 :html-link-home ""
	 :html-link-up ""
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :html5-fancy t
	 :recursive t
         :html-head " <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\" />"
         :html-preamble t
	 :html-preamble-format (("en" "<div id=\"quote\">Ashok Khanna</div>
<div id=\"home-link\"><a href=\"/index.html\">Home</a></div>
<div id=\"articles-link\"><a href=\"/articles.html\">Articles</a></div>
<div id=\"resources-link\"><a href=\"/resources.html\">Resources</a></div>"))
	 :html-postamble-format (("en" "<p class=\"Date\">Last updated: %d</p><script src=\"/scripts/main.js\"></script>"))
	 :html-postamble t)
	("org-static"
       :base-directory "~/web/org/"
       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
       :publishing-directory "~/web/"
       :recursive t
       :publishing-function org-publish-attachment) 
        ("org" :components ("org-notes" "org-static"))))

;; M-x org-publish-project org
;; C-u M-x org-publish-project org (force republish all)
;; Always check the above settings in case you have wrong directory, etc.
;; For more settings - refer to https://orgmode.org/manual/Publishing-options.html#Publishing-options

;;; 7.0 Math Hydras

(require 'hydra)

;; Hydra - Lower Case

(defhydra math-lowercase (:color blue :columns 6)
  "Select your symbol below (also try C-c n)"
  ("a" (lambda () (interactive) (insert "α")) "α")
  ("b" (lambda () (interactive) (insert "β")) "β")
  ("c" (lambda () (interactive) (insert "∊")) "∊")
  ("d" (lambda () (interactive) (insert "δ")) "δ")
  ("e" (lambda () (interactive) (insert "ε")) "ε")
  ("f" (lambda () (interactive) (insert "φ")) "φ")
  ("g" (lambda () (interactive) (insert "γ")) "γ")
  ("h" (lambda () (interactive) (insert "θ")) "θ")
  ("i" (lambda () (interactive) (insert "ι")) "ι")
  ("j" (lambda () (interactive) (insert "ξ")) "ξ")
  ("k" (lambda () (interactive) (insert "κ")) "κ")
  ("l" (lambda () (interactive) (insert "λ")) "λ")
  ("m" (lambda () (interactive) (insert "μ")) "μ")
  ("n" (lambda () (interactive) (insert "η")) "η")
  ("o" (lambda () (interactive) (insert "ο")) "ο")
  ("p" (lambda () (interactive) (insert "π")) "π")
  ("r" (lambda () (interactive) (insert "ρ")) "ρ")
  ("s" (lambda () (interactive) (insert "σ")) "σ")
  ("t" (lambda () (interactive) (insert "τ")) "τ")
  ("u" (lambda () (interactive) (insert "υ")) "υ")
  ("v" (lambda () (interactive) (insert "ν")) "ν")
  ("w" (lambda () (interactive) (insert "ω")) "ω")
  ("x" (lambda () (interactive) (insert "χ")) "χ")
  ("y" (lambda () (interactive) (insert "ψ")) "ψ")
  ("z" (lambda () (interactive) (insert "ζ")) "ζ")
  ("," (lambda () (interactive) (insert "≤")) "≤")
  ("." (lambda () (interactive) (insert "≥")) "≥")
  ("=" (lambda () (interactive) (insert "≠")) "≠")
  ("-" (lambda () (interactive) (insert "±")) "±")
  ("0" (lambda () (interactive) (insert "∅")) "∅")
  ("1" (lambda () (interactive) (insert "→")) "→")
  ("2" (lambda () (interactive) (insert "↔")) "↔")
  ("3" (lambda () (interactive) (insert "↦")) "↦")
  ("4" (lambda () (interactive) (insert "↑")) "↑")
  ("5" (lambda () (interactive) (insert "↓")) "↓")
  ("6" (lambda () (interactive) (insert "↗")) "↗")
  ("7" (lambda () (interactive) (insert "↘")) "↘")
  ("8" (lambda () (interactive) (insert "∞")) "∞")
  ("9" (lambda () (interactive) (insert "⋯")) "⋯")
  ("q" nil "cancel"))

;; Hydra - Upper Case

(defhydra math-uppercase (:color blue :columns 6)
  "Select your symbol below (also try C-c m)"
  ("a" (lambda () (interactive) (insert "∀")) "∀")
  ("b" (lambda () (interactive) (insert "∃")) "∃")
  ("c" (lambda () (interactive) (insert "ℂ")) "ℂ")
  ("d" (lambda () (interactive) (insert "Δ")) "Δ")
  ("e" (lambda () (interactive) (insert "∈")) "∈")
  ("f" (lambda () (interactive) (insert "Φ")) "Φ")
  ("g" (lambda () (interactive) (insert "Γ")) "Γ")
  ("h" (lambda () (interactive) (insert "Θ")) "Θ")
  ("i" (lambda () (interactive) (insert "∫")) "∫")
  ("j" (lambda () (interactive) (insert "∂")) "∂")
  ("k" (lambda () (interactive) (insert "⊢")) "⊢")
  ("l" (lambda () (interactive) (insert "Λ")) "Λ")
  ("m" (lambda () (interactive) (insert "∄")) "∄")
  ("n" (lambda () (interactive) (insert "ℕ")) "ℕ")
  ("o" (lambda () (interactive) (insert "⊕")) "⊕")
  ("p" (lambda () (interactive) (insert "Π")) "Π")
  ("r" (lambda () (interactive) (insert "ℝ")) "ℝ")
  ("s" (lambda () (interactive) (insert "Σ")) "Σ")
  ("t" (lambda () (interactive) (insert "∴")) "∴")
  ("u" (lambda () (interactive) (insert "∵")) "∵")
  ("v" (lambda () (interactive) (insert "√")) "√")
  ("w" (lambda () (interactive) (insert "Ω")) "Ω")
  ("x" (lambda () (interactive) (insert "∊")) "∊")
  ("y" (lambda () (interactive) (insert "Ψ")) "Ψ")
  ("z" (lambda () (interactive) (insert "ℤ")) "ℤ")
  ("," (lambda () (interactive) (insert "¬")) "¬")
  ("." (lambda () (interactive) (insert "≡")) "≡")
  ("=" (lambda () (interactive) (insert "≈")) "≈")
  ("-" (lambda () (interactive) (insert "≠")) "≠")
  ("0" (lambda () (interactive) (insert "∉")) "∉")
  ("1" (lambda () (interactive) (insert "ℚ")) "ℚ")
  ("2" (lambda () (interactive) (insert "⊂")) "⊂")
  ("3" (lambda () (interactive) (insert "⊃")) "⊃")
  ("4" (lambda () (interactive) (insert "⋂")) "⋂")
  ("5" (lambda () (interactive) (insert "⋃")) "⋃")
  ("6" (lambda () (interactive) (insert "∧")) "∧")
  ("7" (lambda () (interactive) (insert "∨")) "∨")
  ("8" (lambda () (interactive) (insert "∙")) "∙")
  ("9" (lambda () (interactive) (insert "∘")) "∘")
  ("q" nil "cancel"))

;; Key Maps

(global-set-key (kbd "C-c m") #'math-lowercase/body)
(global-set-key (kbd "C-c n") #'math-uppercase/body)
(global-set-key (kbd "M-p") #'math-lowercase/body)
(global-set-key (kbd "M-n") #'math-uppercase/body)

;;; 8.0 Other Settings

;; ERC Settings

(require 'erc)
(require 'tls)
(defun start-erc () "Connect to IRC." (interactive)
       (erc-tls :server "irc.libera.chat" :port 6697 :nick "lisp123" :full-name "Your Name"))
(setq  erc-hide-list '("JOIN" "PART" "QUIT"))

(global-set-key (kbd "C-c e") '(lambda () (interactive) (start-erc)))


;; 9.0 Experimenting with Evil States

;; EVIL / Paredit Bindings

(evil-define-state paredit
  "Paredit state."
  :tag " <P> "
  :enable (normal))


;; Escape & Lisp States
(define-key evil-paredit-state-map [escape] (lambda () (interactive) (evil-normal-state)))
(define-key evil-paredit-state-map (kbd "SPC") (lambda () (interactive) (evil-normal-state)))
(define-key evil-normal-state-map (kbd "SPC") (lambda () (interactive) (evil-paredit-state)))
(define-key evil-paredit-state-map "i" (lambda () (interactive) (evil-insert-state)))

;; Normal Keys - Not needed as taking from normal mode above via enable
;; (define-key evil-paredit-state-map "h" 'evil-backward-char)
;; (define-key evil-paredit-state-map "j" 'evil-next-visual-line)
;; (define-key evil-paredit-state-map "k" 'evil-previous-visual-line)
;; (define-key evil-paredit-state-map "l" 'evil-forward-char)

;; Lisp Keys

(define-key evil-paredit-state-map "w" 'paredit-forward)                       ;; C-M-f    paredit-forward
(define-key evil-paredit-state-map "b" 'paredit-backward)                      ;; C-M-b    paredit-backward
(define-key evil-paredit-state-map "d" 'paredit-forward-down)                  ;; C-M-d    paredit-forward-down
(define-key evil-paredit-state-map "f" 'paredit-backward-up)                   ;; C-M-u    paredit-backward-up
(define-key evil-paredit-state-map "n" 'paredit-forward-up)                    ;; C-M-n    paredit-forward-up
(define-key evil-paredit-state-map "t" 'paredit-backward-down)                 ;; C-M-p    paredit-backward-down     
(define-key evil-paredit-state-map "r" 'paredit-raise-sexp)                    ;; M-r      paredit-raise-sexp
(define-key evil-paredit-state-map "t" 'paredit-splice-sexp)                   ;; M-s      paredit-splice-sexp
(define-key evil-paredit-state-map "(" 'paredit-backward-slurp-sexp)           ;; C-(      paredit-backward-slurp-sexp    
(define-key evil-paredit-state-map ")" 'paredit-forward-slurp-sexp)            ;; C-)      paredit-forward-slurp-sexp
(define-key evil-paredit-state-map "{" 'paredit-backward-barf-sexp)            ;; C-{      paredit-backward-barf-sexp
(define-key evil-paredit-state-map "}" 'paredit-forward-barf-sexp)             ;; C-}      paredit-forward-barf-sexp
(define-key evil-paredit-state-map "a" 'beginning-of-defun)                    ;; C-M-a    beginning-of-defun
(define-key evil-paredit-state-map "s" 'mark-sexp)                             ;; C-M-SPC  mark-sexp
