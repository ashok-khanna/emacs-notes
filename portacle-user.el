;;;; Portacle Emacs Settings of Ashok Khanna
;;;; Last updated 26 September 2021

;;; 1.0 Basic Settings

(setq scroll-conservatively 101)               ;; Continuous scrolling
(setq org-startup-folded nil)                  ;; Prevent org-mode from auto collapsing headers
(delete-selection-mode 1)                      ;; More normal behaviour when replacing text on active region

;; Quality of Life Key Bindings:

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-x C-u") 'undo)
(global-set-key (kbd "M-o") 'other-window)

;; Make C-x C-b take focus when run

(global-set-key [remap list-buffers] #'buffer-menu-other-window)

;;; 2.0 Appearance

(setq-default left-fringe-width  10)               ;; And a small padding to the left of the frame
(setq-default right-fringe-width 10)               ;; Add a small padding to the right of the frame
(set-face-attribute 'fringe nil :background nil)   ;; Make background of fringes transparent
(setq-default cursor-type 'bar)                    ;; Change cursor type to a line
(blink-cursor-mode 0)                              ;; Turn off blinking on cursor
(toggle-scroll-bar -1)                             ;; Turn off scroll bars
(setq column-number-mode t)                        ;; Shows column number in buffer

;; 3.0 Custom Settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
  '(linum-format " %7i ")
 '(objed-cursor-color "#99324b")
 '(org-loop-over-headlines-in-active-region t)
 '(package-selected-packages
   (quote
    (evil-tutor ## evil ag expand-region multiple-cursors magit slime-company company-quickhelp company paredit slime helpful)))
 '(pdf-view-midnight-colors (cons "#2a2a2a" "#fafafa"))
 '(portacle-project-licence "BSD-3")
 '(portacle-setup-done-p t)
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil)
 '(window-divider-default-places t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Roboto Mono" :foundry "outline" :slant normal :weight normal :height 90 :width normal))))
 '(org-headline-done ((t (:foreground "black")))))

(add-to-list 'default-frame-alist '(font . "Courier New 10"))
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq org-cycle-separator-lines -1)
;; (global-font-lock-mode 0)
(add-hook 'org-mode-hook 'font-lock-mode)
(define-key paredit-mode-map (kbd "C--") 'paredit-forward-slurp-sexp)
(find-file "c:/users/akhanna/Desktop/EMACS/master-file.org")

;;; 4.0 Evil Mode Settings

;; Download & Enable Evil

(unless (package-installed-p 'evil)(package-install 'evil))
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

(setq evil-emacs-state-cursor '((bar . 1) "black") evil-normal-state-cursor '(box "black"))

;;; 5.0 Lisp Settings

(setq pop-up-windows nil)                                  ;; Prevents slime from opening up in a pop-up
(setq slime-enable-evaluate-in-emacs t)                    ;; Allows CL to run Elisp code within Emacs
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

;; Allow for local loading of HyperSpec in Emacs via EWW

(setq browse-url-browser-function 'eww-browse-url)
(setq common-lisp-hyperspec-root (concat "file://" (expand-file-name "c:/users/akhanna/Desktop/EMACS/clhs-main/HyperSpec/")))

;; 6.0 Settings to remove portacle settings

(global-company-mode 0)
(company-quickhelp-mode 0)
(setq slime-contribs (remove 'slime-company slime-contribs))
(show-paren-mode 0)
(ido-everywhere -1)
(doom-modeline-mode -1)
(global-semanticdb-minor-mode -1)
(semantic-mode -1)
(global-semantic-idle-scheduler-mode -1)
(flyspell-mode -1)
(display-line-numbers-mode -1)
(diff-auto-refine-mode -1)
(global-display-line-numbers-mode -1)
(sly-symbol-completion-mode -1)
(ido-mode -1)

;; Enable Evil Paredit (Ensure Evil & Paredit Modes coexist nicely with each other)

;; Begin Evil Paredit

;;; evil-paredit.el --- Paredit support for evil keybindings
;;
;; Copyright (C) 2012 Roman Gonzalez
;;
;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Mantainer: Roman Gonzalez <romanandreg@gmail.com>
;; Keywords: paredit, evil
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;; Version: 0.0.2

;; URL: https://github.com/roman/evil-paredit

;; Package-Requires: ((evil "1.0.9") (paredit "25beta"))

;;; Code:

(require 'evil)
(require 'paredit)

;;;###autoload

(define-minor-mode evil-paredit-mode
  "Minor mode for setting up Evil with paredit in a single buffer"
  :keymap '()
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)))

(defun -evil-paredit-check-region (beg end)
  (if (fboundp 'paredit-check-region-state)
      (if (and beg end)
          ;; Check that region begins and ends in a sufficiently similar
          ;; state, so that deleting it will leave the buffer balanced.
          (save-excursion
            (goto-char beg)
            (let* ((state (paredit-current-parse-state))
                   (state* (parse-partial-sexp beg end nil nil state)))
              (paredit-check-region-state state state*))))
    (paredit-check-region-for-delete beg end)))

(evil-define-operator evil-paredit-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (-evil-paredit-check-region beg end)
  (cond
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(evil-define-operator evil-paredit-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-yank beg end type register)))

(evil-define-operator evil-paredit-delete
  (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (evil-paredit-yank beg end type register yank-handler)
  (if (eq type 'block)
      (evil-apply-on-block #'delete-region beg end nil)
    (delete-region beg end))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evil-paredit-delete-line
  (beg end type register yank-handler)
  "Delete to end of line respecting parenthesis."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-delete beg end
                         type register yank-handler)))

(defun evil-paredit-kill-end ()
  "Returns the position where paredit-kill would kill to"
  (when (paredit-in-char-p)             ; Move past the \ and prefix.
    (backward-char 2))                  ; (# in Scheme/CL, ? in elisp)
  (let* ((eol (point-at-eol))
         (end-of-list-p (save-excursion
                          (paredit-forward-sexps-to-kill (point) eol))))
    (if end-of-list-p (progn (up-list) (backward-char)))
    (cond ((paredit-in-string-p)
           (if (save-excursion (paredit-skip-whitespace t (point-at-eol))
                               (eolp))
               (kill-line)
             (save-excursion
               ;; Be careful not to split an escape sequence.
               (if (paredit-in-string-escape-p)
                   (backward-char))
               (min (point-at-eol)
                    (cdr (paredit-string-start+end-points))))))
          ((paredit-in-comment-p)
           eol)
          (t (if (and (not end-of-list-p)
                      (eq (point-at-eol) eol))
                 eol
               (point))))))

(evil-define-operator evil-paredit-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'evil-paredit-delete))
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg)))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (evil-open-above 1))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator evil-paredit-change-line
  (beg end type register yank-handler)
  "Change to end of line respecting parenthesis."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-change beg end type register yank-handler)))

(defun evil-paredit-change-whole-line ()
  "Change whole line."
  (interactive)
  (beginning-of-line)
  (evil-paredit-change-line nil nil)
  (indent-according-to-mode))

(evil-define-key 'normal evil-paredit-mode-map
  (kbd "d") 'evil-paredit-delete
  (kbd "c") 'evil-paredit-change
  (kbd "y") 'evil-paredit-yank
  (kbd "D") 'evil-paredit-delete-line
  (kbd "C") 'evil-paredit-change-line
  (kbd "S") 'evil-paredit-change-whole-line
  (kbd "Y") 'evil-paredit-yank-line
  (kbd "X") 'paredit-backward-delete
  (kbd "x") 'paredit-forward-delete
  ;; (kbd "f") 'paredit-forward
  ;; (kbd "b") 'paredit-backward
  ;; (kbd "u") 'paredit-backward-up
  ;; (kbd "d") 'paredit-forward-down
  ;; (kbd "n") 'paredit-forward-up
  ;; (kbd "p") 'paredit-backward-up
  ;; (kbd "s") 'paredit-split-sexp
  ;; (kbd "j") 'paredit-join-sexps
  ;; (kbd ";") 'paredit-comment-dwim
  ;; (kbd "r") 'paredit-raise
  )

(provide 'evil-paredit)

;;; evil-paredit.el ends here

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)





 

