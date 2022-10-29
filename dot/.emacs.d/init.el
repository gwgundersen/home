;; =================================================================-*- lisp -*-
;;
;; emacs config
;;
;; =============================================================================

;; ==========
;; Miscellany
;; ==========

;; No startup screen, menu, toolbar, and scroll bar.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Ctrl-z for undo.
(global-set-key [(control z)] 'undo)
(global-unset-key [(control x)(control z)])
(global-set-key '[(control z)] 'undo)

;; Wrap lines.
(setq-default truncate-lines t)

;; Add line numbers.
(column-number-mode)
(global-display-line-numbers-mode t)

;; Show whitespace.
(setq-default show-trailing-whitespace t)
(global-set-key '[(control ?c) (?w) (?w)] 'whitespace-mode)
(global-set-key '[(control ?c) (?w) (?o)] 'whitespace-toggle-options)

;; Disable bell.
(setq ring-bell-function 'ignore)

;; Simple dark theme.
(load-theme 'wombat)

;; Auto-complete.
(global-auto-complete-mode t)

; Stop modifying init.el with customize.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Place all temp files in ~/.saves.
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)


;; ===============
;; Package manager
;; ===============

(require 'package)
(require 'use-package)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elisp")


;; ============
;; Screen width
;; ============

; Full screen on startup.
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; Fill text to 80 characters.
(setq-default fill-column 80)

; Show vertical ruler at 80 characters.
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

; Highlight characters over 80 characters.
(require 'whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style '(face lines-tail))


;; ===============
;; Vim keybindings
;; ===============

(require 'evil)
(evil-mode 1)(require 'evil)


;; ============
;; Project view
;; ============

;; Display keybinding options of incomplete command.
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0))


;; ==================
;; File / text search
;; ==================

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x);
	 ("C-x C-f" . counsel-fzf)
	 ("C-r" . counsel-rg))
  :config
  ; Don't start searches with ^.
  (setq ivy-initial-inputs-alist nil))


;; ============
;; Key bindings
;; ============

; Make ESC quit prompts.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; -----------------------------------------------------------------------------
;; Functions
;; -----------------------------------------------------------------------------

(defun gwg-hline (character)
  (while (< (current-column) (current-fill-column))
    (insert character)))

(defun gwg-hline-minus ()
  (interactive)
  (insert "# ")
  (gwg-hline "-"))

(defun gwg-hline-equals ()
  (interactive)
  (insert "# ")
  (gwg-hline "="))

(global-set-key '[(control ?c) (control ?-)] 'gwg-hline-minus)
(global-set-key '[(control ?c) (control ?=)] 'gwg-hline-equals)

(defun gwg-indent-to-4 ()
  "Move the point to the next column that is a multiple of 4."
  (interactive)
  (indent-to (let ((col (current-column))) (+ col (- 4 (% col 4))))))

(global-set-key '[(meta ?i)] 'gwg-indent-to-4)


;; -----------------------------------------------------------------------------
;; Programming languages.
;; -----------------------------------------------------------------------------

;; ========
;; Org mode
;; ========

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files   (list "~/org/")
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file)


;; ===========
;; Python mode
;; ===========

(use-package python-mode
    :ensure t
    :hook (python-mode . lsp-deferred))


; Syntax check on-the-fly.
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (global-flycheck-mode))

