;; =================================================================-*- lisp -*-
;;
;; emacs config
;;
;; =============================================================================

(set-frame-parameter nil 'alpha-background 85) ; Adjust the value (0-100) as desired
(add-to-list 'default-frame-alist '(alpha-background . 55))

(defvar efs/frame-transparency '(90 . 90))

;; ==========
;; Miscellany
;; ==========

;; No startup screen, menu, toolbar, and scroll bar.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Wrap lines.
(setq-default truncate-lines t)

;; Show whitespace.
(setq-default show-trailing-whitespace t)
(global-set-key '[(control ?c) (?w) (?w)] 'whitespace-mode)
(global-set-key '[(control ?c) (?w) (?o)] 'whitespace-toggle-options)

;; Disable bell.
(setq ring-bell-function 'ignore)

;; Add line numbers.
(column-number-mode)
(global-display-line-numbers-mode t)

;; Auto-complete.
(global-auto-complete-mode t)

;; Stop customize from adding to this config.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Place all temp files in ~/.saves.
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
;; Prevent Emacs from asking if we want to delete old files.
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Stop auto-indenting.
(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; Edit multiple occurrences in the same way.
(require 'iedit)


;; ==================
;; Package management
;; ==================

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
(add-hook 'org-mode-hook 'whitespace-mode)

; Highlight characters over 80 characters.
(require 'whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style '(face lines-tail))


;; ============
;; Theme, fonts
;; ============

(use-package doom-themes
  :init (load-theme 'doom-one t))

;; Set fonts.
(defvar gwg-font-size 160)
(set-face-attribute 'default nil
  :font "iosevka"
  :height gwg-font-size)
(set-face-attribute 'fixed-pitch nil
  :font "iosevka"
  :height gwg-font-size)


;; ==================
;; Project management
;; ==================

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (when (file-directory-p "~/org")
    (setq projectile-project-search-path '("~/org")))
  (setq projectile-switch-project-action #'projectile-dired))


;; ===============
;; Text completion
;; ===============

;; Display keybinding options of while typing.
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind ("C-s" . swiper)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x);
	 ("C-f" . counsel-fzf)
	 ("C-r" . counsel-rg))
  :config
  ; Don't start searches with ^.
  (setq ivy-initial-inputs-alist nil))


;; ============
;; Key bindings
;; ============

;; Make ESC quit prompts.
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Don't just list buffers; offer to switch.
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)


;; ===============
;; Git integration
;; ===============



;; ===============
;; Windows
;; ===============

(setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "C-2") 'winum-select-window-by-number)
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))

(require 'winum)

(winum-mode)


;; -----------------------------------------------------------------------------
;; Functions
;; -----------------------------------------------------------------------------

(defun gwg-org-col ()
  (interactive)
  (execute-kbd-macro (read-kbd-macro "C-u C-c C-q")))

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

(defun gwg-word-wrap ()
  (auto-fill-mode 1)
  (set-fill-column 80)
  (visual-line-mode 1))

(global-set-key '[(meta ?w)] 'gwg-word-wrap)


;; -----------------------------------------------------------------------------
;; Language modes
;; -----------------------------------------------------------------------------
;; ================
;; Language servers
;; ================

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config (lsp-enable-which-key-integration t))


;; ===========
;; Python mode
;; ===========

(use-package python-mode
    :ensure t
    :hook (python-mode . lsp-deferred))

;; Check syntax on the fly.
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (global-flycheck-mode))

;; Check spelling.
(use-package flyspell
  :config
  (set-face-attribute
   'flyspell-incorrect nil
   :foreground nil
   :background "#401"
   :underline nil)
  (set-face-attribute
   'flyspell-duplicate nil
   :foreground nil
   :background "#280008"
   :underline nil))


;; ========
;; Org mode
;; ========

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files   (list "~/org/")
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file)

;; Word wrap in org mode.
(add-hook 'org-mode-hook 'gwg-word-wrap)

;; No forced indentation
(setq org-adapt-indentation nil)

;; Allow selection by holding shift.
(setq org-support-shift-select t)

;; Citations.
(use-package oc
  :custom
  (org-cite-global-bibliography '("/Users/gwg/org/references.bib")))

;; Spell check.
(setq ispell-program-name "aspell")

;; =============
;; Markdown mode
;; =============

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(add-hook 'markdown-mode-hook 'gwg-word-wrap)
