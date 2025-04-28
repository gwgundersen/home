;; =================================================================-*- lisp -*-
;;
;; emacs config
;;
;; =============================================================================

;; ==================
;; Package management
;; ==================

(require 'package)
(require 'use-package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elisp")


;; ==============
;; Coding systems
;; ==============

;; Some programs like Flycheck run external shell commands and may misparse the
;; output without the correct encodings
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)


;; ==========
;; Navigation
;; ==========

;; Fuzzy find commands, files, and options in the minibuffer.
(use-package ivy
  :diminish
  :bind ("C-s" . swiper)
  :config
  (ivy-mode 1))

;; Enhanced versions of common Emacs commands (M-x, find-file, etc) using Ivy.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-f" . counsel-fzf)
	 ("C-r" . counsel-rg))
  :config
  ; Don't start searches with ^.
  (setq ivy-initial-inputs-alist nil))

;; Show available keybindings as you type.
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Inline autocompletion inside buffers (code, text, etc).
(use-package company
    :ensure t
    :config
    (global-company-mode t))


;; ============
;; Theme, fonts
;; ============

(use-package doom-themes
  :init (load-theme 'doom-one t))

(set-face-attribute 'default nil :family "monospace" :height 145)


;; ============
;; Screen width
;; ============

; Full screen on startup.
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; Fill text to 100 characters.
(setq-default fill-column 100)

; Show vertical ruler at 1o0 characters.
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook 'whitespace-mode)

; Highlight characters over 100 characters.
(require 'whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style '(face lines-tail))


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
  (when (file-directory-p "~/base")
    (setq projectile-project-search-path '("~/base")))
  (when (file-directory-p "~/org")
    (setq projectile-project-search-path '("~/org")))
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))


;; ======
;; Keymap
;; ======

(global-set-key '[(control ?.)] 'shell-command)


;; =============
;; Git porcelain
;; =============

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; Sort branches by recency, not alphabetically.
  (magit-list-refs-sortby "-creatordate"))


;; ==========
;; Miscellany
;; ==========

;; Keep cursor at same vertical position while scrolling.
(setq scroll-preserve-screen-position t)

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
(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode)))

;; Don't just list buffers; offer to switch.
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)


;; =========
;; Functions
;; =========

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


;; ================
;; Language servers
;; ================

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))


;; ======
;; Python
;; ======

(use-package flycheck
  :ensure t
  :hook (python-mode . flycheck-mode)
  :init
  (setq flycheck-checker 'python-flake8)
  (setq flycheck-temp-prefix ".flycheck")
  :config
  (setq-default flycheck-disabled-checkers '(python-mypy))
  (set-face-attribute 'flycheck-warning nil
                      :foreground "yellow"
                      :weight 'bold)
  (set-face-attribute 'flycheck-error nil
                      :foreground "red"
                      :weight 'bold))

(defun resize-flycheck-error-buffer ()
  "Resize the Flycheck error buffer to a fixed height."
  (let ((desired-height 20))  ; Set your desired buffer height here
    (with-current-buffer flycheck-error-list-buffer
      (let ((window (get-buffer-window)))
        (if window
            (progn
              (message "Resizing Flycheck error buffer to %d lines." desired-height)
              (window-resize window (- desired-height (window-total-height window))))
          (message "Flycheck error buffer window not found."))))))

(add-hook 'flycheck-error-list-after-refresh-hook #'resize-flycheck-error-buffer)


;; ========
;; Spelling
;; ========

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

(setq ispell-program-name "aspell")

;; Enable live spell checking in text-mode
(add-hook 'text-mode-hook 'flyspell-mode)


;; ===
;; Org
;; ===

(use-package org
  :defer t
  :custom
  (org-agenda-files (list "~/org/"))
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path 'file)
  ;; No forced indentation
  (org-adapt-indentation nil)
  ;; Allow selection by holding shift.
  (org-support-shift-select t)
  :hook
  ;; Word wrap in org mode.
  (org-mode . gwg-word-wrap))

(use-package org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))


;; ========
;; Markdown
;; ========

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode-hook . gwg-word-wrap))


;; ====
;; LLMs
;; ====

(use-package gptel
  :ensure t
  :bind
  ("C-c g s" . gptel-send)
  ("C-c g m" . gptel-menu)
  :config
  (setq gptel-api-key
        (string-trim
          (with-temp-buffer
            (insert-file-contents "~/.gptel-openai-api-key")
            (buffer-string)))))

(defun gwg-gptel-backend-and-model ()
  "Return gptel backend and model (if any)."
  (let ((backend (if  (boundp 'gptel-backend)  (aref gptel-backend 1)))
        (model (if  (boundp 'gptel-model) gptel-model)))
    (format "(%s %s)" backend model)))
