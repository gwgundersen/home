;; =========================================================-*- lisp -*-
;; Emacs config.
;; =====================================================================

;; ===============
;; Package manager
;; ===============

(require 'package)
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elisp")

;; ===================
;; Project / tree view
;; ===================

;; ==================
;; File / text search
;; ==================

(require 'find-file-in-project)
(require 'helm-ag)

;; ===========
;; Python mode
;; ===========

(use-package company :ensure t :pin melpa)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(load "elpy")
(load "elpy-rpc")
(load "elpy-shell")
(load "elpy-profile")
(load "elpy-refactor")
(load "elpy-django")

;; ========
;; Org mode
;; ========

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-log-done 'time
      org-agenda-files   (list "~/org/")
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      )

;; ======
;; Custom
;; ======

; https://stackoverflow.com/a/5058752/1830334
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; ===============
;; Temporary files
;; ===============

; https://stackoverflow.com/a/151946/1830334
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; ====
;; Math
;; ====

;; ============
;; Key bindings
;; ============

;; ======
;; Colors
;; ======

(load-theme 'spacemacs-dark t)
