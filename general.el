(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; (use-package auto-package-update
;;    :ensure t
;;    :config
;;    (setq auto-package-update-delete-old-versions t
;;          auto-package-update-interval 4)
;;    (auto-package-update-maybe))

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;
;;; VISUALS ;;;
;;;;;;;;;;;;;;;

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(global-auto-revert-mode 1)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  ;; (set-face-background hl-line "gray13")
  (global-hl-line-mode +1)
  )

(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode t)
  )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-vcs-max-length 20)
  )

(setq echo-keystrokes 0.5)

(custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 106 :width normal :family "Ubuntu Mono")))))

;; (global-prettify-symbols-mode t)
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (push '("<=" . ?≤) prettify-symbols-alist)
;;             (push '("->" . ?→) prettify-symbols-alist)
;;             (push '("<->" . ?↔) prettify-symbols-alist)
;;             (push '("->>" . ?↠) prettify-symbols-alist)
;;             (push '("=>" . ?⇒) prettify-symbols-alist)
;;             (push '("map" . ?↦) prettify-symbols-alist)
;;             (push '("/=" . ?≠) prettify-symbols-alist)
;;             (push '("!=" . ?≠) prettify-symbols-alist)
;;             (push '("==" . ?≡) prettify-symbols-alist)
;;             (push '("<=" . ?≤) prettify-symbols-alist)
;;             (push '(">=" . ?≥) prettify-symbols-alist)
;;             (push '("<=<" . ?↢) prettify-symbols-alist)
;;             (push '(">=>" . ?↣) prettify-symbols-alist)
;;             (push '("&&" . ?∧) prettify-symbols-alist)
;;             (push '("||" . ?∨) prettify-symbols-alist)
;;             (push '("not" . ?¬) prettify-symbols-alist)
;;             (push '("<=" . ?≤) prettify-symbols-alist)
;;             (push '("+-" . ?±) prettify-symbols-alist)
;;             (push '("sum" . ?∑) prettify-symbols-alist)
;;             (push '("all" . ?∀) prettify-symbols-alist)
;;             (push '("any" . ?∃) prettify-symbols-alist)
;;             (push '("def" . ?▷) prettify-symbols-alist)
;;             (push '("class" . ?◼) prettify-symbols-alist)
;;             (push '("in" . ?∈) prettify-symbols-alist)
;;             (push '("set()" . ?∅) prettify-symbols-alist)
;;             ;; (push '(" in" . (? (Br . Bl) ?∈)) prettify-symbols-alist)
;;             ))


(use-package highlight-symbol
  :ensure t
  :bind
  ("C-<f5>" . highlight-symbol)
  ("<f5>" . highlight-symbol-next)
  ("S-<f5>" . highlight-symbol-prev)
  ("M-<f5>" . highlight-symbol-query-replace)
  )

(use-package nav-flash
  :ensure t
  :config
  (nav-flash-show))

;; (use-package visual-regexp
;;   :defer t)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-solarized-dark t)
;;   )

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t)  ;; check
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :ensure t)

;; (add-to-list 'load-path "~/.emacs.d/tabbar/")

(setq awesome-tab-background-color "#002B36")
(setq awesome-tab-style "bar")
(add-to-list 'load-path "~/.emacs.d/awesome-tab/")
;; (load "tabbar")
(require 'awesome-tab)
(awesome-tab-mode t)
(global-set-key (kbd "<C-tab>") 'awesome-tab-forward-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'awesome-tab-backward-tab)

;; (use-package centaur-tabs
;;   :demand
;;   :ensure t
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-style 'bar)
;;   ;; (setq centaur-tabs-set-bar 'alternate)
;;   ;; (setq centaur-tabs-style "wave")
;;   ;; (centaur-tabs-group-by-projectile-project)
;;   (setq centaur-tabs-set-close-button nil)
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-gray-out-icons 'buffer)
;;   ;; :bind
;;   ;; ("C-<tab>" . centaur-tabs-forward-tab)
;;   ;; ("C-<iso-lefttab>" . centaur-tabs-backward-tab)
;;   )

;;;;;;;;;;;;;;;
;;; GENERAL ;;;
;;;;;;;;;;;;;;;

(setq browse-url-browser-function 'browse-url-chrome)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package ace-window
  :ensure t)

(use-package amx
  :ensure t
  :config
  (amx-mode t)
  )

(use-package avy
  :ensure t)

(use-package avy-zap
  :ensure t
  )

(use-package better-defaults
  :ensure t)

;; (use-package boon
;;   :ensure t)  ;; I'm thinking of using some functions from it...

(setq calendar-week-start-day 1)

;; (use-package centered-cursor-mode
;;   :defer t)

(use-package change-inner
  :ensure t)

(use-package comment-dwim-2
  :ensure t
  ;; :bind ("C-a" . comment-dwim-2)
  )

(column-number-mode t)

(use-package counsel
  :after ivy
  :ensure t
  :config
  (setq counsel-find-file-ignore-regexp "~undo-tree~")
  )

(use-package counsel-projectile
  :after counsel
  :ensure t
  )

(use-package counsel-tramp
  :after counsel
  :ensure t
  )

(cua-mode t)

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map
        ("w" . wdired-change-to-wdired-mode)
        ("u" . dired-up-directory)
        ("U" . dired-unmark)
        ("* u" . dired-unmark-all-files)
        ("i" . dired-next-line)
        ("o" . dired-previous-line)
        ("p" . dired-find-file)
        ("j" . dired-up-directory)
        ("k" . dired-next-dirline)
        ("l" . dired-prev-dirline)
        (";" . dired-find-file)
        ("s" . swiper)
        ;; ("K" . dired-do-kill-lines)
        ("K" . scroll-up-and-recenter)
        ("L" . scroll-down-and-recenter)
        ("f" . counsel-find-file)
        )
  :config
  (setq dired-dwim-target t)
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))
  (use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))
  )

(use-package dired-toggle
  :after dired
  :ensure t
  :bind
  ("<f7>" . dired-toggle)
  :config
  (add-hook 'dired-toggle-mode-hook
          (lambda () (interactive)
            (visual-line-mode 1)
            (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
            (setq-local word-wrap nil)))
  )

(use-package dired-x
  :after dired)

(use-package dumb-jump
  :ensure t)

(use-package eshell-toggle
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (global-set-key (kbd "s-<del>") 'eyebrowse-close-window-config)
  (global-set-key (kbd "s-<f10>") 'eyebrowse-switch-to-window-config-0)
  (global-set-key (kbd "s-<f1>") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "s-<f2>") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "s-<f3>") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "s-<f4>") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "s-<f5>") 'eyebrowse-switch-to-window-config-5)
  (global-set-key (kbd "s-<f6>") 'eyebrowse-switch-to-window-config-6)
  (global-set-key (kbd "s-<f7>") 'eyebrowse-switch-to-window-config-7)
  (global-set-key (kbd "s-<f8>") 'eyebrowse-switch-to-window-config-8)
  (global-set-key (kbd "s-<f9>") 'eyebrowse-switch-to-window-config-9)
  )

;; (use-package find-file-in-project
;;   :defer t)

(setq gc-cons-threshold 50000000)

(use-package gif-screencast
  :ensure t)

(use-package goto-last-change
  :ensure t)

(use-package helm-config
  :config
  (helm-mode 1)
  )

(use-package helm
  :ensure t
  :init
  (setq
   helm-M-x-fuzzy-match t
   helm-mode-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-locate-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
  )
  :config
  (helm-mode 1)
  (helm-adaptive-mode t)
  :bind
  ("C-c p s g" . helm-do-ag-project-root)
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x b" . helm-mini)
  ("C-x C-r" . helm-recentf)
  )

(use-package helm-ag
  :ensure t)

(use-package helm-rg
  :ensure t)

(use-package helm-flycheck
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :ensure t)

;; (use-package ivy-posframe
;;   :ensure t
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;   (setq ivy-posframe-parameters
;;       '((left-fringe . 10)
;;         (right-fringe . 10)))
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left))
;;         ivy-posframe-height-alist '((swiper . 20)
;;                                     (t      . 20))
;;         ivy-posframe-parameters '((internal-border-width . 1)))
;;   (ivy-posframe-mode 1)
;;   (defun posframe-poshandler-frame-bottom-left-corner (info)
;;   "Posframe's position handler.

;; Get a position which let posframe stay onto its parent-frame's
;; bottom left corner.  The structure of INFO can be found
;; in docstring of `posframe-show'."
;;   (cons 10 (- 0
;;              (plist-get info :mode-line-height)
;;              (plist-get info :minibuffer-height))))
;;   )

(use-package ivy-historian
  :ensure t)

(use-package ivy
  :ensure t
  :after helm
  :init
  (historian-mode +1)
  :config
  (setq ivy-height 20)
  (setq ivy-fixed-height-minibuffer t)
  (ivy-mode t)
  (ivy-prescient-mode)
  )

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode t)
  )

(use-package ivy-youtube
  :defer t
  :config
  (autoload 'ivy-youtube "ivy-youtube" nil t)
  )

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode +1)
  (key-chord-define-global "jk" 'ryo-modal-on)
  (key-chord-define-global "fk" 'kill-current-buffer)
  (key-chord-define-global "fm" 'ivy-switch-buffer)
  (key-chord-define-global "fs" 'save-and-enter-ryo)
  (key-chord-define-global "qq" 'kill-word-or-region)
  (key-chord-define-global "qw" 'my-copy-word-or-region)
  (key-chord-define-global "wq" 'my-backward-copy-word-or-region)
  (key-chord-define-global "fq" 'venv-workon)
  )

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(setq kill-ring-max 500)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

;; (line-number-mode t)

;; (global-linum-mode t)
(add-hook 'shell-mode-hook (lambda () (display-line-number-mode -1)))

(use-package multiple-cursors
  :ensure t
  :config
  :bind
  (
   ;; ("C-t" . mc/edit-lines)
   ;; ("C-t" . mc/mark-next-word-like-this)
   ;; ("M->" . mc/mark-next-word-like-this)
   ;; ("M-," . mc/unmark-next-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   )
  )

(use-package phi-search
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward)
  )  ;; think about it

(use-package ace-mc
  :ensure t) ;; please review this

(setq org-support-shift-select t)
;; (set-default 'truncate-lines t)
;; (setq org-latex-pdf-process
;;       '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "bibtex %b"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f"))
;; (require 'org-ref)
;; (autoload 'helm-bibtex "helm-bibtex" "" t)
;; (setq reftex-default-bibliography "/home/grining/boybi.bib")
;; (setq org-ref-bibliography-notes "~/notes.org")
;; (setq org-ref-default-bibliography reftex-default-bibliography)
;; (setq org-ref-pdf-directory "~/Documents/Mendeley Desktop")
;; (setq bibtex-completion-bibliography reftex-default-bibliography)
;; (setq bibtex-completion-library-path "~/Documents/Mendeley Desktop/")

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  )

(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name my-slack-name
;;    :default t
;;    :client-id my-slack-id
;;    :client-secret my-slack-secret
;;    :token my-slack-token
;;    :subscribed-channels '(test-rename crawler)
;;    :full-and-display-names t)
;;   :bind
;;   ("C-c s q" . slack-start)
;;   ("C-c s w" . slack-select-rooms)
;;   ("C-c s e" . slack-im-open)
;;   ("C-c s a" . slack-message-add-reaction)
;;   )

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

(use-package smart-newline
  :ensure t
  :config
  (smart-newline-mode 1)
  )

;; (use-package smartparens
;;   :config (progn (require 'smartparens-config)
;;                  (smartparens-global-mode t))
;;   )

;; (use-package smartparens-config
;;   :commands smartparens-mode)

(use-package helm-smex
  :ensure t)

(setq savehist-file "~/.emacs.d/savehist"
      history-length 150)

(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saveplace")

;; (use-package spaceline-config
;;   :ensure spaceline
;;   :config
;;   ;; (spaceline-helm-mode 1)
;;   (spaceline-emacs-theme)
;;   (spaceline-toggle-python-pyvenv-on)
;;   (spaceline-toggle-python-env-on)
;;   (spaceline-toggle-python-pyenv-on)
;;   (spaceline-toggle-version-control-on)
;;   )

(use-package string-inflection
  :ensure t
  )

(use-package swiper
  :ensure t
  )

(use-package switch-buffer-functions
  :ensure t
  )

(setq tramp-default-method "ssh")
(use-package helm-tramp
  :ensure t)
;; (add-hook 'helm-tramp-pre-command-hook '(lambda () (projectile-mode 0)))
;; (add-hook 'helm-tramp-quit-hook '(lambda () (projectile-mode 1)))
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history 1)
  (setq undo-tree-visualizer-timestamps 1)
  )

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1)
  )

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode)
  )

(winner-mode 1)



;;;;;;;;;;;;;;;;;
;;; SHORTCUTS ;;;
;;;;;;;;;;;;;;;;;

(bind-keys*
;;  ("M-j" . left-char)  ; used to be electric-newline-and-maybe-indent
;;  ("M-k" . next-line)  ; used to be kill-whole-line
;;  ("M-l" . previous-line)  ; used to be recenter-top-bottom
;;  ("M-;" . right-char)
;;  ("C-j" . left-word)  ; was indent-new-comment-line
;;  ("C-k" . forward-paragraph)  ; was kill-sentence; can be done now by M-0 C-d
;;  ("C-l" . backward-paragraph)  ; was downcase-word
;;  ("C-;" . right-word) ; was comment-dwim
;;  ("C-," . forward-sexp)
;;  ("C-." . backward-sexp)
;;  ("C-M-j" . move-beginning-of-line)  ; was comment-indent-new-line
;;  ("C-M-k" . scroll-up-command)  ; was kill-sexp
;;  ("C-M-l" . scroll-down-command)  ; was reposition
;;  ("C-M-;" . move-end-of-line)
;;  ("C-o" . vi-open-line-below)
 ("M-o" . ace-window)
;;  ;; ("C-a" . comment-dwim)  ; was move-beginning-of-line
;;  ("C-f" . recenter-top-bottom)  ; was forward-char
;;  ("C-d" . kill-whole-line)  ; was some delete
;;  ("C-w" . backward-kill-word)  ; was kill-region
;;  ("C-e" . highlight-symbol-next)  ; was same as <end>
;;  ("C-S-e" . highlight-symbol-prev)
;;  ("C-M-e" . highlight-symbol)  ; was forward-sentence
)

;; (global-set-key (kbd "M-:") (kbd "S-<right>"))
;; (global-set-key (kbd "C-:") (kbd "S-M-f"))
;; (global-set-key (kbd "C-M-:") (kbd "S-<end>"))
;; (global-set-key (kbd "C-<") (kbd "S-C-,"))
;; (global-set-key (kbd "C->") (kbd "S-C-."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PYTHON AND PROJECTS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blacken
  :ensure t
  :config
  (setq blacken-skip-string-normalization nil)
  (setq blacken-line-length 120)
  (setq blacken-allow-py36 t)
  (add-hook 'python-mode-hook 'blacken-mode)
  ;; (remove-hook 'python-mode-hook 'blacken-mode)
  )

;; (use-package python-black
;;   :demand t
;;   :ensure t
;;   :after python
;;   :config
;;   (python-black-on-save-mode)
;;   )

(use-package company
  :ensure t
  :config
  (setq company-backends '((company-capf)))
  (global-company-mode 1)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.01)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations 't)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case 0)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  )

(use-package company-jedi
  :ensure t
  ;; (defun my/python-mode-hook ()
  ;;   (add-to-list 'company-backends 'company-jedi))

  ;; (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

;; (use-package company-posframe
;;   :ensure t
;;   :config
;;   (company-posframe-mode nil)
;;   )

(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode 1)
  )


;; I think the following makes autocompletion slow:
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi)
;;   (company-mode)
;;   )
;; (add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package cython-mode
  :defer t
  )

(use-package docker
  :defer t
  :bind
  ("C-c d" . docker)
  )
(use-package docker-tramp
  :after (docker tramp)
  :ensure t
  )

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'eglot-ensure)
;;   )

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (add-hook 'python-mode-hook 'hs-minor-mode)
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  )

;; (eval-after-load 'flycheck
  ;; '(define-key flycheck-mode-map (kbd "C-c C-! C-h") 'helm-flycheck))
;; (eval-after-load 'flycheck
;;   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
;; ^ will be handled by ryo mode

;; (use-package hy-mode
;;   :ensure t)

;; ;; isort
;; (use-package py-isort
;;   :ensure t
;;   :config
;;   (add-hook 'before-save-hook 'py-isort-before-save))

(use-package jedi
  :ensure t
  :config
  ;; (add-hook 'python-mode-hook 'jedi:setup)
  (setq
   jedi:complete-on-dot t
   jedi:use-shortcuts t
   jedi:environment-root "jedi"
   python-environment-directory "~/.virtualenvs")
  )

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :custom
;;   (lsp-auto-guess-root nil)
;;   (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;   (lsp-enable-snippet t)
;;   :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;   :hook ((python-mode c-mode c++-mode) . lsp))


;; (use-package lsp-ui
;;   :after lsp-mode
;;   :diminish
;;   :commands lsp-ui-mode
;;   :custom-face
;;   (lsp-ui-doc-background ((t (:background nil))))
;;   (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references)
;;               ("C-c u" . lsp-ui-imenu))
;;   :custom
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-position 'top)
;;   (lsp-ui-doc-border (face-foreground 'default))
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-code-actions nil)
;;   :config
;;   ;; Use lsp-ui-doc-webkit only in GUI
;;   (setq lsp-ui-doc-use-webkit t)
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil)))

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 3)

;;   (global-company-mode t)
;;   )

;; (venv-workon "crwcommon")
;; (setq lsp-python-executable-cmd "python")

;; (use-package company-lsp
;;   :ensure t
;;   :config
;;  (push 'company-lsp company-backends)
;; )

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c m" . magit-blame-addition)
  )
(setenv "EDITOR" "emacsclient")

;; (use-package neotree
;;   :defer t
;;   :init
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   (defun neotree-project-dir ()
;;   "Open NeoTree using the git root."
;;   (interactive)
;;   (let ((project-dir (projectile-project-root))
;;         (file-name (buffer-file-name)))
;;     (neotree-toggle)
;;     (if project-dir
;;         (if (neo-global--window-exists-p)
;;             (progn
;;               (neotree-dir project-dir)
;;               (neotree-find file-name)))
;;       (message "Could not find git project root."))))
;;   :bind
;;   ("<f8>" . neotree-project-dir)
;;   )

(use-package pip-requirements
  :hook ((pip-requirements-mode . company-mode))
  :ensure t)

(use-package projectile
  :ensure t
  :init
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  ;; (helm-projectile-on)
  ;; :bind
  ;; ("C-c C-p C-p" . projectile-switch-project)
  ;; ("C-c C-p C-h" . helm-projectile)
  ;; ("C-c C-p C-f" . projectile-find-file)
  ;; ("C-c C-p C-d" . projectile-dir)
  ;; ("C-c C-p C-t" . projectile-toggle-between-implementation-and-test)
  ;; ("C-c C-p C-r" . projectile-replace)
  ;; ("C-c C-p C-e" . projectile-replace-regexp)
  ;; ("C-c C-p C-s s" . projectile-ag)
  ;; ("C-c C-p C-s g" . projectile-grep)
  ;; ("C-c C-p C-s r" . projectile-ripgrep)
  ;; ("C-c C-p C-k" . projectile-kill-buffers)
  ;; ("C-c C-p C-S" . projectile-save-project-buffers)
  ;; ("C-c p p" . projectile-switch-project)
  ;; ("C-c p h" . helm-projectile)
  ;; ("C-c p f" . projectile-find-file)
  ;; ("C-c p d" . projectile-dir)
  ;; ("C-c p t" . projectile-toggle-between-implementation-and-test)
  ;; ("C-c p r" . projectile-replace)
  ;; ("C-c p e" . projectile-replace-regexp)
  ;; ("C-c p s s" . projectile-ag)
  ;; ("C-c p s g" . projectile-grep)
  ;; ("C-c p s r" . projectile-ripgrep)
  ;; ("C-c p k" . projectile-kill-buffers)
  ;; ("C-c p S" . projectile-save-project-buffers)
  ;; ^ will be handled by ryo mode
  )

;; (use-package ibuffer-projectile
;;   :ensure t
;;   :config
;;   (add-hook
;;    'ibuffer-hook
;;    (lambda ()
;;      (ibuffer-projectile-set-filter-groups)
;;      (unless (eq ibuffer-sorting-mode 'alphabetic)
;;        (ibuffer-do-sort-by-alphabetic))))
;;   )

;; (use-package python-pytest
;;   :ensure t)

;; (use-package realgud
;;   :ensure t)

(use-package virtualenvwrapper
  :ensure t
  )

(venv-initialize-interactive-shells)
(defvar python-environment-directory)
(setq python-environment-directory "~/.virtualenvs/")
(setq venv-location "~/.virtualenvs/")
(venv-initialize-eshell)

;; (use-package auto-virtualenvwrapper
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
;;   (add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
;;   (add-hook 'focus-in-hook #'auto-virtualenvwrapper-activate)
;;   )

(use-package auto-virtualenv
  :ensure t
  :config
  (setq auto-virtualenv-dir "~/.virtualenvs")
  ;; (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  ;; (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)
  )

(use-package yasnippet-snippets
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.conf/snippets"))
  (yas-reload-all)
  )

;;;;;;;;;;;;;;;;;;;;;;
;; Other Languages ;;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t)
(use-package csv-mode
  :defer t
  :mode "\\.csv\\'")
(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(use-package json-mode
  :ensure t)
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.jinja\\'")
  :config (setq web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2)
  )
(use-package markdown-mode
  :ensure t
  )
(use-package markdown-mode+
  :ensure t
  )
(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )
(use-package auto-complete-rst
  :defer t
  )
(use-package uml-mode
  :ensure t
  )
(use-package yaml-mode
  :ensure t
  :config
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPERIMENTAL TABBAR TWEAKS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/3demax/1264635#file-tabbar-tweak-el
;; Tabbar settings
;; (set-face-attribute
;;  'tabbar-default nil
;;  :background "#002b36"
;;  :foreground "#002b36"
;;  :underline nil
;;  :box nil)
;; (set-face-attribute
;;  'tabbar-unselected nil
;;  :background "#002b36"
;;  :foreground "#aaaaaa"
;;  :underline nil
;;  :box nil)
;; (set-face-attribute
;;  'tabbar-selected nil
;;  :background "#aaaaaa"
;;  :foreground "#002b36"
;;  :underline nil
;;  :box nil)
;; (set-face-attribute
;;  'tabbar-highlight nil
;;  :background "#aaaaaa"
;;  :foreground "#002b36"
;;  :underline nil
;;  :box nil)
;; (set-face-attribute
;;  'tabbar-button nil
;;  :underline nil
;;  :box nil)
;; (set-face-attribute
;;  'tabbar-separator nil
;;  :underline nil
;;  :background "#002b36em"
;;  :height 0.6)
;; ;; adding spaces
;; (defun tabbar-buffer-tab-label (tab)
;;   "Return a label for TAB.
;; That is, a string used to represent it on the tab bar."
;;   (let ((label  (if tabbar--buffer-show-groups
;;                     (format "[%s]  " (tabbar-tab-tabset tab))
;;                   (format "%s  " (tabbar-tab-value tab)))))
;;     ;; Unless the tab bar auto scrolls to keep the selected tab
;;     ;; visible, shorten the tab label to keep as many tabs as possible
;;     ;; in the visible area of the tab bar.
;;     (if tabbar-auto-scroll-flag
;;         label
;;       (tabbar-shorten
;;        label (max 1 (/ (window-width)
;;                        (length (tabbar-view
;;                                 (tabbar-current-tabset)))))))))
;; (tabbar-mode 1)

(define-key helm-find-files-map (kbd "C-j") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-u") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-i") 'helm-next-line)
(define-key helm-find-files-map (kbd "C-o") 'helm-previous-line)
(define-key helm-find-files-map (kbd "C-p") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-;") 'helm-execute-persistent-action)
(define-key helm-buffer-map (kbd "C-i") 'helm-next-line)
(define-key helm-buffer-map (kbd "C-o") 'helm-previous-line)
(define-key helm-read-file-map (kbd "C-j") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-u") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-i") 'helm-next-line)
(define-key helm-read-file-map (kbd "C-o") 'helm-previous-line)
(define-key helm-read-file-map (kbd "C-p") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-;") 'helm-execute-persistent-action)
(define-key ivy-minibuffer-map (kbd "C-i") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "<left>") 'counsel-up-directory)
(define-key ivy-minibuffer-map (kbd "C-j") 'counsel-up-directory)
(define-key ivy-minibuffer-map (kbd "C-u") 'counsel-up-directory)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
;; (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-;") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<RET>") 'ivy-alt-done)

(global-set-key (kbd "M-<up>") 'elpy-nav-move-line-or-region-up)
(global-set-key (kbd "M-<down>") 'elpy-nav-move-line-or-region-down)
(global-set-key (kbd "C-M-<return>") 'newline)
(global-set-key (kbd "<S-iso-lefttab>") 'elpy-folding-toggle-at-point)


(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :bind ("<escape>" . ryo-modal-mode)
  :bind ("C-c C-r" . ryo-modal-mode)
  :config
  (add-hook 'text-mode-hook #'ryo-modal-mode)
  (add-hook 'prog-mode-hook #'ryo-modal-mode)
  (add-hook 'fundamental-mode-hook #'ryo-modal-mode)
  (add-hook 'special-mode-hook #'ryo-modal-mode)
  (add-hook 'magit-status-mode-hook #'ryo-modal-off)
  (add-hook 'conf-unix-mode-hook #'ryo-modal-mode)
  (setq ryo-modal-default-cursor-color "#839496")
  ;; (setq-default cursor-type 'bar)
  (setq ryo-modal-cursor-type 'hbar)
  ;; (setq ryo-modal-cursor-color nil)
  (ryo-modal-mode)

  (ryo-modal-keys
   ;; ("," ryo-modal-repeat)
   ("q" kill-word-or-region)  ;; think about it: it's a great shortcut but maybe underused
   ("Q" my-copy-word-or-region)  ;; not sure
   ("w" backward-kill-word-or-region)  ;; think about it: it's a great shortcut but maybe underused
   ("W" my-backward-copy-word-or-region)  ;; not surem
   ("e" highlight-symbol-next)
   ("E" highlight-symbol-prev)
   ("r" avy-goto-word-1-below)
   ("R" avy-goto-word-1-above)
   ("t" vi-open-line-below)
   ("T" vi-open-line-above)
   ("y" ivy-resume)  ;; think about it
   ("U" nav-backward-indent)
   ("u" backward-char)
   ("i" next-line)
   ("I" magic-elpy-nav-forward-block)
   ("o" previous-line)
   ("O" magic-elpy-nav-backward-block)
   ("p" forward-char)
   ("P" nav-forward-indent)
   ;; ("[" )  ;; think about it
   ;; ("]" )  ;; think about it

   ("a" comment-dwim-2)
   ("s" swiper)
   ("S" swiper-thing-at-point) ;; use it
   ("g" keyboard-quit)
   ("G" end-of-buffer)
   ("h" move-beginning-of-line)
   ;; ("h" beginning-of-line-or-indentation)
   ("j" my-backward-word)
   ("J" move-beginning-of-line)  ;; almost useless
   ("k" forward-paragraph)
   ("K" scroll-up-and-recenter)
   ("l" backward-paragraph)
   ("L" scroll-down-and-recenter)
   (";" my-forward-word)
   (":" move-end-of-line)  ;; almost useless
   ("'" move-end-of-line)
   ("\"" mark-until-end-of-line)  ;; haven't been using it but doesn't hurt

   ("z" undo-tree-undo)
   ("Z" undo-tree-redo)
   ("x" kill-whole-line-or-region)
   ("X" delete-char)  ;; think about it
   ("c" copy-whole-line-or-region)
   ("C" delete-backward-char)
   ("v" cua-paste)
   ("V" paste-in-new-line)
   ("b" er-switch-to-previous-buffer)  ;; use it
   ("n" recenter-top-bottom)
   ("." awesome-tab-forward-tab)
   ;; ("." centaur-tabs-forward)
   ("," awesome-tab-backward-tab)
   ;; ("," centaur-tabs-backward)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("/" dumb-jump-go)
   ("?" dumb-jump-back)

   ("<f1>" blacken-buffer)  ;; almost useless now with blacken on save
   ("`" bookmark-jump) ;; useful but does it warrant 1-key sequence?
   ;; ("~" )  ;; think about it
   ("!" helm-flycheck)
   ("%" query-replace)
   ("-" delete-horizontal-and-vertical-space)
   ("_" delete-horizontal-and-vertical-space-but-leave-one-space)
   ("=" er/expand-region)
   ("+" mark-paragraph)  ;; use me
   ("]" mark-paragraph)  ;; use me
   ("\\" er/mark-python-statement)  ;; use me
   ("SPC" cua-set-mark)
   ("RET" smart-newline)
   )

  (ryo-modal-keys
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9")
   )

  (ryo-modal-key
   "d" '(
         ;; ("q" elfeed)  ;; think about it
         ("w" add-correct-start-of-commit)
         ("e" projectile-replace-regexp)  ;; I'm not using it and and this is a valuable shortcut
         ("r" projectile-replace)
         ;; ("R" projectile-ripgrep)
         ("t" projectile-toggle-between-implementation-and-test)  ;; useful when it works
         ("y" string-inflection-python-style-cycle)  ;; unify this and learn

         ;; The following are all very important, but I'm not 100% used to them yet.
         ;; I feel like I need to invest more into them and unify with f i, f o and such.
         ("u" change-inner-with-paren)
         ;; ("u" subword-mode)
         ("i" change-inner-with-square)
         ("o" change-inner-with-curly)
         ("p" copy-inner-with-paren)
         ("[" copy-inner-with-square)
         ("]" copy-inner-with-curly)
         ("U" copy-inner-with-paren)
         ("I" copy-inner-with-square)
         ("O" copy-inner-with-curly)
         ;; ("P" copy-outer-with-paren)
         ;; ("{" copy-outer-with-square)
         ;; ("}" copy-outer-with-curly)

         ;; ("a" )  ;; prime location and unused!
         ;; ("s" counsel-projectile-ag)
         ("s" helm-projectile-ag)
         ("S" projectile-save-project-buffers)  ;; perfect, I don't use it often and I remember it
         ("d" projectile-dired)  ;; probably duplicates dired-jump
         ("f" counsel-projectile-find-file-dwim)
         ;; ("f" projectile-find-file)
         ("g" helm-projectile-rg)
         ;; ("g" counsel-projectile-ag-at-point)
         ("h" counsel-projectile)
         ;; ("h" helm-projectile)
         ("j" counsel-projectile-switch-project)
         ;; ("j" helm-projectile-switch-project)
         ("k" projectile-kill-buffers)  ;; similar to f k for one buffer, but not sure if I need this mnemotechnique
         ;; ("l" centaur-tabs-counsel-switch-group)  ;; important but not sure if in the right place
         ("l" awesome-tab-switch-group)
         ;; (";" kill-inside-or-not)  ;; think about it
         ("'" kill-all-buffers-but-scratch)  ;; useful and out of the way, good use

         ;; pytest I'm struggling to use inside emacs...
         ("z" python-pytest-popup)  ;; think about it
         ("x" python-pytest-function-dwim)  ;; think about it
         ("c" python-pytest-file-dwim)  ;; think about it
         ("v" python-pytest-repeat)  ;; think about it
         ;; ("b" )  ;; think about it
         ;; ("n" )  ;; think about it
         ("m" go-to-119)  ;; almost useless

         ;; please unify those and learn them...
         ("," string-inflection-all-cycle)  ;; think about it
         ("." string-inflection-python-style-cycle)  ;; think about it
         ;; ("/" )  ;; think about it

         ;; almost useless but hey, it's not like I'm loosing anything
         ("!" projectile-run-shell-command-in-root)
         ("%" projectile-run-async-shell-command-in-root)
         )
   )

  (ryo-modal-key
   "D" '(
         ("R" projectile-ripgrep)
         ("U" change-outer-with-paren)
         ("I" change-outer-with-square)
         ("O" change-outer-with-curly)
         ;; ("P" copy-outer-with-paren)
         ;; ("{" copy-outer-with-square)
         ;; ("}" copy-outer-with-curly)
         )
   )

  (ryo-modal-key
   "f" '(
         ("q" venv-workon)
         ("w" python-add-breakpoint)
         ("e" eval-last-sexp)
         ("r" avy-goto-line)
         ("t" elpy-multiedit-python-symbol-at-point)
         ("y" kill-inside-or-not)  ;; remember
         ("u" copy-inside-or-not)  ;; remember
         ("i" mark-inside-or-not)
         ("o" mark-outside-or-not)
         ("p" copy-outside-or-not)  ;; remember
         ;; ("p" other-window)
         ("[" kill-outside-or-not)  ;; remember
         ;; ("[" ace-window)
         ("a" goto-last-change)  ;; think about it
         ("s" save-buffer)
         ("d" dired-jump)
         ("f" counsel-find-file)
         ;; ("f" helm-find-files)
         ("g" magit-status)
         ("h" mark-whole-buffer)
         ;; ("j" )  ;; EMPTY
         ("k" kill-current-buffer)  ;; useful but maybe somewhere else?
         ("l" counsel-projectile-switch-to-buffer)  ;; useful but maybe somewhere else?
         ;; ("l" helm-mini)
         ;; (";" )  ;; EMPTY
         ("z" avy-zap-up-to-char-dwim)
         ("Z" avy-zap-to-char-dwim)
         ("x" counsel-M-x)
         ;; ("x" helm-M-x)
         ("c" save-buffers-kill-terminal)
         ("v" counsel-yank-pop)
         ;; ("v" helm-show-kill-ring)
         ("V" paste-from-kill-ring-new-line)
         ("b" imenu)  ;; think about it
         ("n" goto-line)  ;; useful but I could just as well use M-g M-g
         ("m" ivy-switch-buffer)
         ("." xref-find-definitions)
         ("," xref-pop-marker-stack)
         ;; ("/" )  ;; think about it
         ("0" delete-window)
         ("1" delete-other-windows)
         ("2" split-window-below)
         ("3" split-window-right)
         ("5 0" delete-frame)
         ("5 1" delete-other-frames)
         ("5 2" make-frame-command)
         ("SPC" rectangle-mark-mode)
         ("M-o" elpy-nav-move-line-or-region-up)  ;; this is not useful
         ("M-i" elpy-nav-move-line-or-region-down)  ;; this is not useful
         )
   )

  (ryo-modal-key
   "F" '(
         ("D" docker)
         )
   )
  )
