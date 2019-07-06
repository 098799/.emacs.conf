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

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 106 :width normal :family "Ubuntu Mono")))))

;; (global-prettify-symbols-mode t)

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

(use-package solarized-theme
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
  )

;; (add-to-list 'load-path "~/.emacs.d/tabbar/")

(setq awesome-tab-background-color "#002B36")
(setq awesome-tab-style "bar")
(add-to-list 'load-path "~/.emacs.d/awesome-tab/")
;; (load "tabbar")
(require 'awesome-tab)
(awesome-tab-mode t)
(global-set-key (kbd "<C-tab>") 'awesome-tab-forward-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'awesome-tab-backward-tab)

;;;;;;;;;;;;;;;
;;; GENERAL ;;;
;;;;;;;;;;;;;;;

(setq browse-url-browser-function 'browse-url-chrome)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package ace-window
  :ensure t)

(use-package avy
  :ensure t)

(use-package avy-zap
  :ensure t
  )

(use-package better-defaults
  :ensure t)

;; (use-package boon
;;   :ensure t)  ;; I'm thinking of using some functions from it...

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
        ("K" . dired-do-kill-lines)
        ("f" . counsel-find-file)
        )
  :config
  (setq dired-dwim-target t)
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

;; (use-package find-file-in-project
;;   :defer t)

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
  (awesome-tab-build-helm-source)
  :bind
  ("C-c p s g" . helm-do-ag-project-root)
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x b" . helm-mini)
  ("C-x C-r" . helm-recentf)
  )

(use-package helm-flycheck
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :ensure t)

(use-package company-prescient
  :ensure t)

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
  (ivy-rich-mode t)
  (ivy-prescient-mode)
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
  (key-chord-define-global "jk" 'ryo-modal-mode)
  )

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(global-display-line-numbers-mode 1)

;; (line-number-mode t)

;; (global-linum-mode t)
(add-hook 'shell-mode-hook (lambda () (display-line-number-mode -1)))

(use-package multiple-cursors
  :ensure t
  :bind
  (
   ("M-." . mc/mark-next-like-this)
   ("M->" . mc/mark-next-word-like-this)
   ("M-," . mc/unmark-next-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   )
  )

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

(use-package smex
  :ensure t)

(use-package helm-smex
  :ensure t)

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

;; (use-package which-key
;;   :ensure t
;;   :init
;;   (setq which-key-separator " ")
;;   (setq which-key-prefix-prefix "+")
;;   :config
;;   (which-key-mode 1)
;;   )

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
  (setq blacken-line-length 110)
  (setq blacken-allow-py36 t)
  )

(use-package company
  :ensure t
  :config
  ;; (global-company-mode)
  ;; (setq company-auto-complete t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.1)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations 't)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case 0)
  ;; (company-tng-configure-default)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  )

(use-package company-jedi
  :ensure t
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
  :defer t
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
;;   :init
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-prefer-flymake nil)
;;   :config
;;   (require 'lsp-clients)
;;   (add-hook 'prog-mode-hook (lambda () (flymake-mode -1)))
;;   (define-key lsp-mode-map (kbd "S-<f6>") 'lsp-rename)
;;   (defun wcx/activate-lsp ()
;;     (ycmd-mode -1)
;;     (lsp))
;;   :hook ((python-mode bash-mode lua-mode ruby-mode) . wcx/activate-lsp))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;; (push 'company-lsp company-backends)

;; (use-package dap-mode
;;   :ensure t
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (require 'dap-python))

;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (require 'lsp-clients)
;;   (add-hook 'lsp-after-open-hook 'lsp-ui-mode)
;;   (add-hook 'python-mode-hook 'lsp))
;; (use-package lsp-ui
;;   :ensure t)

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

(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (ibuffer-projectile-set-filter-groups)
     (unless (eq ibuffer-sorting-mode 'alphabetic)
       (ibuffer-do-sort-by-alphabetic))))
  )

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
  :defer t
  )
(use-package markdown-mode+
  :defer t
  )
(use-package auto-complete-rst
  :defer t
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

;; (define-key helm-find-files-map (kbd "C-j") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-u") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-i") 'helm-next-line)
;; (define-key helm-find-files-map (kbd "C-o") 'helm-previous-line)
;; (define-key helm-find-files-map (kbd "C-p") 'helm-execute-persistent-action)
;; (define-key helm-find-files-map (kbd "C-;") 'helm-execute-persistent-action)
;; (define-key helm-buffer-map (kbd "C-i") 'helm-next-line)
;; (define-key helm-buffer-map (kbd "C-o") 'helm-previous-line)
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

(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :bind ("<escape>" . ryo-modal-mode)
  :bind ("C-c C-r" . ryo-modal-mode)
  :config
  (add-hook 'text-mode-hook #'ryo-modal-mode)
  (add-hook 'prog-mode-hook #'ryo-modal-mode)
  (add-hook 'fundamental-mode-hook #'ryo-modal-mode)
  ;; (add-hook 'special-mode-hook #'ryo-modal-mode)
  (add-hook 'conf-unix-mode-hook #'ryo-modal-mode)
  (ryo-modal-mode)

  (ryo-modal-keys
   ;; ("," ryo-modal-repeat)
   ("q" kill-word-or-region)
   ("Q" my-copy-word-or-region)
   ("w" backward-kill-word-or-region)
   ("W" my-backward-copy-word-or-region)
   ("e" highlight-symbol-next)
   ("E" highlight-symbol-prev)
   ("r" avy-goto-word-1)
   ("t" vi-open-line-below)
   ("T" vi-open-line-above)
   ("y" eshell-toggle)
   ("U" nav-backward-indent)
   ("u" backward-char)
   ("i" next-line)
   ("I" elpy-nav-forward-block)
   ("o" previous-line)
   ("O" elpy-nav-backward-block)
   ("p" forward-char)
   ("P" nav-forward-indent)

   ("a" comment-dwim-2)
   ("s" swiper)
   ("S" swiper-thing-at-point)
   ("g" keyboard-quit)
   ("G" end-of-buffer)
   ("h" move-beginning-of-line)
   ;; ("h" beginning-of-line-or-indentation)
   ("j" my-backward-word)
   ("J" move-beginning-of-line)
   ("k" forward-paragraph)
   ("K" scroll-up-and-recenter)
   ("l" backward-paragraph)
   ("L" scroll-down-and-recenter)
   (";" my-forward-word)
   (":" move-end-of-line)
   ("'" move-end-of-line)
   ("\"" mark-until-end-of-line)

   ("z" undo-tree-undo)
   ("Z" undo-tree-redo)
   ("x" kill-whole-line-or-region)
   ("X" delete-char)
   ("c" copy-whole-line-or-region)
   ("v" cua-paste)
   ("V" paste-in-new-line)
   ("n" recenter-top-bottom)
   ("." xref-find-definitions)
   ("," xref-pop-marker-stack)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("/" dumb-jump-go)
   ("?" dumb-jump-back)

   ("<f1>" blacken-buffer)
   ("<f2>" blacken-region)
   ("`" bookmark-jump)
   ("!" helm-flycheck)
   ("%" query-replace)
   ("-" delete-horizontal-space)
   ("=" er/expand-region)
   ("+" mark-paragraph)
   ("]" mark-paragraph)
   ("\\" er/mark-python-statement)
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
         ;; ("q" elfeed)
         ("w" add-correct-start-of-commit)
         ("e" projectile-replace-regexp)
         ("r" projectile-replace)
         ("R" projectile-ripgrep)
         ("t" projectile-toggle-between-implementation-and-test)
         ("y" string-inflection-python-style-cycle)
         ;; ("u" subword-mode)
         ("u" copy-inner-with-paren)
         ("i" change-inner-with-paren)
         ("o" change-outer-with-paren)
         ("p" copy-outer-with-paren)
         ;; ("[" helm-projectile-recentf)
         ;; ("s" counsel-projectile-ag)
         ("s" helm-projectile-ag)
         ("S" projectile-save-project-buffers)
         ("d" projectile-dired)
         ("f" counsel-projectile-find-file-dwim)
         ;; ("f" projectile-find-file)
         ;; ("g" counsel-projectile-ag-at-point)
         ("g" helm-projectile-grep)
         ("h" counsel-projectile)
         ;; ("h" helm-projectile)
         ("j" counsel-projectile-switch-project)
         ;; ("j" helm-projectile-switch-project)
         ("k" projectile-kill-buffers)
         ("l" awesome-tab-switch-group)
         ;; ("z" python-pytest-popup)
         ;; ("x" python-pytest-repeat)
         ("!" projectile-run-shell-command-in-root)
         ("%" projectile-run-async-shell-command-in-root)
         )
   )

  (ryo-modal-key
   "D" '(
         ("R" projectile-ripgrep)
         ("U" copy-inner)
         ("I" change-inner)
         ("O" change-outer)
         ("P" copy-outer)
         )
   )

  (ryo-modal-key
   "f" '(
         ("q" venv-workon)
         ("w" python-add-breakpoint)
         ("e" eval-last-sexp)
         ("r" avy-goto-line)
         ;; ("r" helm-recentf)
         ("u" undo-tree-visualize)
         ("i" mark-inside-or-not)
         ("o" mark-outside-or-not)
         ("p" other-window)
         ("[" ace-window)
         ("a" goto-last-change)
         ("s" save-buffer)
         ("d" dired-jump)
         ("f" counsel-find-file)
         ;; ("f" helm-find-files)
         ("g" magit-status)
         ("h" mark-whole-buffer)
         ("j" awesome-tab-backward-tab)
         ("k" kill-current-buffer)
         ("l" counsel-projectile-switch-to-buffer)
         ;; ("l" helm-mini)
         (";" awesome-tab-forward-tab)
         ("z" avy-zap-up-to-char-dwim)
         ("Z" avy-zap-to-char-dwim)
         ("x" counsel-M-x)
         ;; ("x" helm-M-x)
         ("c" save-buffers-kill-terminal)
         ("v" counsel-yank-pop)
         ;; ("v" helm-show-kill-ring)
         ("V" paste-from-kill-ring-new-line)
         ("n" goto-line)
         ("m" counsel-switch-buffer)
         ("m" ivy-switch-buffer)
         ("0" delete-window)
         ("1" delete-other-windows)
         ("2" split-window-below)
         ("3" split-window-right)
         ("5 0" delete-frame)
         ("5 1" delete-other-frames)
         ("5 2" make-frame-command)
         ("SPC" rectangle-mark-mode)
         )
   )

  (ryo-modal-key
   "F" '(
         ("D" docker)
         )
   )
  )

(add-hook 'switch-buffer-functions
          (lambda (prev cur) (ryo-modal--cursor-color-update))
          )
