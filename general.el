;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default nil)
;; (setq package-enable-at-startup nil)


;; (add-to-list 'package-archives
;;              '("nongnu" . "https://elpa.nongnu.org/nongnu/"))


(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
;;                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(straight-use-package 'project)  ;; some problem with old version that magit had...

;;;;;;;;;;;;;;;
;;; VISUALS ;;;
;;;;;;;;;;;;;;;

;; THINK ABOUT THIS but not now
;; (add-to-list 'load-path "/Users/rougier/Documents/GitHub/nano-emacs")
;; (straight-use-package
;;   '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(defalias 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(global-auto-revert-mode 1)

(setq scroll-margin 5)
(setq recenter-positions '(middle top bottom))

(use-package all-the-icons
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  ;; (set-face-background hl-line "gray13")
  ;; (global-hl-line-mode +1)
  )

(use-package command-log-mode
  :ensure t)

(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode t)
  )

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 0)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-vcs-max-length 5)
  (setq doom-modeline-project-detection 'auto)
  )


(use-package nerd-icons
  :ensure t)


(setq echo-keystrokes 0.5)

(custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "Ubuntu Mono")))))

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
  :config
  (use-package auto-highlight-symbol
    :ensure t
    :config
    (global-auto-highlight-symbol-mode t)
    (setq ahs-idle-interval 0.0)
    )
  )

(use-package highlight-indentation
  :ensure t
  )

(use-package nav-flash
  :ensure t
  :config
  (nav-flash-show))

;; (use-package nano-theme
;;   :ensure nil
;;   :defer t
;;   :quelpa (nano-theme
;;            :fetcher github
;;            :repo "rougier/nano-theme")
;;   :config
;;   ;; (load-theme 'nano-light)
;;   ;; (load-theme 'nano-dark)
;;   )

(use-package solarized-theme
  :ensure t
  ;; :config
  ;; (load-theme 'solarized-light t)
  ;; (load-theme 'solarized-selenized-dark t)
  ;; (load-theme 'solarized-gruvbox-light t)
  ;; (load-theme 'solarized-gruvbox t)
  )

;; (use-package material-theme
;;   :ensure t
;;   :config
;;   (load-theme 'material t)
;;   (load-theme 'material-light t)
;;   )

;; (use-package spacemacs-theme
;;   :ensure t
;;   :config
;;   (load-theme 'spacemacs-dark t))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (show-paren-mode t)
  (setq show-paren-style 'expression)
  )

(use-package rainbow-mode
  :ensure t)

;; (add-to-list 'load-path "~/.emacs.d/tabbar/")

;; (use-package awesome-tab
;;   :load-path "~/.emacs.d/awesome-tab/"
;;   :config
;;   (setq awesome-tab-background-color "#fbf8ef")
;;   (awesome-tab-mode t)
;;   (global-set-key (kbd "<C-tab>") 'awesome-tab-forward-tab)
;;   (global-set-key (kbd "<C-iso-lefttab>") 'awesome-tab-backward-tab)
;;   )

(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode 0)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-height 28)
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-label-fixed-length 14)
  (global-set-key (kbd "<C-tab>") 'centaur-tabs-forward)
  (global-set-key (kbd "<C-iso-lefttab>") 'centaur-tabs-backward)
  ;; (centaur-tabs-mode 1)
  )

;;;;;;;;;;;;;;;
;;; GENERAL ;;;
;;;;;;;;;;;;;;;

(setq browse-url-browser-function 'browse-url-chrome)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package ace-window
  :ensure t)

;; (use-package amx
;;   :ensure t
;;   :config
;;   (amx-mode t)
;;   ;; (amx-mode nil)
;;   )

(use-package avy
  :ensure t)

(use-package avy-zap
  :ensure t
  )

(use-package better-defaults
  :ensure t)

(setq bookmark-save-flag t)

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

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'copilot-mode)
;;   ;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "C-e") 'copilot-accept-completion)
;;   )

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :config
  (add-hook 'python-mode-hook 'copilot-mode)
  ;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-e") 'copilot-accept-completion)
  )

(use-package counsel
  :after ivy
  :ensure t
  :config
  (setq counsel-find-file-ignore-regexp "~undo-tree~")
  (global-set-key (kbd "C-h b") 'counsel-descbinds)
  )

(use-package counsel-projectile
  :after (counsel projectile)
  :ensure t
  :config
  (counsel-projectile-mode 1)
  (setq counsel-ag-base-command "ag --vimgrep --ignore \"*.sql\" --ignore \"*.csv\" --ignore \"*.mar\" %s")
  )

(use-package counsel-tramp
  :after counsel
  :ensure t
  )

;; (require 'quelpa-use-package)
;; (use-package chatgpt
;;   :quelpa ((chatgpt :fetcher git :url "https://github.com/joshcho/ChatGPT.el.git") :upgrade t)
;;   :init
;;   (require 'python)
;;   (unless (boundp 'python-interpreter)
;;     (defvaralias 'python-interpreter 'python-shell-interpreter))
;;   (setq chatgpt-repo-path (expand-file-name "chatgpt/" quelpa-build-dir))
;;   :bind ("C-c q" . chatgpt-query))


(use-package gptel
  :ensure t
  :config
  
  (defcustom gptel-directives
  '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
    (writing . "You are a large language model and a writing assistant. Respond concisely.")
    (chat . "You are a large language model and a conversation partner. Respond concisely.")
    (code . "You are presented with a part of computer program. Respond with the code that is most likely to fit at the end of the block you're presented with. Don't give any markup. Make sure you're at a right level of indentation. Don't give any comments. Imagine your whole response is verbatum pasted in the code file you're presented with.")
    )
  "System prompts (directives) for the LLM.

These are system instructions sent at the beginning of each
request to the LLM.

Each entry in this alist maps a symbol naming the directive to
the string that is sent.  To set the directive for a chat session
interactively call `gptel-send' with a prefix argument."
  :group 'gptel
  :safe #'always
  :type '(alist :key-type symbol :value-type string))
  )
 

;; (require 'gptel-curl)
;; (require 'gptel-transient)

(cua-mode t)

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :straight nil
  :bind
  (:map dired-mode-map
        ("w" . wdired-change-to-wdired-mode)
        ("e" . eshell)
        ("u" . dired-up-directory)
        ("* u" . dired-mark-undo-tree)
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
        ("c" . dired-do-compress)
        ("H" . dired-hide-dotfiles-mode)
        ("n" . dired-unmark)
        )
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq dired-dwim-target t)
  (setq wdired-allow-to-change-permissions t)
  )

  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))

  (use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

(use-package dired-ranger
  :ensure t)

(use-package dired-toggle
  :after dired
  :ensure t
  :bind
  ("<f7>" . dired-toggle)
  :config
  (setq dired-toggle-window-size 40)
  (add-hook 'dired-toggle-mode-hook
          (lambda () (interactive)
            (visual-line-mode 1)
            (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
            (setq-local word-wrap nil)))
  )

(use-package dired-hide-dotfiles
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)  ;; is this making things work?
  )

;; (use-package dired-posframe
;;   :ensure t
;;   :config
;;   :bind (:map dired-mode-map
;;               ("_" . dired-posframe-mode)))

;; (straight-use-package '(empv :type git :host github :repo "isamert/empv.el"))

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

;; The default is 800 kilobytes.  Measured in bytes.
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

;; (use-package gif-screencast
;;   :ensure t
;;   :bind
;;   ("<f8>" . gif-screencast-toggle-pause)
;;   ("<f9>" . gif-screencast-stop)
;;   ("<f10>" . gif-screencast)
;;   )

(use-package goto-last-change
  :ensure t)

;; (use-package helm-config
;;   :config
;;   (helm-mode 1)
;;   :ensure t
;;   ;; :straight nil
;;   )

(use-package harpoon
  :ensure t)

;; (use-package helm
;;   :ensure t
;;   :init
;;   (setq
;;    helm-M-x-fuzzy-match t
;;    helm-mode-fuzzy-match t
;;    helm-buffers-fuzzy-matching t
;;    helm-recentf-fuzzy-match t
;;    helm-locate-fuzzy-match t
;;    helm-semantic-fuzzy-match t
;;    helm-imenu-fuzzy-match t
;;    helm-completion-in-region-fuzzy-match t
;;   )
;;   :config
;;   (helm-mode 1)
;;   (helm-adaptive-mode t)
;;   :bind
;;   ("C-c p s g" . helm-do-ag-project-root)
;;   ("M-x" . helm-M-x)
;;   ("C-x C-f" . helm-find-files)
;;   ("C-x b" . helm-mini)
;;   ("C-x C-r" . helm-recentf)
;;   )

;; (use-package helm-ag
;;   :ensure t)

;; (use-package helm-rg
;;   :ensure t)

;; (use-package helm-flycheck
;;   :ensure t)

;; (use-package helm-projectile
;;   :ensure t
;;   :config

;; (use-package helm-smex
;;   :ensure t)

;;   (defun helm-projectile-ag-with-defaults (&optional additional-options)
;;     "Wrapper for `helm-projectile-ag' with default options."
;;     (interactive (if current-prefix-arg
;;                      (list (helm-read-string "Additional options: " "" 'helm-ag--extra-options-history))
;;                    nil))
;;     (let ((default-options "--ignore *.mar --ignore *.sql --ignore *.pt --ignore *openapi_sdk* --ignore *.txt --ignore *.json"))
;;       (helm-projectile-ag (concat default-options " " additional-options))))

;;   (defun helm-projectile-ag-thing-at-point ()
;;     (interactive)
;;     (mark-inside-or-not nil)
;;     (helm-projectile-ag-with-defaults)
;;     (deactivate-mark)
;;     )
;;   )

(use-package hideshow
  :ensure t)

;; One of the following was making ivy slow:
;; historian
;; prescient
;; rich
;;
;; Figure it out and go back to the others...

(use-package ivy-historian
  :ensure t)

(use-package ivy
  :ensure t
  ;; :after helm
  :init
  (historian-mode +1)
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-height 20)
  (setq ivy-fixed-height-minibuffer t)
  ;; (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; (ivy-prescient-mode)
  (add-to-list 'ivy-ignore-buffers "\\*Help")
  ;; (add-to-list 'ivy-ignore-buffers "\\*helm")
  )

(use-package ivy-prescient
  :ensure t
  :config
  (prescient-persist-mode t)
  )

(use-package prescient
  :diminish
  :config
  )

;; (use-package ivy-rich
;;   :ensure t
;;   :init (ivy-rich-mode 1)
;;   :config
;;   (setq ivy-rich-parse-remote-buffer nil)
;;   )

;; (use-package ivy-rich
;;   :ensure t
;;   :config
;;   (ivy-rich-mode t)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line) ; Recommended in Github repo
;;     (setq ivy-rich-parse-remote-buffer nil ; https://github.com/Yevgnen/ivy-rich/issues/47
;;           ivy-rich-parse-remote-file-path nil
;;           ivy-rich-path-style (quote full))
;;   ;; (ivy-rich-mode 0)
;;     )

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode +1)
  (key-chord-define-global "jk" 'ryo-modal-on)
  (key-chord-define-global "fk" 'kill-current-buffer)
  (key-chord-define-global "fs" 'save-and-enter-ryo)
  (key-chord-define-global "FS" 'save-and-enter-ryo)
  (key-chord-define-global "fg" 'magit-status)
  )

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(setq kill-ring-max 500)

(setq-default display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
(setq display-line-numbers-type t)
;; (setq display-line-numbers-type nil)
(global-display-line-numbers-mode 1)

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

(use-package ace-mc
  :ensure t) ;; please review this

(require 'org)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

(setq org-support-shift-select t)
(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "|" "DONE" "POSTPONED")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("IN PROGRESS" . "#FF8000")
        ("DONE" . (:foreground "grey" :weight bold))
        ("POSTPONED" . (:foreground "grey" :weight bold))
        ))

(setq org-startup-folded t)

(use-package org-present
  :ensure t)

(use-package visual-fill-column
  :ensure t
  :config
  (setq visual-fill-column-width 110)
  (setq visual-fill-column-center-text t)
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "-i --simple-prompt")

(eval-after-load "ox-latex"
  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-latex-listings t)


(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  )

(use-package rg
  :ensure t)

(setq recentf-max-saved-items 300)
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

(use-package smart-newline
  :ensure t
  :config
  (smart-newline-mode 1)
  )

(setq savehist-file "~/.emacs.d/savehist"
      history-length 300)

(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saveplace")

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



;; (use-package helm-tramp
;;   :ensure t)
;; (add-hook 'helm-tramp-pre-command-hook '(lambda () (projectile-mode 0)))
;; (add-hook 'helm-tramp-quit-hook '(lambda () (projectile-mode 1)))



(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-chunksize 500)
;; (use-package kubernetes-tramp
;;   :ensure t)
;; (use-package kubernetes-helm
;;   :ensure t)
;; (use-package kubernetes
;;   :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history 1)
  (setq undo-tree-visualizer-timestamps 1)
  )

;; (use-package vimish-fold
;;   :ensure t)

;; (use-package which-key
;;   :ensure t
;;   :init
;;   (setq which-key-separator " ")
;;   (setq which-key-prefix-prefix "+")
;;   :config
;;   (which-key-mode 1)
;;   )

;; (use-package wgrep
;;   :ensure t
;;   )

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode)
  )

(winner-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PYTHON AND PROJECTS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blacken
  :ensure t
  :config
  (setq blacken-skip-string-normalization nil)
  (setq blacken-line-length 160)
  (setq blacken-allow-py36 nil)
  (add-hook 'python-mode-hook 'blacken-mode)
  ;; (remove-hook 'python-mode-hook 'blacken-mode)
  )

(use-package company
  :ensure t
  :config
  (setq company-backends '((company-capf company-files)))
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

  (add-to-list 'ivy-ignore-buffers "\\*company")

  ;; (setq company-transformers '(company-sort-by-backend-importance))
  )


(use-package company-box
  :hook (company-mode . company-box-mode)
  :ensure t)

(use-package company-jedi
  :ensure t
  ;; (defun my/python-mode-hook ()
  ;;   (add-to-list 'company-backends 'company-jedi))

  ;; (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

(use-package company-prescient
  :after company
  :ensure t
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode)
  )

;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion))))
;;   :ensure t)



;; (use-package corfu
;;   :ensure t
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-quit-at-boundary 'separator)
;;   ;; (corfu-quit-no-match 'separator)
;;   (corfu-quit-no-match 'separator)
;;   (corfu-echo-documentation 0.25)
;;   (corfu-preview-current t)
;;   (corfu-preselect 'first)
;;   ;; (corfu-on-exact-match nil)
;;   (corfu-preview-current 'insert)
;;   ;; (corfu-scroll-margin 5)
;;   ;; :init
;;   ;; (setq completion-cycle-threshold 3)
;;   :bind (:map corfu-map
;;               ("M-SPC" . corfu-insert-separator)
;;               ("C-g" . corfu-quit)
;;               ("TAB" . corfu-complete)
;;               ("RET" . corfu-insert)
;;               )
;;   :config
;;   (global-corfu-mode t)
;;   (corfu-history-mode t)
;;   (corfu-echo-mode t)
;;   )


;; (use-package corfu-prescient
;;   :ensure t)


(use-package cython-mode
  :defer t
  )

(use-package docker
  :defer t
  :bind
  ("C-c d" . docker)
  )
;; (use-package docker-tramp
;;   :after (docker tramp)
;;   :ensure t
;;   )

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  ;; (setq elpy-rpc-timeout 10)
  ;; (setq elpy-rpc-backend "jedi")
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (add-to-list 'ivy-ignore-buffers "\\*Flycheck")
  )

;; (use-package importmagic
;;     :ensure t
;;     :config
;;     (add-hook 'python-mode-hook 'importmagic-mode)
;;     (setq importmagic-style-configuration-alist '((multiline . parentheses)
;;                                                   (max_columns . 200)))
;;     (add-to-list 'ivy-ignore-buffers "\\*epc con")
;;     (setq importmagic-be-quiet t)
;;     )

;; isort
(use-package py-isort
  :ensure t
  :config
  (add-hook 'before-save-hook 'py-isort-before-save)
  ;; (remove-hook 'before-save-hook 'py-isort-before-save)
  ;; (setq py-isort-options '("--line-length=160 --profile=black"))
  )

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

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c m" . magit-blame-addition)
  :config
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-log "-A"
      '("-1" "First parent" "--first-parent")))
  (add-to-list 'ivy-ignore-buffers "magit-process:")
  (add-to-list 'ivy-ignore-buffers "magit-diff:")
  (add-to-list 'ivy-ignore-buffers "magit:")
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq projectile-switch-project-action 'magit-status)
  )

;; (use-package forge
;;   :ensure t
;;   :after magit
;;   :config
;;   (with-eval-after-load 'forge
;;     (add-to-list 'forge-alist
;;                  '("git.legartis.ai" "git.legartis.ai/api/v4" "git.legartis.ai" forge-gitlab-repository)))
;;   (setq auth-source-debug 'trivia)

  ;; (defclass forge-gitlab-http-repository (forge-gitlab-repository)
  ;;   ((issues-url-format         :initform "http://%h/%o/%n/issues")
  ;;    (issue-url-format          :initform "http://%h/%o/%n/issues/%i")
  ;;    (issue-post-url-format     :initform "http://%h/%o/%n/issues/%i#note_%I")
  ;;    (pullreqs-url-format       :initform "http://%h/%o/%n/merge_requests")
  ;;    (pullreq-url-format        :initform "http://%h/%o/%n/merge_requests/%i")
  ;;    (pullreq-post-url-format   :initform "http://%h/%o/%n/merge_requests/%i#note_%I")
  ;;    (commit-url-format         :initform "http://%h/%o/%n/commit/%r")
  ;;    (branch-url-format         :initform "http://%h/%o/%n/commits/%r")
  ;;    (remote-url-format         :initform "http://%h/%o/%n")
  ;;    (create-issue-url-format   :initform "http://%h/%o/%n/issues/new")
  ;;    (create-pullreq-url-format :initform "http://%h/%o/%n/merge_requests/new")
  ;;    (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))

  ;; (add-to-list 'ghub-insecure-hosts "git.legartis.ai/api/v4")
  ;; )

(use-package jupyter
  :ensure t)
;;(use-package ob-ipython
;;  :ensure t)

(setenv "EDITOR" "emacsclient")

(use-package pip-requirements
  :hook ((pip-requirements-mode . company-mode))
  :ensure t)

(use-package projectile
  :ensure t
  :init
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-completion-system 'helm)
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  )

(use-package python-pytest
  :ensure t)

(use-package pyvenv
  :ensure t)

(use-package virtualenvwrapper
  :ensure t)

(venv-initialize-interactive-shells)
(defvar python-environment-directory)
(setq python-environment-directory "~/.virtualenvs/")
(setq venv-location "~/.virtualenvs/")
(venv-initialize-eshell)

;; (use-package auto-virtualenv
;;   :ensure t
;;   :config
;;   (setq auto-virtualenv-dir "~/.virtualenvs")
;;   ;; the config that makes my life hell:
;;   ;; (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;;   ;; (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
;;   (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)
;;   )

(use-package yasnippet-snippets
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.conf/snippets"))
  (yas-reload-all)
  )

;;;;;;;;;;;;;;;;;;;;;;
;; Other Languages ;;;
;;;;;;;;;;;;;;;;;;;;;;

(setq-default c-basic-offset 4)

;; dhall-mode highlight the syntax and run dhall format on save
(use-package dhall-mode
  :ensure t
  :config
  (setq
    ;; uncomment the next line to disable automatic format
    ;; dhall-format-at-save nil

    ;; comment the next line to use unicode syntax
    dhall-format-arguments (\` ("--ascii"))

    ;; header-line is obsoleted by lsp-mode
    dhall-use-header-line nil))

;; lsp-mode provides the lsp client and it configure flymake to explain errors
;; (use-package lsp-mode
;;   :ensure t
;;   :init (setq lsp-keymap-prefix "C-c l")
;;   :hook ((dhall-mode . lsp))
;;   :commands lsp)

;; (use-package eglot
;;   :ensure t)

;; (use-package kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview)
;;   :config
;;   (setq kubernetes-poll-frequency 3600
;;         kubernetes-redraw-frequency 3600))
(use-package kubectx-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  )

(use-package poly-ansible
  :ensure t
  )
(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode))
  )
(use-package tide
  :ensure t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  (setq tide-format-options '(:indentSize 2))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (setq-default typescript-indent-level 2)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  )
(use-package xref-js2
  :ensure t)
(use-package typescript-mode
  :mode (("\\.ts$" . typescript-mode))
  :ensure t)

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
  (defun my-web-mode-save-hook ()
    (when (eq major-mode 'web-mode)
      (web-mode-buffer-indent)))

  (add-hook 'after-save-hook 'my-web-mode-save-hook)
  )
(use-package markdown-mode
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
  (setq yaml-indent-offset 2)
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


;;;;;;;;;;;;;;;;;
;;; SHORTCUTS ;;;
;;;;;;;;;;;;;;;;;

(bind-keys*
 ("M-o" . ace-window)
 )

;; (define-key helm-find-files-map (kbd "C-j") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-u") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-i") 'helm-next-line)
;; (define-key helm-find-files-map (kbd "C-o") 'helm-previous-line)
;; (define-key helm-find-files-map (kbd "C-p") 'helm-execute-persistent-action)
;; (define-key helm-find-files-map (kbd "C-;") 'helm-execute-persistent-action)
;; (define-key helm-buffer-map (kbd "C-i") 'helm-next-line)
;; (define-key helm-buffer-map (kbd "C-o") 'helm-previous-line)
;; (define-key helm-read-file-map (kbd "C-j") 'helm-find-files-up-one-level)
;; (define-key helm-read-file-map (kbd "C-u") 'helm-find-files-up-one-level)
;; (define-key helm-read-file-map (kbd "C-i") 'helm-next-line)
;; (define-key helm-read-file-map (kbd "C-o") 'helm-previous-line)
;; (define-key helm-read-file-map (kbd "C-p") 'helm-execute-persistent-action)
;; (define-key helm-read-file-map (kbd "C-;") 'helm-execute-persistent-action)
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

(global-set-key (kbd "'") 'quote-up-or-replace)
(global-set-key (kbd "\"") 'double-quote-up-or-replace)
(global-set-key (kbd "~") 'tild-up-or-replace)


(use-package hydra
  :ensure t
  :config
  (defhydra hydra-smerge (:color pink
                                 :hint nil
                                 :pre (smerge-mode 1)
                                 ;; Disable `smerge-mode' when quitting hydra if
                                 ;; no merge conflicts remain.
                                 :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))

  (defhydra hydra-osm (:color purple
                              :hint nil
                              :pre (osm-home)
                              :post (kill-buffer))
    ("u" osm-left)
    ("i" osm-up)
    ("o" osm-down)
    ("p" osm-right)

    ("d" osm-zoom-in)
    ("f" osm-zoom-out)

    ("s" osm-search)
    ("g" osm-goto)
    )

  (defhydra hydra-flymake (:color purple
                                  :hint nil)
    "
Flymake hydra
-------------
j -- next
; -- prev
"
    ("j" flymake-goto-prev-error)
    (";" flymake-goto-next-error)
    ("q" nil "cancel" :color blue)
    )
  )

(use-package ivy-hydra
  :ensure t)


(load "~/.emacs.conf/gptel-custom.el" t)


(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :bind ("<escape>" . ryo-modal-mode)
  :bind ("C-c C-r" . ryo-modal-mode)
  :bind ("M-;" . ryo-modal-mode)
  :config
  (add-hook 'text-mode-hook #'ryo-modal-mode)
  (add-hook 'prog-mode-hook #'ryo-modal-mode)
  (add-hook 'fundamental-mode-hook #'ryo-modal-mode)
  (add-hook 'special-mode-hook #'ryo-modal-mode)
  (add-hook 'magit-status-mode-hook #'ryo-modal-off)
  (add-hook 'conf-unix-mode-hook #'ryo-modal-mode)
  (setq ryo-modal-default-cursor-color "#859900")
  (setq ryo-modal-cursor-color "#859900")
  (setq-default cursor-type 'bar)
  (setq ryo-modal-cursor-type 'box)
  (ryo-modal-mode)

  (ryo-modal-keys
   ("q" my-change-word-or-region)
   ("w" my-backward-change-word-or-region)
   ;; ("q" kill-word)
   ;; ("w" backward-kill-word)
   ("Q" delete-forward-char)
   ("W" backward-delete-char-untabify)
   ("e" highlight-symbol-next)
   ("E" highlight-symbol-prev)
   ("r" avy-goto-word-1-below)
   ("R" avy-goto-word-1-above)
   ("t" vi-open-line-below)
   ("T" vi-open-line-above)
   ("y" other-window)
   ("u" backward-char)
   ("i" next-line)
   ("o" previous-line)
   ("p" forward-char)
   ("[" square-bracket-up-or-replace)
   ("{" curly-bracket-up-or-replace)

   ("A" comment-paragraph) ;; use it
   ("S" swiper-thing-at-point) ;; use it
   ;; ("D" ) ;; think
   ("F" harpoon-quick-menu-hydra)
   ("g" keyboard-quit)
   ("G" end-of-buffer)
   ("h" move-beginning-of-line)
   ("H" beginning-of-line-or-indentation)
   ("j" my-backward-word)
   ("J" backward-sexp)
   ("k" forward-paragraph)
   ("K" scroll-up-and-recenter)
   ("l" backward-paragraph)
   ("L" scroll-down-and-recenter)
   (";" my-forward-word)
   (":" forward-sexp)
   ("'" move-end-of-line)
   ("\"" double-quote-up-or-replace)

   ("z" undo-tree-undo)
   ("Z" undo-tree-redo)
   ("x" kill-whole-line-or-region)
   ("X" kill-thing-at-point)  ;; think about it
   ("c" copy-whole-line-or-region)
   ("C" copy-thing-at-point)
   ("v" delete-and-paste)
   ("V" paste-in-new-line)
   ("b" er-switch-to-previous-buffer)  ;; use it
   ("n" recenter-top-bottom)
   ;; ("n" reposition-window)
   ("m" ryo-modal-repeat)  ;; use it as well!
   ;; ("," awesome-tab-backward-tab)
   ;; ("," awesome-tab-backward-tab)
   ("." centaur-tabs-forward)
   ("," centaur-tabs-backward)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("/" move-end-of-line)
   ("?" dumb-jump-back)

   ;; ("`" bookmark-jump) ;; useful but does it warrant 1-key sequence?
   ("~" tild-up-or-replace)
   ("!" flycheck-list-errors)
   ("#" highlight-symbol-query-replace)
   ("$" query-replace-thing-at-point-or-selection)
   ("%" query-replace)
   ("(" insert-parentheses)
   ("-" delete-horizontal-and-vertical-space)
   ("_" delete-horizontal-and-vertical-space-but-leave-one-space)
   ("=" er/expand-region)
   ("+" mark-paragraph)  ;; use me ;; or not, really, what's the point...
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
   "a" '(
         ("q" my-change-word-or-region)
         ("w" my-backward-change-word-or-region)
         ("e" highlight-symbol)
         ("r" blacken-buffer)
         ;; ("t")

         ("Q" my-substitute-word-or-region)
         ("W" my-backward-substitute-word-or-region)

         ("a" comment-line)
         ;; ("s" helm-projectile-rg)
         ("s" counsel-projectile-ag)
         ("d" copy-full-path-to-kill-ring)
         ("D" copy-folder-path-to-kill-ring)
         ("G" gptel-menu) 
         ;; ("h" ) unused!!!
         ("j" recentf)
         ("k" save-buffers-kill-terminal)
         ("l" bookmark-jump)
         (";" ibuffer)
         ("'" string-inflection-kebab-case)

         ("y" change-inside-string-or-not)
         ("u" change-inside-or-not)
         ("i" change-inner-with-paren)
         ("o" change-inner-with-square)
         ("p" change-inner-with-curly)

         ("Y" substitute-inside-string-or-not)
         ("U" substitute-inside-or-not)
         ("I" substitute-inner-with-paren)
         ("O" substitute-inner-with-square)
         ("P" substitute-inner-with-curly)

         ("m" change-outside-or-not)
         ("," change-outer-with-paren)
         ("." change-outer-with-square)
         ("/" change-outer-with-curly)

         ("M" substitute-outside-or-not)
         ("<" substitute-outer-with-paren)
         (">" substitute-outer-with-square)
         ("?" substitute-outer-with-curly)

         ("fu" substitute-inside-or-not-with-kill-ring)
         ("fi" substitute-inner-with-paren-with-kill-ring)
         ("fo" substitute-inner-with-square-with-kill-ring)
         ("fp" substitute-inner-with-curly-with-kill-ring)

         ("fm" substitute-outside-or-not-with-kill-ring)
         ("f," substitute-outer-with-paren-with-kill-ring)
         ("f." substitute-outer-with-square-with-kill-ring)
         ("f/" substitute-outer-with-curly-with-kill-ring)
         
         ("gj" gptel-send-to-gpt4--short)
         ("gk" gptel-send-to-claude-opus--short)
         ("gl" gptel-send-to-claude-haiku--short)

         ("gu" gptel-send-to-gpt4--general)
         ("gi" gptel-send-to-claude-opus--general)
         ("go" gptel-send-to-claude-haiku--general)
         
         ("gm" gptel-send-to-gpt4--continue)
         ("g," gptel-send-to-opus--continue)
         ("g." gptel-send-to-haiku--continue)
         )
   )

  (ryo-modal-key
   "s" '(
         ("q" my-copy-word-or-region)
         ("w" my-backward-copy-word-or-region)
         ("e" add-correct-start-of-commit)
         ("r" autoimport)
         ;; ("R" importmagic-save-revert-and-fix)

         ("y" copy-inside-string-or-not)
         ("u" copy-inside-or-not)
         ("i" copy-inner-with-paren)
         ("o" copy-inner-with-square)
         ("p" copy-inner-with-curly)

         ("s" swiper-region)
         ("d" hydra-smerge/body)
         ;; ("g" ) unused!!!
         ;; ("h" ) unused!!!
         ("j" counsel-projectile)
         ;; ("j" projectile-switch-to-buffer)
         ("k" kill-all-buffers-but-scratch)
         ;; ("l" venv-workon)
         ("l" pyvenv-workon)
         ;; ("'" ) unused!!!
         ("'" string-inflection-upcase)

         ("m" copy-outside-or-not)
         ("," copy-outer-with-paren)
         ("." copy-outer-with-square)
         ("/" copy-outer-with-curly)
         )
   )

  (ryo-modal-key
   "d" '(
         ("q" my-cut-word-or-region)
         ("w" my-backward-cut-word-or-region)
         ("e" projectile-replace-regexp)
         ("r" projectile-replace)

         ("y" cut-inside-string-or-not)
         ("u" cut-inside-or-not)
         ("i" cut-inner-with-paren)
         ("o" cut-inner-with-square)
         ("p" cut-inner-with-curly)

         ;; ("a" helm-projectile-ag-thing-at-point)
         ("A" insert-class)
         ;; ("s" helm-projectile-ag-with-defaults)
         ("s" counsel-projectile-ag)
         ("d" projectile-dired)  ;; probably duplicates dired-jump
         ("f" counsel-projectile-find-file)
         ("F" invalidate-cache-and-counsel-projectile-find-file)
         ;; ("g" helm-projectile-rg)
         ("g" counsel-projectile-ag-at-point)
         ("h" counsel-projectile)
         ("j" counsel-projectile-switch-to-buffer)
         ("k" projectile-kill-buffers)
         ("l" awesome-tab-switch-group)
         (";" xref-pop-marker-stack)
         ("'" string-inflection-camelcase)

         ("b" superword-on)
         ("B" superword-off)
         ("n" subword-on)
         ("N" subword-off)

         ("m" cut-outside-or-not)
         ("," cut-outer-with-paren)
         ("." cut-outer-with-square)
         ("/" cut-outer-with-curly)

         ;; almost useless but hey, it's not like I'm loosing anything
         ("!" projectile-run-shell-command-in-root)
         ("%" projectile-run-async-shell-command-in-root)
         )
   )

  (ryo-modal-key
   "f" '(
         ("q" my-mark-word)
         ("w" my-backward-mark-word)
         ("e" magit-diff-develop)
         ("r" avy-goto-line)
         ("t" elpy-multiedit-python-symbol-at-point)

         ("y" mark-inside-string-or-not)
         ("u" mark-inside-or-not)
         ("i" mark-inner-with-paren)
         ("o" mark-inner-with-square)
         ("p" mark-inner-with-curly)

         ("a" goto-last-change)  ;; think about it
         ("s" save-buffer)
         ("S" projectile-save-project-buffers)  ;; perfect, I don't use it often and I remember it
         ("d" dired-jump)
         ("f" counsel-find-file)
         ;; ("f" helm-find-files)
         ("g" magit-status)
         ("h" mark-whole-buffer)
         ("j" ivy-switch-buffer)
         ;; ("j" helm-mini)
         ("k" kill-current-buffer)  ;; useful but maybe somewhere else?
         ("l" projectile-switch-project)
         (";" xref-find-definitions)
         (":" xref-find-references-at-point)
         ("'" string-inflection-underscore)

         ("z" avy-zap-up-to-char-dwim)
         ("Z" avy-zap-to-char-dwim)
         ("x" counsel-M-x)
         ;; ("x" helm-M-x)
         ;; ("c" save-buffers-kill-terminal)
         ("v" counsel-yank-pop)
         ("V" paste-from-kill-ring-new-line)
         ;; ("b" imenu)  ;; think about it
         ("n" goto-line)

         ("m" mark-outside-or-not)
         ("," mark-outer-with-paren)
         ("." mark-outer-with-square)
         ("/" mark-outer-with-curly)

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

  (ryo-modal-major-mode-keys
   'python-mode
   ("U" magic-elpy-nav-backward-method)
   ("I" magic-elpy-nav-forward-class)
   ("O" magic-elpy-nav-backward-class)
   ("P" magic-elpy-nav-forward-method)

   ("\\" er/mark-python-statement)  ;; use me

   ("M-o" elpy-nav-move-line-or-region-up)  ;; this is not useful
   ("M-i" elpy-nav-move-line-or-region-down)  ;; this is not useful

   ("a"
    (
     ("t" python-add-return)
     )
    )

   ("s"
    (
     ("e" python-add-breakpoint)
     ("t" python-add-pass)
     )
    )

   ("d"
    (
     ("t" autoflake)
     ;; ("t" projectile-toggle-between-implementation-and-test)  ;; useful when it works

     ("z" get-test-string)
     ("x" get-class-string)
     )
    )

   ;; ("f"
   ;;  (
   ;;   ("J" magic-elpy-nav-backward-method)
   ;;   (":" magic-elpy-nav-forward-method)
   ;;   )
   ;;  )
   )

  (ryo-modal-major-mode-keys
   'emacs-lisp-mode
   ("I" forward-sexp)
   ("O" backward-sexp)

   ("f"
    (
     ("e" eval-last-sexp)
     ("E" eval-buffer)
     )
    )
   )

  (ryo-modal-major-mode-keys
   'haskell-mode
   ("f"
    (
     (";" haskell-mode-jump-to-def)
     )
    )
   )

  (ryo-modal-major-mode-keys
   'org-present-mode
   ("q" org-present-quit)
   ("<left>" org-present-prev)
   ("<right>" org-present-next)
   ("<home>" org-present-beginning)
   ("<end>" org-present-end)
   )
  )

