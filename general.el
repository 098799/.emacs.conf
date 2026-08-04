;;; general.el --- Main Emacs configuration -*- lexical-binding: t -*-

;; Suppress annoying warnings
(setq warning-suppress-types '((files) (defvaralias) (straight) (comp)))
(setq warning-suppress-log-types '((files) (defvaralias) (straight) (comp)))
(setq warning-minimum-level :error)

;; Silence native-comp warnings (e.g., obsolete macro warnings from packages)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors 'silent))

;; Tree-sitter configuration
(when (treesit-available-p)
  ;; Grammar sources for automatic installation
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")))
  ;; Auto-install missing grammars
  (dolist (lang '(python))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang)))
  ;; Remap python-mode to python-ts-mode
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

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
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Uncomment to profile startup: M-x use-package-report after startup
;; (setq use-package-compute-statistics t)

;; Don't check MELPA on every startup - huge time saver
(setq quelpa-update-melpa-p nil)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Only fetch quelpa-use-package if not installed
(unless (package-installed-p 'quelpa-use-package)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))

(require 'quelpa-use-package)

;; (use-package auto-package-update
;;    :ensure t
;;    :config
;;    (setq auto-package-update-delete-old-versions t
;;          auto-package-update-interval 4)
;;    (auto-package-update-maybe))

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
  :defer 1
  :ensure t)

(use-package beacon
  :ensure t
  :defer 1
  :config
  (beacon-mode 1)
  ;; (set-face-background hl-line "gray13")
  ;; (global-hl-line-mode +1)
  )

(use-package command-log-mode
  :ensure t
  :defer t)

(use-package default-text-scale
  :ensure t
  :defer 1
  :config
  (default-text-scale-mode t)
  )

(use-package emojify
  :ensure t
  :defer 1)

;; nerd-icons must load before doom-modeline
(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :after nerd-icons
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
  (setq doom-modeline-project-detection 'auto))


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
  :defer 1
  :config
  (use-package auto-highlight-symbol
    :ensure t
    :config
    (global-auto-highlight-symbol-mode t)
    (setq ahs-idle-interval 0.0)
    ;; Use only background color, no bold (prevents font jiggle with flycheck)
    (set-face-attribute 'ahs-face nil :weight 'normal :underline nil)
    (set-face-attribute 'ahs-plugin-whole-buffer-face nil :weight 'normal :underline nil)))

(use-package highlight-indentation
  :ensure t
  )

(use-package nav-flash
  :ensure t
  :defer 1
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

;; (use-package solarized-theme
;;   :ensure t
;;   ;; :config
;;   ;; (load-theme 'solarized-light t)
;;   ;; (load-theme 'solarized-selenized-dark t)
;;   ;; (load-theme 'solarized-gruvbox-light t)
;;   ;; (load-theme 'solarized-gruvbox t)
;;   )

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

;; Hand-written themes live here: win95 and win311, the Emacs half of
;; ~/dotfiles/win95-pkg. `theme`/`w95 on` load them by name, so the directory
;; has to be searchable before either can run.
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" (file-name-directory
                                          (or load-file-name buffer-file-name "~/.emacs.conf/"))))

(use-package doom-themes
  :ensure t
  :config
  ;; gnus (Emacs 30+) flipped its group faces: gnus-group-news-low now
  ;; inherits gnus-group-news-low-empty. doom-themes-base still ships the
  ;; -empty face inheriting the other way, and once both specs sit on the
  ;; symbol EVERY load-theme dies with "Face inheritance results in
  ;; inheritance cycle: gnus-group-news-low" -- which is what silently broke
  ;; `theme`/w95-switch. Drop doom's entry; the gnus default takes over.
  (require 'doom-themes-base)
  (setq doom-themes-base-faces
        (assq-delete-all 'gnus-group-news-low-empty doom-themes-base-faces))
  ;; `w95 on`/`off` records the desktop's theme in ~/.config/w95/emacs-theme,
  ;; because unlike GTK or alacritty there is no file Emacs re-reads and
  ;; `theme` only pokes a running server -- so without this an Emacs started
  ;; after the switch would come up gruvbox on a Windows desktop. Absent or
  ;; unloadable file means gruvbox, so nothing here can cost us a startup.
  (let* ((f "~/.config/w95/emacs-theme")
         (want (and (file-readable-p f)
                    (with-temp-buffer
                      (insert-file-contents f)
                      (let ((s (string-trim (buffer-string))))
                        (and (not (string-empty-p s)) (intern s)))))))
    (unless (and want
                 (condition-case err
                     (progn (load-theme want t) t)
                   (error (message "w95 theme %s failed (%S); using gruvbox" want err)
                          nil)))
      (load-theme 'doom-gruvbox t))))

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (load-theme 'modus-vivendi-tinted t)
;;   )

(use-package rainbow-delimiters
  :ensure t
  :defer 1
  :config
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'python-ts-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (show-paren-mode t)
  (setq show-paren-style 'expression)
  )

(use-package rainbow-mode
  :ensure t)


(global-visual-line-mode 1)

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
  :ensure t
  :defer 1)

(use-package avy-zap
  :ensure t
  )

(use-package better-defaults
  :ensure t
  :defer 1)

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

;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "zerolfx/copilot.el"
;;                    :branch "main"
;;                    :files ("dist" "*.el"))
;;   :config
;;   (add-hook 'python-mode-hook 'copilot-mode)
;;   (add-hook 'python-ts-mode-hook 'copilot-mode)
;;   ;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "C-e") 'copilot-accept-completion)
;;   )

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
  :defer 1
  :config
  ;; Add kubectl pods to counsel-tramp candidates
  (defun counsel-tramp-kubernetes-pods ()
    "Get list of kubernetes pods for current context/namespace."
    (when (executable-find "kubectl")
      (let ((pods '()))
        (cl-loop for line in (cdr (ignore-errors
                                     (apply #'process-lines "kubectl"
                                            (list "get" "pods" "--no-headers" "-o" "custom-columns=:metadata.name"))))
                 do (when (and line (not (string-empty-p line)))
                      (push (concat "/kubectl:" line ":/") pods)))
        pods)))

  ;; Advice to add kubectl pods to the candidate list
  (defun counsel-tramp--add-kubectl-pods (orig-fun &optional file)
    "Advice to add kubectl pods to counsel-tramp candidates."
    (let ((base-candidates (funcall orig-fun file))
          (kubectl-candidates (counsel-tramp-kubernetes-pods)))
      (append base-candidates kubectl-candidates)))

  (advice-add 'counsel-tramp--candidates :around #'counsel-tramp--add-kubectl-pods)
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

  (setq gptel-log-level 'debug)
  )

;; (quelpa '(eat :fetcher git
;;               :url "https://codeberg.org/akib/emacs-eat"
;;               :files ("*.el" ("term" "term/*.el") "*.texi"
;;                       "*.ti" ("terminfo/e" "terminfo/e/*")
;;                       ("terminfo/65" "terminfo/65/*")
;;                       ("integration" "integration/*")
;;                       (:exclude ".dir-locals.el" "*-tests.el"))))

;; (use-package claude-code
;;   :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
;;                    :files ("*.el" (:exclude "images/*")))
;;   :bind-keymap
;;   ("C-c c" . claude-code-command-map) ;; or your preferred key
;;   :config
;;   (setq claude-code-terminal-backend 'eat)
;;   (setq claude-code-program "~/.nvm/versions/node/v18.20.5/bin/claude")
;;   (claude-code-mode))
 

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
        ("c" . dired-ranger-copy)
        ("P" . dired-ranger-paste)
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

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

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
  :defer 1
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
  :ensure t
  :defer 1)

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


(use-package harpoon
  :ensure t)


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
  (setq ivy-use-virtual-buffers t)  ;; show recent files in buffer switch
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

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  )

;; Consult - modern search commands (works with ivy too, not just vertico)
(use-package consult
  :ensure t
  :defer t
  :commands (consult-line consult-ripgrep consult-imenu consult-buffer consult-outline)
  :config
  (setq consult-preview-key "M-.")  ;; preview with M-.
  (setq consult-async-min-input 1)  ;; start searching after 1 char
  ;; Use projectile for project root detection (not project.el)
  (setq consult-project-function
        (lambda (_)
          (when (fboundp 'projectile-project-root)
            (projectile-project-root))))
  )

(use-package marginalia
  :ensure t
  :defer 1
  :config
  (marginalia-mode 1))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; context menu on current target
   ("C-;" . embark-dwim)        ;; "do what I mean" on target
   ("C-h B" . embark-bindings)) ;; show bindings for current context
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)  ;; auto-save after applying changes
  (setq wgrep-change-readonly-file t))

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
  :ensure t
  :defer 1)

;; (require 'org)  ;; defer org loading - it's slow

;; (use-package org-modern
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'org (global-org-modern-mode))
  
;;   ;; (modify-all-frames-parameters
;;   ;;  '((right-divider-width . 40)
;;   ;;    (internal-border-width . 40)))
  
;;   ;; (dolist (face '(window-divider
;;   ;;                 window-divider-first-pixel
;;   ;;                 window-divider-last-pixel))
;;   ;;   (face-spec-reset-face face)
;;   ;;   (set-face-foreground face (face-attribute 'default :background)))
;;   ;; (set-face-background 'fringe (face-attribute 'default :background))
  
;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-ellipsis "…"

;;    ;; Agenda styling
;;    org-agenda-tags-column 0
;;    org-agenda-block-separator ?─
;;    org-agenda-time-grid
;;    '((daily today require-timed)
;;      (800 1000 1200 1400 1600 1800 2000)
;;      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;    org-agenda-current-time-string
;;    "◀── now ─────────────────────────────────────────────────")

;;   (global-org-modern-mode)
;;   )

;; (setq org-src-fontify-natively t)
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
  :ensure t
  :defer 1)

(setq recentf-max-saved-items 300)
(setq recentf-auto-cleanup 'mode)  ;; clean up stale entries on mode change
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

(use-package smart-newline
  :ensure t
  :config
  (smart-newline-mode 1)
  )

(setq savehist-file "~/.emacs.d/savehist"
      history-length 300)

(setq save-place-file "~/.emacs.d/saveplace")
(save-place-mode 1)

(use-package string-inflection
  :ensure t
  )

(use-package swiper
  :ensure t
  )

(use-package switch-buffer-functions
  :ensure t
  )

(require 'tramp)
(setq tramp-verbose 1)  ;; errors only (use 2+ for debugging)
(setq tramp-default-method "ssh")

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-chunksize 500)

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024))

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :machine "server")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))
;; some copy-pasted stuff, sus


;; kubernetes-tramp is obsolete - use built-in tramp-container instead
;; (use-package kubernetes-tramp
;;   :ensure t)
(require 'tramp-container)
;; (use-packagekubernetes-helm
;;   :ensure t)
;; (use-package kubernetes
;;   :ensure t)

(use-package undo-tree
  :ensure t
  :commands (undo-tree-undo undo-tree-redo undo-tree-visualize)
  :init
  (defvar global-undo-tree-mode-buffers nil
    "List of buffers with undo-tree-mode enabled (fixes void-variable error).")
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  ;; Store undo history in a central location instead of next to files
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history/")))
  )

;; (use-package vimish-fold
;;   :ensure t)

(use-package which-key
  :ensure t
  :defer 1
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1)
  ;; Descriptions for ryo-modal prefix keys
  (which-key-add-key-based-replacements
    "a" "change/substitute"
    "a g" "gptel"
    "a f" "substitute w/kill-ring"
    "s" "copy/search"
    "d" "cut/project"
    "f" "file/find/mark"
    "f 5" "rectangle"
    "\\" "toggle modes"
    "`" "pop mark"
    "@" "call macro"))

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h F" . helpful-function))
  :config
  (add-to-list 'ivy-ignore-buffers "\\*helpful"))

;; Show current function in modeline
(which-function-mode 1)

;; (use-package wgrep
;;   :ensure t
;;   )

(use-package whitespace-cleanup-mode
  :ensure t
  :defer 1
  :config (global-whitespace-cleanup-mode)
  )

(winner-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PYTHON AND PROJECTS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ruff — the only python linter and formatter.
;;
;; The version is not a local choice.  It is whatever the astral-sh/ruff-pre-commit
;; rev in the *nearest ancestor* .pre-commit-config.yaml says, which is the single
;; source of truth the whole toolchain reads: the Legartis CI lint job
;; (`_read_ruff_version' in .gitlab/ci-generator/pipelines/backend_pipeline.py),
;; pre-commit itself, and the repo's agent edit hook (.claude/hooks/ruff-on-edit.sh).
;; A bare `ruff' off $PATH floats to whatever version happens to be installed and
;; reformats against a different style than CI enforces — which is precisely how a
;; formatting-only save turns a pipeline red (DEV-4621).
;;
;; Invocation mirrors the hook: `uv tool run ruff@<version> ...' (uvx is not always
;; on PATH; `uv tool run' is).  `--force-exclude' makes ruff honour its own excludes
;; even for an explicitly named file, so generated *_oag SDKs, notebooks and
;; tools/legartis-diag are left alone here exactly as they are everywhere else.
;;
;; Everything below is fail-safe by design: no resolvable version, no uv, a ruff
;; that errors, a syntax error mid-edit — all leave the buffer untouched and let the
;; save proceed.  A save must never be blocked by formatting.

(defvar my-ruff-uv-executable
  (or (and (file-executable-p "~/.local/bin/uv") (expand-file-name "~/.local/bin/uv"))
      (executable-find "uv"))
  "Path to the `uv' binary used to run the repo-pinned ruff, or nil if absent.")

(defvar my-ruff-shim-directory
  (expand-file-name "emacs-ruff" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory holding the generated per-version ruff shims used by flycheck.")

(defvar my-ruff--version-cache (make-hash-table :test 'equal)
  "Maps a .pre-commit-config.yaml path to (MTIME . VERSION).
Re-read whenever the file's mtime changes, so bumping the pin takes effect
without restarting Emacs.")

(defun my-ruff--directory ()
  "Directory the ruff version is resolved from.
The current file's directory, else `default-directory'."
  (or (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun my-ruff--repo-root (dir)
  "Nearest ancestor of DIR holding a .pre-commit-config.yaml, or nil.
Remote (TRAMP) directories are refused — walking them is slow and there is no
local ruff to run against them anyway."
  (when (and dir (not (file-remote-p dir)))
    (let ((root (locate-dominating-file dir ".pre-commit-config.yaml")))
      (and root (expand-file-name root)))))

(defun my-ruff--config-file (dir)
  "Path of the .pre-commit-config.yaml governing DIR, or nil."
  (let ((root (my-ruff--repo-root dir)))
    (and root (expand-file-name ".pre-commit-config.yaml" root))))

(defun my-ruff--parse-version (config)
  "Read the astral-sh/ruff-pre-commit rev out of CONFIG, or nil.
Same shape as the regex CI uses: the repo line, then the first `rev:' after it."
  (with-temp-buffer
    (insert-file-contents config)
    (goto-char (point-min))
    (when (and (re-search-forward "astral-sh/ruff-pre-commit" nil t)
               (re-search-forward "rev:[ \t]*['\"]?v?\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" nil t))
      (match-string 1))))

(defun my-ruff-pinned-version (&optional dir)
  "Ruff version pinned for DIR (default: the current buffer's), or nil.
Interactively, report it — handy for confirming Emacs and CI agree."
  (interactive)
  (let* ((dir (or dir (my-ruff--directory)))
         (config (my-ruff--config-file dir))
         (version
          (when config
            (let ((mtime (file-attribute-modification-time (file-attributes config)))
                  (cached (gethash config my-ruff--version-cache)))
              (if (and cached (equal (car cached) mtime))
                  (cdr cached)
                (let ((parsed (ignore-errors (my-ruff--parse-version config))))
                  (puthash config (cons mtime parsed) my-ruff--version-cache)
                  parsed))))))
    (when (called-interactively-p 'any)
      (message (if version
                   (format "ruff %s (pinned in %s)" version config)
                 "no pinned ruff for this buffer — falling back to `ruff' on PATH")))
    version))

(defun my-ruff--command (dir)
  "Return (PROGRAM ARGS...) running the ruff pinned for DIR, or nil if none.
Falls back to a plain `ruff' on PATH when the version cannot be resolved (a
file outside any pre-commit repo) or when uv is missing."
  (let ((version (my-ruff-pinned-version dir)))
    (cond ((and version my-ruff-uv-executable)
           (list my-ruff-uv-executable "tool" "run" (concat "ruff@" version)))
          ((executable-find "ruff") (list (executable-find "ruff")))
          (t nil))))

(defun my-ruff--run (subcommand extra-args ok-codes)
  "Pipe the current buffer through the pinned ruff SUBCOMMAND.
EXTRA-ARGS go after SUBCOMMAND; OK-CODES are the exit codes whose stdout is
trustworthy output.  Returns `changed', `unchanged', `skipped' or `failed', and
never signals — the buffer is only touched on a clean, non-empty result."
  (let* ((dir (my-ruff--directory))
         (cmd (my-ruff--command dir)))
    (if (not cmd)
        'skipped
      (let ((source (buffer-substring-no-properties (point-min) (point-max)))
            ;; Captured here, not inside `stdout' below, where `buffer-file-name'
            ;; would be nil — and ruff would then apply neither this file's config
            ;; nor its excludes.
            (stdin-name (or buffer-file-name "buffer.py"))
            (stdout (generate-new-buffer " *ruff-output*"))
            (exit-code nil))
        (unwind-protect
            (progn
              (setq exit-code
                    (with-current-buffer stdout
                      (insert source)
                      ;; Run from the repo root, as CI and pre-commit do.
                      ;; For a saved file this is not what finds the config: ruff
                      ;; resolves that by walking up from --stdin-filename, so an
                      ;; absolute name picks up the root [tool.ruff] (line-length 160)
                      ;; from any cwd — verified by piping a 111-char line, which stays
                      ;; on one line at 160 and splits at 88, from services/backend, the
                      ;; repo root and /tmp alike.
                      ;; It *is* load-bearing for a python buffer with no file, where
                      ;; the name below falls back to the relative "buffer.py": a
                      ;; relative --stdin-filename does depend on cwd, and the same line
                      ;; piped from services/backend gets split at 88.  Rooting the
                      ;; process here means a scratch buffer in the repo is formatted
                      ;; like the repo.
                      (let ((default-directory
                             (or (my-ruff--repo-root dir)
                                 (and (file-directory-p dir) dir)
                                 "~/")))
                        ;; stdout replaces the region, stderr is discarded: ruff writes
                        ;; the code to stdout and its diagnostics to stderr, and mixing
                        ;; them would splice error text into the buffer.
                        (condition-case nil
                            (apply #'call-process-region (point-min) (point-max)
                                   (car cmd) t (list t nil) nil
                                   (append (cdr cmd)
                                           (list subcommand)
                                           extra-args
                                           (list "--force-exclude" "--quiet"
                                                 "--stdin-filename" stdin-name
                                                 "-")))
                          (error nil)))))
              (let ((output (with-current-buffer stdout
                              (buffer-substring-no-properties (point-min) (point-max)))))
                (cond
                 ;; Not a code we trust (syntax error, uv download failure, ...) or no
                 ;; output at all: keep what the user typed.
                 ((not (memq exit-code ok-codes)) 'failed)
                 ((string-empty-p output) 'failed)
                 ((string= source output) 'unchanged)
                 (t
                  ;; A non-destructive replacement keeps point, markers and undo
                  ;; minimal instead of erase+insert.  The 0.5s limit degrades to a
                  ;; plain replacement rather than hanging on a big diff.
                  ;; `replace-region-contents' only takes a buffer as its source from
                  ;; Emacs 31 on; before that it wanted a function, hence the split.
                  (condition-case nil
                      (if (and (fboundp 'replace-region-contents)
                               (>= emacs-major-version 31))
                          (replace-region-contents (point-min) (point-max) stdout nil 0.5)
                        (with-no-warnings (replace-buffer-contents stdout 0.5)))
                    (error (let ((point (point)))
                             (erase-buffer)
                             (insert output)
                             (goto-char (min point (point-max))))))
                  'changed))))
          (kill-buffer stdout))))))

(defun ruff-format-buffer ()
  "Format the current buffer with the repo-pinned ruff.
Safe on `before-save-hook': failures are reported, never signalled."
  (interactive)
  (let ((interactive-p (called-interactively-p 'any)))
    (condition-case err
        (pcase (my-ruff--run "format" nil '(0))
          ('changed (when interactive-p (message "Formatted with ruff %s"
                                                 (or (my-ruff-pinned-version) "(PATH)"))))
          ('unchanged (when interactive-p (message "Buffer already formatted")))
          ('skipped (when interactive-p (message "No ruff available — buffer left alone")))
          ('failed (when interactive-p (message "ruff format declined this buffer — left alone"))))
      (error (when interactive-p
               (message "ruff format skipped: %s" (error-message-string err)))))))

(defun ruff-fix-buffer ()
  "Apply ruff's auto-fixes to the current buffer, import sorting (I) included.
Exit code 1 still carries usable output — it just means some violations are
not auto-fixable — so it is accepted here."
  (interactive)
  (let ((interactive-p (called-interactively-p 'any)))
    (condition-case err
        (pcase (my-ruff--run "check" '("--fix") '(0 1))
          ('changed (when interactive-p (message "Fixed with ruff %s"
                                                 (or (my-ruff-pinned-version) "(PATH)"))))
          ('unchanged (when interactive-p (message "Nothing for ruff to fix")))
          ('skipped (when interactive-p (message "No ruff available — buffer left alone")))
          ('failed (when interactive-p (message "ruff check declined this buffer — left alone"))))
      (error (when interactive-p
               (message "ruff check skipped: %s" (error-message-string err)))))))

(defun my-ruff--flycheck-shim ()
  "Path to an executable running the pinned ruff, or nil.
`flycheck-python-ruff-executable' has to be a single program, so the
`uv tool run' prefix (and `--force-exclude', which flycheck does not pass) lives
in a tiny generated script, one per version."
  (let ((version (my-ruff-pinned-version)))
    (when (and version my-ruff-uv-executable)
      (let ((shim (expand-file-name (format "ruff-%s" version) my-ruff-shim-directory))
            (uv (shell-quote-argument my-ruff-uv-executable)))
        (unless (file-executable-p shim)
          (ignore-errors
            (make-directory my-ruff-shim-directory t)
            (with-temp-file shim
              (insert "#!/bin/sh\n"
                      "# Generated by ~/.emacs.conf/general.el — do not edit.\n"
                      "# Runs the ruff version pinned in .pre-commit-config.yaml.\n"
                      "case \"$1\" in\n"
                      "  check|format)\n"
                      "    sub=$1; shift\n"
                      "    exec " uv " tool run \"ruff@" version "\" \"$sub\" --force-exclude \"$@\"\n"
                      "    ;;\n"
                      "esac\n"
                      "exec " uv " tool run \"ruff@" version "\" \"$@\"\n"))
            (set-file-modes shim #o755)))
        (and (file-executable-p shim) shim)))))

(defun my-python-ruff-setup ()
  "Format with the repo-pinned ruff on save.
Also points flycheck at that same version."
  (add-hook 'before-save-hook #'ruff-format-buffer nil t)
  (let ((shim (ignore-errors (my-ruff--flycheck-shim))))
    ;; Set via `make-local-variable' rather than `setq-local' so this works whether
    ;; or not flycheck has loaded yet (it is deferred).
    (when shim
      (set (make-local-variable 'flycheck-python-ruff-executable) shim))))

(add-hook 'python-mode-hook #'my-python-ruff-setup)
(add-hook 'python-ts-mode-hook #'my-python-ruff-setup)

;; Fix for Emacs 31 development version compatibility with minor modes
;; These variables are expected by minor modes but not defined in Emacs 31 dev
(defvar company-mode--suppress-set-explicitly nil
  "Compatibility variable for company-mode with Emacs 31+")
(defvar yas-minor-mode--suppress-set-explicitly nil
  "Compatibility variable for yasnippet with Emacs 31+")
(defvar flycheck-mode--suppress-set-explicitly nil
  "Compatibility variable for flycheck with Emacs 31+")
(defvar flycheck-mode--set-explicitly nil
  "Compatibility variable for flycheck with Emacs 31+")
(defvar auto-highlight-symbol-mode--suppress-set-explicitly nil
  "Compatibility variable for auto-highlight-symbol with Emacs 31+")
(defvar auto-highlight-symbol-mode--set-explicitly nil
  "Compatibility variable for auto-highlight-symbol with Emacs 31+")
(defvar git-gutter-mode--set-explicitly nil
  "Compatibility variable for git-gutter with Emacs 31+")

(use-package company
  :ensure t
  :defer 1
  :config
  (setq company-backends '((company-capf company-files)))
  (global-company-mode 1)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.15)  ;; fast but not CPU-intensive
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
  :defer 1
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

;; Elpy - commented out in favor of eglot
;; (use-package elpy
;;   :ensure t
;;   :defer 1
;;   :commands (elpy-multiedit-python-symbol-at-point
;;              elpy-nav-forward-block
;;              elpy-nav-backward-block
;;              elpy-nav-move-line-or-region-up
;;              elpy-nav-move-line-or-region-down
;;              elpy-goto-definition)
;;   :config
;;   (elpy-enable)
;;   (add-hook 'python-mode-hook 'hs-minor-mode)
;;   (add-hook 'python-ts-mode-hook 'hs-minor-mode)
;;   (when (load "flycheck" t t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; Eglot - built-in LSP client (faster, simpler than elpy)
;; Using ty instead of pyright — pyright consumed 494k+ inotify watchers
(use-package eglot
  :ensure nil  ;; built-in since Emacs 29
  :defer t
  :commands (eglot-rename eglot-code-actions eglot-format-buffer)
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :init
  ;; Use completing-read (ivy) for xref results instead of popup buffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
  ;; Disable auto-import (often imports from wrong package)
  (setq-default eglot-workspace-configuration
                '(:completions (:autoImport :json-false)))
  :config
  ;; Use ty (fast Python type checker) instead of pyright
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ty" "server")))
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (add-hook 'python-ts-mode-hook 'hs-minor-mode)
  ;; Limit reconnection attempts to avoid crash loops
  (setq eglot-autoreconnect 3)
  ;; Disable document highlight (causes font shift with flycheck underlines)
  (add-to-list 'eglot-ignored-server-capabilities :documentHighlightProvider)
  ;; Disable eglot diagnostics - use flycheck instead
  (add-to-list 'eglot-ignored-server-capabilities :textDocument/publishDiagnostics)
  ;; Disable signature help to prevent duplicate eldoc (hover already shows this)
  (add-to-list 'eglot-ignored-server-capabilities :signatureHelpProvider)
  ;; Ty asks eglot to watch ~/ with glob **, which triggers `find' over 500k+
  ;; dirs in $HOME, hits `eglot-max-file-watches' (default 10000), and ends up
  ;; with zero watches registered. Refuse watchers outside the project root.
  (setq eglot-watch-files-outside-project-root nil))

;; Show eldoc in tooltip popup near cursor (hover docs)
(use-package eldoc-box
  :ensure t
  :defer t
  :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode)))

;; Move lines up/down (replacement for elpy-nav-move-line-or-region)
(use-package move-text
  :ensure t
  :defer t
  :commands (move-text-up move-text-down))

(use-package flycheck
  :ensure t
  :defer 1
  :config
  (global-flycheck-mode nil)
  (add-to-list 'ivy-ignore-buffers "\\*Flycheck")

  ;; Ruff is the only python linter.  The executable is *not* set globally here:
  ;; `my-python-ruff-setup' sets it buffer-locally to the version pinned by that
  ;; buffer's repo, so flycheck flags exactly the rules CI does.
  (setq-default flycheck-disabled-checkers '(python-flake8 python-pylint python-pycompile python-mypy))

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

(use-package jedi
  :ensure t
  :defer 1
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

(use-package git-gutter-fringe
  :ensure t
  :defer 1
  :config
  (global-git-gutter-mode 1)
  ;; Subtle indicators
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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
  :ensure t
  :defer t)
;;(use-package ob-ipython
;;  :ensure t)

(setenv "EDITOR" "emacsclient")

(use-package pip-requirements
  :hook ((pip-requirements-mode . company-mode))
  :ensure t)

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project projectile-switch-to-buffer
             counsel-projectile counsel-projectile-find-file counsel-projectile-switch-project
             counsel-projectile-ag counsel-projectile-switch-to-buffer)
  :init
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-globally-ignored-file-suffixes '("j2" "llamafile" "pdf" "docx"))
  ;; Periodically refresh cache when idle (5 min)
  (run-with-idle-timer 300 t (lambda () (projectile-invalidate-cache nil)))
  )

(use-package python-pytest
  :ensure t
  :defer t)

(use-package pyvenv
  :ensure t
  :defer 1
  :config
  ;; Restart eglot when activating a venv so it picks up the new environment
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (when (and (derived-mode-p 'python-mode 'python-ts-mode)
                         (eglot-current-server))
                (eglot-reconnect (eglot-current-server))))))

(use-package virtualenvwrapper
  :ensure t
  :defer 1)

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
  :defer 1
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
;;   :ensure t
;;   :defer t
;;   :hook (python-ts-mode . eglot-ensure))

;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("ruff" "server")))
;;   (add-hook 'after-save-hook 'eglot-format))

;; (require 'flymake-ruff)
;; (add-hook 'python-mode-hook #'flymake-ruff-load)

;; (use-package kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview)
;;   :config
;;   (setq kubernetes-poll-frequency 3600
;;         kubernetes-redraw-frequency 3600))
(use-package kubectx-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package poly-ansible
  :ensure t
  :defer 1)
(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode))
  )
(use-package tide
  :ensure t
  :defer t
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
  :ensure t
  :defer 1)
(use-package typescript-mode
  :mode (("\\.ts$" . typescript-mode))
  :ensure t
  :config

  (add-hook 'typescript-mode-hook 
            (lambda () (setq-local create-lockfiles nil)))
  (add-hook 'web-mode-hook 
            (lambda () (setq-local create-lockfiles nil)))
  (setq create-lockfiles nil)
  )

(use-package json-mode
  :ensure t
  :defer t  ;; load when opening .json files
  :config
  (setq json-reformat:indent-width 2)
  (setq js-indent-level 2))
(use-package csv-mode
  :defer t
  :mode "\\.csv\\'")
(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")
(with-eval-after-load 'flycheck
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

(use-package package-lint
  :ensure t
  :defer t
  :commands package-lint-current-buffer)

(use-package package-lint-flymake
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . package-lint-flymake-setup))
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

;;;;;;;;;;;;;;;;;
;;; SHORTCUTS ;;;
;;;;;;;;;;;;;;;;;

(bind-keys*
 ("M-o" . ace-window)
 )

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

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
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

  (defhydra hydra-flycheck (:color pink :hint nil)
    "
^Navigation^      ^Actions^          ^Display^
^^^^^^^^-------------------------------------------------
_i_: next         _c_: clear         _l_: list errors
_o_: previous     _v_: verify setup  _e_: explain error
_f_: first        _x_: disable       _h_: help
^ ^               _s_: select checker
"
    ("i" flycheck-next-error)
    ("o" flycheck-previous-error)
    ("f" flycheck-first-error)
    ("l" flycheck-list-errors :color blue)
    ("e" flycheck-explain-error-at-point)
    ("h" flycheck-display-error-at-point)
    ("c" flycheck-clear)
    ("v" flycheck-verify-setup :color blue)
    ("x" flycheck-disable-checker :color blue)
    ("s" flycheck-select-checker :color blue)
    ("q" nil "quit" :color blue))

  (defhydra hydra-toggle (:color blue :hint nil)
    "
^Display^         ^Editing^          ^Modes^
^^^^^^^^-------------------------------------------------
_l_: line nums    _w_: whitespace    _f_: flycheck
_t_: truncate     _h_: hl-line       _a_: auto-fill
_v_: visual-line  _c_: column        _r_: read-only
_g_: git-gutter   _i_: indent-guide
"
    ("l" display-line-numbers-mode)
    ("t" toggle-truncate-lines)
    ("v" visual-line-mode)
    ("g" git-gutter-mode)
    ("w" whitespace-mode)
    ("h" hl-line-mode)
    ("c" column-number-mode)
    ("i" highlight-indent-guides-mode)
    ("f" flycheck-mode)
    ("a" auto-fill-mode)
    ("r" read-only-mode)
    ("q" nil "quit"))

  (defhydra hydra-eglot (:color blue :hint nil)
    "
^Actions^         ^Navigate^         ^Info^
^^^^^^^^-------------------------------------------------
_a_: code action  _;_: definition    _h_: hover doc
_r_: rename       _:_: references    _d_: declaration
_f_: format       _i_: implementation
_o_: organize imports
"
    ("a" eglot-code-actions)
    ("r" eglot-rename)
    ("f" eglot-format-buffer)
    ("o" eglot-code-action-organize-imports)
    (";" xref-find-definitions)
    (":" xref-find-references)
    ("i" eglot-find-implementation)
    ("d" eglot-find-declaration)
    ("h" eldoc-box-help-at-point)
    ("q" nil "quit"))
  )

(use-package ivy-hydra
  :ensure t
  :defer 1)


(load "~/.emacs.conf/claudegel.el" t)


(use-package kkp
  :ensure t
  :config
  (global-kkp-mode +1))

;; tmux 3.6 does not proxy the kitty keyboard protocol, so kkp cannot
;; activate inside tmux. Fall back to a sit-for disambiguation: a bare
;; ESC byte with no follow-up within 30ms is translated to <escape>;
;; ESC followed quickly by another byte (real escape sequences, M-x,
;; etc.) falls through unchanged.
(define-key input-decode-map [?\e]
  `(menu-item "" [escape]
              :filter ,(lambda (real-binding)
                         (when (sit-for 0.03 t) real-binding))))


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

  ;; Visual mode indicator - change modeline color
  ;; Emacs 29+ uses mode-line-active for active window
  (defvar ryo-modal-mode-line-bg-orig nil "Original mode-line background.")
  (defvar ryo-modal-mode-line-active-bg-orig nil "Original mode-line-active background.")

  ;; These colours have to follow the theme. The gruvbox hexes used to be
  ;; hardcoded in both branches, which meant the modeline stayed gruvbox under
  ;; every other theme -- most visibly under win95, where the navy caption bar
  ;; is most of what makes it look like Windows.
  (defun ryo-modal--win-theme-p ()
    (or (memq 'win95 custom-enabled-themes)
        (memq 'win311 custom-enabled-themes)))

  (defun ryo-modal--modal-bg ()
    "Modeline background while modal mode is ON."
    (if (ryo-modal--win-theme-p) "#008080" "#3d4220"))   ; teal reads as "armed"

  (defun ryo-modal--normal-bg ()
    "Modeline background while modal mode is OFF.
Prefers the value saved before we first overrode it, so an unknown theme still
gets its own colour back rather than gruvbox's."
    (cond ((ryo-modal--win-theme-p) "#000080")           ; the Win95 caption
          (ryo-modal-mode-line-bg-orig)
          (t "#3c3836")))

  (defun ryo-modal-update-modeline ()
    "Update modeline and cursor color based on ryo-modal state."
    (if ryo-modal-mode
        (progn
          ;; Save original colors once
          (unless ryo-modal-mode-line-bg-orig
            (setq ryo-modal-mode-line-bg-orig (face-background 'mode-line nil t)))
          (unless ryo-modal-mode-line-active-bg-orig
            (setq ryo-modal-mode-line-active-bg-orig (face-background 'mode-line-active nil t)))
          (let ((bg (ryo-modal--modal-bg)))
            (set-face-background 'mode-line bg)
            (set-face-background 'mode-line-active bg))
          (set-cursor-color (if (ryo-modal--win-theme-p) "#000080" "#859900")))
      (let ((bg (ryo-modal--normal-bg)))
        (set-face-background 'mode-line bg)
        (set-face-background 'mode-line-active bg))
      (set-cursor-color (if (ryo-modal--win-theme-p) "#000000" "#a89984"))))

  (add-hook 'ryo-modal-mode-hook #'ryo-modal-update-modeline)

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
   ("m" ryo-modal-repeat)
   ("." next-buffer)
   ("," previous-buffer)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("/" move-end-of-line)
   ("?" dumb-jump-back)

   ("`" pop-global-mark)  ;; jump back through mark ring
   ("@" kmacro-call-macro)  ;; call last keyboard macro (vim-like)
   ("\\" hydra-toggle/body)  ;; toggle various modes
   ("~" tild-up-or-replace)
   ("!" hydra-flycheck/body)
   ("#" highlight-symbol-query-replace)
   ("$" query-replace-thing-at-point-or-selection)
   ("%" query-replace)
   ("(" insert-parentheses)
   ("-" delete-horizontal-and-vertical-space)
   ("_" delete-horizontal-and-vertical-space-but-leave-one-space)
   ("=" er/expand-region)
   ("+" mark-paragraph)  ;; use me ;; or not, really, what's the point...
   ("SPC" set-mark-command)
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
         ("r" ruff-format-buffer)
         ;; ("t")

         ("Q" my-substitute-word-or-region)
         ("W" my-backward-substitute-word-or-region)

         ("a" comment-line)
         ;; ("s" helm-projectile-rg)
         ("s" counsel-projectile-ag)
         ("S" copy-buffer-useful-path)
         ("d" copy-full-path-to-kill-ring)
         ("D" copy-folder-path-to-kill-ring)
         ("G" gptel-menu)
         ("h" query-replace-regexp)  ;; regex find & replace
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

         ;; Claude Code (claudegel) — `ag*` prefix.
         ;; Models, ordered weakest→strongest left→right on home row.
         ;; C-u prefix on any of these enables --effort xhigh ("thinking").
         ("gj" claudegel-send-haiku)              ; fast/cheap
         ("gk" claudegel-send-sonnet)             ; default
         ("gl" claudegel-send-opus)               ; heavy
         ;; Same gradient, capitals = project tier (full tools, in repo root).
         ("gJ" claudegel-send-haiku-project)
         ("gK" claudegel-send-sonnet-project)
         ("gL" claudegel-send-opus-project)
         ;; Specialised prompts on top row.
         ("gu" claudegel-send-short)              ; line-replace short answer
         ("gi" claudegel-send-translate)          ; translate (line-replace)
         ("go" claudegel-send-continue)           ; code autocomplete
         ("gp" claudegel-send-prose)              ; prose / writing
         ;; Session / control.
         ("gg" claudegel-abort)                   ; preserve old muscle memory
         ("g;" claudegel-abort)                   ; alt
         ("gG" claudegel-reset-session)
         ("ga" claudegel-toggle-fold-at-point)    ; fold tool call at point
         ("gA" claudegel-scrub-tools)             ; nuke all tool calls
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
         ("g" counsel-git-grep)  ;; search in git repo
         ("h" consult-line)  ;; search in buffer (fast, async)
         (";" counsel-rg)  ;; search project with ripgrep
         ("n" consult-imenu)  ;; jump to function/class
         ("b" consult-buffer)  ;; buffer switch with preview
         ("j" counsel-projectile)
         ;; ("j" projectile-switch-to-buffer)
         ("k" kill-all-buffers-but-scratch)
         ;; ("l" venv-workon)
         ("l" pyvenv-workon)
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
         ("l" projectile-ibuffer)
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
         ("t" eglot-rename)

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

   ;; M-o is ace-window globally, use M-up/M-down for moving lines
   ("M-i" move-text-down)

   ("at" python-add-return)
   ("se" python-add-breakpoint)
   ("st" ask-aider)

   ("dt" autoflake)
   ("dz" get-test-string)
   ("dx" get-class-string)

   ("f;" xref-find-definitions)
   ("f:" xref-find-references-at-point)
   ("fc" hydra-eglot/body)
   )

  (ryo-modal-major-mode-keys
   'emacs-lisp-mode
   ("I" forward-sexp)
   ("O" backward-sexp)
   ("fe" eval-last-sexp)
   ("fE" eval-current-buffer-and-message)

   )

  (ryo-modal-major-mode-keys
   'haskell-mode
   ("f;" haskell-mode-jump-to-def)
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
