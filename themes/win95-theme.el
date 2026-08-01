;;; win95-theme.el --- The Windows 95 desktop, as an Emacs theme -*- lexical-binding: t -*-

;; Companion to win95-pkg (~/dotfiles/win95-pkg). Same five colours the i3
;; palette and the taskbar are built from, so Emacs stops being the one window
;; on a Win95 desktop that looks like it came from 2026:
;;
;;   #c0c0c0 face   #ffffff hilite   #808080 shadow   #000080 navy   #008080 teal
;;
;; The canvas is white, not grey: grey is *chrome* in Windows: dialogs, buttons,
;; the taskbar. A document — Notepad, Write, an edit control — is white with
;; black text, and a buffer is a document. Grey shows up where Emacs draws
;; furniture (fringes, borders, the mode line's inactive state).
;;
;; The mode line is the title bar: navy with white text when the window is
;; live, shadow-grey with silver text when it isn't, exactly as Win95 paints an
;; active and an inactive caption.
;;
;; Syntax colours come from the 16-colour VGA palette and nothing else. The
;; restraint is the point — every hue here is one an actual 1995 display could
;; show, which is most of why it reads as period rather than as a costume.

;;; Code:

(deftheme win95 "Windows 95: white canvas, navy caption, VGA-16 syntax.")

(let ((face      "#c0c0c0")   ; button face — chrome, never the canvas
      (hilite    "#ffffff")   ; raised edge, top/left
      (light     "#dfdfdf")
      (shadow    "#808080")   ; raised edge, bottom/right; inactive caption
      (dkshadow  "#000000")
      (navy      "#000080")   ; active caption, and the selection
      (navy-lt   "#1084d0")
      (teal      "#008080")   ; the desktop
      (maroon    "#800000")
      (green     "#008000")
      (olive     "#808000")
      (purple    "#800080")
      (red       "#ff0000")
      (white     "#ffffff")
      (black     "#000000"))

  (custom-theme-set-faces
   'win95

   ;; ── the document ────────────────────────────────────────────────────
   `(default        ((t (:background ,white :foreground ,black))))
   `(cursor         ((t (:background ,black))))
   ;; White-on-navy is *the* Windows selection. Nothing else says it as fast.
   `(region         ((t (:background ,navy :foreground ,white))))
   `(highlight      ((t (:background ,navy-lt :foreground ,white))))
   `(hl-line        ((t (:background ,light))))
   `(fringe         ((t (:background ,face :foreground ,shadow))))
   `(vertical-border ((t (:foreground ,shadow))))
   `(window-divider ((t (:foreground ,shadow))))
   `(window-divider-first-pixel ((t (:foreground ,hilite))))
   `(window-divider-last-pixel  ((t (:foreground ,shadow))))
   `(line-number    ((t (:background ,face :foreground ,shadow))))
   `(line-number-current-line ((t (:background ,face :foreground ,black :weight bold))))
   `(shadow         ((t (:foreground ,shadow))))
   `(link           ((t (:foreground ,navy :underline t))))
   `(link-visited   ((t (:foreground ,purple :underline t))))
   `(escape-glyph   ((t (:foreground ,teal))))
   `(trailing-whitespace ((t (:background ,maroon))))

   ;; ── the caption bar ─────────────────────────────────────────────────
   `(mode-line          ((t (:background ,navy :foreground ,white
                             :box (:line-width 1 :color ,face)))))
   `(mode-line-inactive ((t (:background ,shadow :foreground ,light
                             :box (:line-width 1 :color ,face)))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-highlight ((t (:background ,navy-lt :foreground ,white))))
   `(header-line        ((t (:background ,face :foreground ,black
                             :box (:line-width 1 :color ,hilite)))))

   ;; ── chrome ──────────────────────────────────────────────────────────
   `(minibuffer-prompt ((t (:foreground ,navy :weight bold))))
   `(tooltip           ((t (:background "#ffffe1" :foreground ,black))))  ; the yellow tooltip
   `(match             ((t (:background ,olive :foreground ,black))))
   `(isearch           ((t (:background ,navy :foreground ,white :weight bold))))
   `(lazy-highlight    ((t (:background ,teal :foreground ,white))))
   `(show-paren-match  ((t (:background ,teal :foreground ,white :weight bold))))
   `(show-paren-mismatch ((t (:background ,red :foreground ,white :weight bold))))
   `(error             ((t (:foreground ,red :weight bold))))
   `(warning           ((t (:foreground ,maroon :weight bold))))
   `(success           ((t (:foreground ,green :weight bold))))

   ;; ── syntax, VGA-16 only ─────────────────────────────────────────────
   `(font-lock-keyword-face       ((t (:foreground ,navy :weight bold))))
   `(font-lock-builtin-face       ((t (:foreground ,navy))))
   `(font-lock-function-name-face ((t (:foreground ,navy :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,black))))
   `(font-lock-type-face          ((t (:foreground ,teal :weight bold))))
   `(font-lock-constant-face      ((t (:foreground ,maroon))))
   `(font-lock-string-face        ((t (:foreground ,green))))
   `(font-lock-doc-face           ((t (:foreground ,green))))
   `(font-lock-comment-face       ((t (:foreground ,shadow))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,shadow))))
   `(font-lock-negation-char-face ((t (:foreground ,red))))
   `(font-lock-preprocessor-face  ((t (:foreground ,purple))))
   `(font-lock-warning-face       ((t (:foreground ,red :weight bold))))

   ;; ── things that would otherwise leak a modern palette ────────────────
   `(completions-common-part ((t (:foreground ,navy :weight bold))))
   `(diff-added    ((t (:foreground ,green))))
   `(diff-removed  ((t (:foreground ,maroon))))
   `(diff-header   ((t (:background ,face :foreground ,black))))
   `(diff-file-header ((t (:background ,face :foreground ,black :weight bold))))
   `(dired-directory ((t (:foreground ,navy :weight bold))))
   `(compilation-error   ((t (:foreground ,red :weight bold))))
   `(compilation-warning ((t (:foreground ,maroon))))
   `(compilation-info    ((t (:foreground ,green))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'win95)
;;; win95-theme.el ends here
