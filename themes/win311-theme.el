;;; win311-theme.el --- Windows 3.11, as an Emacs theme -*- lexical-binding: t -*-

;; The older sibling of win95-theme.el. Same VGA-16 palette — 3.11 and 95 drew
;; from the identical 16 colours — so the difference between the two themes is
;; not hue, it is *restraint*.
;;
;; Windows 3.x looked more monochrome than Win95 because its applications
;; largely were. Program Manager, Write and Notepad are black text on white or
;; grey with navy chrome; the teals and greens Win95 sprinkled through its
;; shell are barely present. So this theme deliberately spends fewer colours:
;;
;;   black    text, variables, the bulk of every buffer
;;   navy     keywords, functions, the caption bar, the selection
;;   maroon   literals and constants — the one warm accent
;;   grey     comments, chrome, anything receding
;;
;; No green strings, no teal types. A buffer under win311 is close to black and
;; white with two accents, which is what a 3.1 screen actually looked like, and
;; it is what makes this read as *older* than win95 rather than merely different.
;;
;; The other tell is the fringe. Win95 mode leaves it plain grey; here it gets
;; the shadow edge, because 3.x drew a sunken client area inside a raised
;; frame and that inset line is everywhere in Program Manager.

;;; Code:

(deftheme win311 "Windows 3.11: near-monochrome, navy caption, two accents.")

(let ((face      "#c0c0c0")
      (hilite    "#ffffff")
      (light     "#dfdfdf")
      (shadow    "#808080")
      (navy      "#000080")
      (navy-lt   "#1084d0")
      (teal      "#008080")
      (maroon    "#800000")
      (olive     "#808000")
      (red       "#ff0000")
      (green     "#008000")
      (white     "#ffffff")
      (black     "#000000"))

  (custom-theme-set-faces
   'win311

   ;; ── the document ────────────────────────────────────────────────────
   `(default        ((t (:background ,white :foreground ,black))))
   `(cursor         ((t (:background ,black))))
   `(region         ((t (:background ,navy :foreground ,white))))
   `(highlight      ((t (:background ,face :foreground ,black))))
   `(hl-line        ((t (:background ,light))))
   ;; Sunken client area: shadow above the canvas, hilite below it.
   `(fringe         ((t (:background ,face :foreground ,shadow))))
   `(vertical-border ((t (:foreground ,shadow))))
   `(window-divider ((t (:foreground ,shadow))))
   `(window-divider-first-pixel ((t (:foreground ,shadow))))
   `(window-divider-last-pixel  ((t (:foreground ,hilite))))
   `(line-number    ((t (:background ,face :foreground ,shadow))))
   `(line-number-current-line ((t (:background ,face :foreground ,black :weight bold))))
   `(shadow         ((t (:foreground ,shadow))))
   `(link           ((t (:foreground ,navy :underline t))))
   `(link-visited   ((t (:foreground ,maroon :underline t))))
   `(escape-glyph   ((t (:foreground ,maroon))))
   `(trailing-whitespace ((t (:background ,maroon))))

   ;; ── the caption bar ─────────────────────────────────────────────────
   ;; 3.x captions carry no close button, so the mode line gets no highlight
   ;; box on the right either — it is a plain bar, the way PROGMAN drew it.
   `(mode-line          ((t (:background ,navy :foreground ,white))))
   `(mode-line-inactive ((t (:background ,face :foreground ,shadow))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-highlight ((t (:background ,navy-lt :foreground ,white))))
   `(header-line        ((t (:background ,face :foreground ,black))))

   ;; ── chrome ──────────────────────────────────────────────────────────
   `(minibuffer-prompt ((t (:foreground ,navy :weight bold))))
   `(tooltip           ((t (:background "#ffffe1" :foreground ,black))))
   `(match             ((t (:background ,olive :foreground ,black))))
   `(isearch           ((t (:background ,navy :foreground ,white :weight bold))))
   `(lazy-highlight    ((t (:background ,face :foreground ,black))))
   `(show-paren-match  ((t (:background ,face :foreground ,navy :weight bold))))
   `(show-paren-mismatch ((t (:background ,maroon :foreground ,white :weight bold))))
   `(error             ((t (:foreground ,red :weight bold))))
   `(warning           ((t (:foreground ,maroon :weight bold))))
   `(success           ((t (:foreground ,navy :weight bold))))

   ;; ── syntax: black, navy, maroon, grey. That is the whole budget. ─────
   `(font-lock-keyword-face       ((t (:foreground ,navy :weight bold))))
   `(font-lock-builtin-face       ((t (:foreground ,navy))))
   `(font-lock-function-name-face ((t (:foreground ,navy :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,black))))
   `(font-lock-type-face          ((t (:foreground ,black :weight bold))))
   `(font-lock-constant-face      ((t (:foreground ,maroon))))
   `(font-lock-string-face        ((t (:foreground ,maroon))))
   `(font-lock-doc-face           ((t (:foreground ,shadow))))
   `(font-lock-comment-face       ((t (:foreground ,shadow))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,shadow))))
   `(font-lock-negation-char-face ((t (:foreground ,maroon :weight bold))))
   `(font-lock-preprocessor-face  ((t (:foreground ,navy))))
   `(font-lock-warning-face       ((t (:foreground ,red :weight bold))))

   ;; ── the rest ────────────────────────────────────────────────────────
   `(completions-common-part ((t (:foreground ,navy :weight bold))))
   `(diff-added    ((t (:foreground ,green))))
   `(diff-removed  ((t (:foreground ,maroon))))
   `(diff-header   ((t (:background ,face :foreground ,black))))
   `(diff-file-header ((t (:background ,face :foreground ,black :weight bold))))
   `(dired-directory ((t (:foreground ,navy :weight bold))))
   `(compilation-error   ((t (:foreground ,red :weight bold))))
   `(compilation-warning ((t (:foreground ,maroon))))
   `(compilation-info    ((t (:foreground ,navy))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'win311)
;;; win311-theme.el ends here
