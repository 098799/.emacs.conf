;;; claudegel.el --- Claude Code as a gptel-style backend  -*- lexical-binding: t -*-

;; Talks to `claude -p --input-format stream-json --output-format stream-json
;; --verbose' as a subprocess. Uses the CC subscription via OAuth: the
;; ANTHROPIC_API_KEY env var is cleared for the child so the keychain
;; OAuth token is used.

(require 'json)
(require 'cl-lib)

(defgroup claudegel nil
  "Claude Code subprocess backend." :group 'tools)

(defcustom claudegel-program "claude"
  "Path to the claude CLI." :type 'string)

(defcustom claudegel-default-model "sonnet"
  "Default model alias passed to --model." :type 'string)

(defcustom claudegel-default-permission-mode "bypassPermissions"
  "Default permission mode. We rarely run real tools from Emacs."
  :type 'string)

(defcustom claudegel-read-only-tools '("Read" "Grep" "Glob")
  "Tool names allowed when :tools is 'read-only." :type '(repeat string))

(defcustom claudegel-auto-revert-on-edit t
  "After a turn that edited files, refresh the matching live buffers." :type 'boolean)

(defcustom claudegel-response-header "\n\n### "
  "Inserted before the assistant's response." :type 'string)

(defcustom claudegel-prompt-header "\n\n"
  "Inserted after the response; just spacing so the next user turn starts fresh.
Leave empty or newlines — don't repeat the response marker here." :type 'string)

(defcustom claudegel-show-thinking nil
  "If non-nil, render thinking blocks (folded). Otherwise drop them." :type 'boolean)

(defcustom claudegel-include-partial-messages t
  "Pass --include-partial-messages so text streams token-by-token."
  :type 'boolean)

(defface claudegel-tool-face
  '((t :inherit shadow :slant italic))
  "Face for tool-call summary lines.")

(defface claudegel-tool-detail-face
  '((t :inherit shadow :height 0.9))
  "Face for tool-call expanded detail.")

;; --- prompts ---------------------------------------------------------------

(defconst claudegel-prompt-long-text
  "You're talking to Tomek in an Emacs buffer. Any text he sends is his message \
to you; respond directly.

Tone: informal, push back when you disagree, criticize his reasoning when it's \
off. No sycophancy, no \"great question\" openers, no \"Ah…\" starts, no \
disclaimers about being an AI. Dry or subtle humor welcome but not required. \
Obscure words and casual abbreviations (rn, afaict, idk) are fine. Mixed \
casing and skipped punctuation are fine when they fit the register. If a \
request is silly, say so (\"lol no\", \"be real\") instead of humoring it. \
Write roughly 2 SD smarter than you're defaulting to.")

(defconst claudegel-prompt-short-text
  "Output only the answer. No preamble, no trailing commentary, no markdown \
fences unless the answer itself is code that requires them. The reply will be \
pasted directly as a command, snippet, or plain text and must work as-is.")

(defconst claudegel-prompt-continue-text
  "You are a code-completion engine. The user's buffer ends mid-file; your \
output is appended verbatim. Continue the code. Do not repeat anything that \
already appears in the input, do not add commentary, do not wrap in markdown \
fences. Match the existing indentation, identifier style, and language.")

(defconst claudegel-prompt-translation-text
  "Translate the user's input into English and Polish. Output exactly two \
lines, no preamble, no commentary, no quoting the original:

English: <English translation>
Polish: <Polish translation>

Preserve proper nouns. If the input is already English, still produce both \
lines (Polish translation of the English). If ambiguous, pick the most \
natural reading.")

(defconst claudegel-prompt-prose-text
  "You are a serious literary prose stylist. Write in measured, precise \
English: vivid settings, layered characters with real interiority, \
show-don't-tell, controlled pacing. No humor unless requested. No AI-assistant \
voice — write as the narrator. Use sensory detail purposefully, not \
decoratively. Avoid cliché and stock metaphors. Dialogue should reveal \
character and advance the scene, not deliver exposition.")

(defun claudegel-prompt-long ()        claudegel-prompt-long-text)
(defun claudegel-prompt-short ()       claudegel-prompt-short-text)
(defun claudegel-prompt-continue ()    claudegel-prompt-continue-text)
(defun claudegel-prompt-translation () claudegel-prompt-translation-text)
(defun claudegel-prompt-prose ()       claudegel-prompt-prose-text)

;; --- buffer-local state ----------------------------------------------------

(defvar-local claudegel--process nil)
(defvar-local claudegel--session-id nil
  "CC session id for this buffer; resumed on subsequent sends.")
(defvar-local claudegel--insertion-marker nil)
(defvar-local claudegel--line-buffer ""
  "Accumulator for partial JSON lines.")
(defvar-local claudegel--tool-overlays nil
  "Alist of (tool-use-id . overlay) for the in-progress turn.")
(defvar-local claudegel--edited-files nil
  "Files reported as edited during the in-progress turn (for auto-revert).")
(defvar-local claudegel--phase nil
  "Short label describing what Claude is currently doing in this buffer.")
(defvar-local claudegel--sent-end nil
  "Marker at end-of-buffer after the last completed turn.
The next send transmits only the text typed since this marker,
since --resume gives CC the prior history server-side.")

;; --- mode-line spinner -----------------------------------------------------

(defconst claudegel--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
(defvar claudegel--spinner-index 0)
(defvar claudegel--spinner-timer nil)
(defvar claudegel--mode-line "" "String appended to global-mode-string.")
(put 'claudegel--mode-line 'risky-local-variable t)

(defun claudegel--active-buffers ()
  (cl-remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (and claudegel--process (process-live-p claudegel--process))))
   (buffer-list)))

(defun claudegel--tick ()
  (let ((active (claudegel--active-buffers)))
    (if (null active)
        (claudegel--stop-spinner)
      (setq claudegel--spinner-index
            (mod (1+ claudegel--spinner-index)
                 (length claudegel--spinner-frames)))
      (setq claudegel--mode-line
            (format " %s claudegel%s"
                    (aref claudegel--spinner-frames claudegel--spinner-index)
                    (let ((phases (delete-dups
                                   (mapcar
                                    (lambda (b) (buffer-local-value 'claudegel--phase b))
                                    active))))
                      (if (and phases (car phases))
                          (format " [%s]"
                                  (mapconcat #'identity
                                             (cl-remove-if-not #'stringp phases)
                                             "/"))
                        ""))))
      (force-mode-line-update t))))

(defun claudegel--start-spinner ()
  (unless (memq 'claudegel--mode-line global-mode-string)
    (setq global-mode-string
          (append (or global-mode-string '("")) '(claudegel--mode-line))))
  (unless claudegel--spinner-timer
    (setq claudegel--spinner-timer
          (run-at-time 0 0.15 #'claudegel--tick))))

(defun claudegel--stop-spinner ()
  (when claudegel--spinner-timer
    (cancel-timer claudegel--spinner-timer)
    (setq claudegel--spinner-timer nil))
  (setq claudegel--mode-line "")
  (force-mode-line-update t))

;; --- main entry: dispatcher ------------------------------------------------

(defun claudegel-detect-cwd ()
  "Best-effort project root for the current buffer."
  (or (and (fboundp 'projectile-project-root) (ignore-errors (projectile-project-root)))
      (when-let* ((proj (and (fboundp 'project-current) (project-current nil))))
        (cond
         ((fboundp 'project-root) (project-root proj))
         ((fboundp 'project-roots) (car (project-roots proj)))))
      (when buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(cl-defun claudegel-send (&key
                          (model claudegel-default-model)
                          (effort nil)
                          (system-prompt-fn #'claudegel-prompt-long)
                          (mode 'append)
                          (tools 'none)
                          (cwd nil)
                          (label nil))
  "Send the active region (or whole buffer) to Claude Code.

Keyword args:
  :model    model alias (haiku/sonnet/opus or full id)
  :effort   one of low/medium/high/xhigh/max, or nil
  :system-prompt-fn  thunk returning the system prompt string
  :mode     'append   — insert response after the prompt (default)
            'replace  — delete the region/line first; stream into the gap
  :tools    'none      — disable all tools (pure LLM, like gptel) — default
            'read-only — Read/Grep/Glob only
            'all       — full toolset (let Claude edit your project)
  :cwd      directory to run claude in. Defaults to project root.
  :label    short string for the echo-area message"
  (when (and claudegel--process (process-live-p claudegel--process))
    (user-error "Claude is already responding here; M-x claudegel-abort first"))
  (let* ((replace (eq mode 'replace))
         (resuming (and claudegel--session-id claudegel--sent-end
                        (not (use-region-p)) (not replace)))
         (start (cond ((use-region-p) (region-beginning))
                      (replace        (line-beginning-position))
                      (resuming       (marker-position claudegel--sent-end))
                      (t              (point-min))))
         (end   (cond ((use-region-p) (region-end))
                      (replace        (line-end-position))
                      (t              (point-max))))
         (prompt-text (buffer-substring-no-properties start end))
         (insert-pos end))
    (when (use-region-p) (deactivate-mark))
    (cond
     (replace
      (delete-region start end)
      (goto-char start))
     (t
      (goto-char insert-pos)
      (insert claudegel-response-header)))
    (setq claudegel--insertion-marker (copy-marker (point) t)
          claudegel--line-buffer ""
          claudegel--tool-overlays nil
          claudegel--edited-files nil
          claudegel--phase "waiting")
    (claudegel--start-spinner)
    (let ((default-directory (or cwd (claudegel-detect-cwd))))
      (claudegel--start-process prompt-text model effort system-prompt-fn
                                tools (eq mode 'append))
      (message "claudegel: %s [%s tools, cwd %s]%s%s"
               (or label model)
               tools
               (abbreviate-file-name (directory-file-name default-directory))
               (if effort (format " (effort %s)" effort) "")
               (if claudegel--session-id
                   (format " resume %s" (substring claudegel--session-id 0 8))
                 " new")))))

;;;###autoload
(defun claudegel-abort ()
  "Abort the in-flight Claude response in this buffer."
  (interactive)
  (when (and claudegel--process (process-live-p claudegel--process))
    (interrupt-process claudegel--process)
    (message "claudegel: aborted")))

;;;###autoload
(defun claudegel-reset-session ()
  "Forget the session id so next send starts fresh."
  (interactive)
  (setq claudegel--session-id nil
        claudegel--sent-end nil)
  (message "claudegel: session reset"))

;; --- thin command wrappers (these are what you bind) ----------------------

;; "Ask" tier — no tools, pure LLM answers (the gptel-equivalent default).
;; Each command honours the prefix arg: C-u enables --effort xhigh ("thinking").

(defun claudegel--effort-from-prefix ()
  (when current-prefix-arg "xhigh"))

;;;###autoload
(defun claudegel-send-haiku () (interactive)
       (claudegel-send :model "haiku" :tools 'none
                       :effort (claudegel--effort-from-prefix) :label "haiku"))

;;;###autoload
(defun claudegel-send-sonnet () (interactive)
       (claudegel-send :model "sonnet" :tools 'none
                       :effort (claudegel--effort-from-prefix) :label "sonnet"))

;;;###autoload
(defun claudegel-send-opus () (interactive)
       (claudegel-send :model "opus" :tools 'none
                       :effort (claudegel--effort-from-prefix) :label "opus"))

;; "Project" tier — full tool access, runs in detected project root.
;;;###autoload
(defun claudegel-send-haiku-project () (interactive)
       (claudegel-send :model "haiku" :tools 'all
                       :effort (claudegel--effort-from-prefix) :label "haiku+proj"))

;;;###autoload
(defun claudegel-send-sonnet-project () (interactive)
       (claudegel-send :model "sonnet" :tools 'all
                       :effort (claudegel--effort-from-prefix) :label "sonnet+proj"))

;;;###autoload
(defun claudegel-send-opus-project () (interactive)
       (claudegel-send :model "opus" :tools 'all
                       :effort (claudegel--effort-from-prefix) :label "opus+proj"))

;; Misc workflows
;;;###autoload
(defun claudegel-send-short () (interactive)
       (claudegel-send :model "sonnet" :tools 'none
                       :system-prompt-fn #'claudegel-prompt-short
                       :mode 'replace :label "short"))

;;;###autoload
(defun claudegel-send-continue () (interactive)
       (claudegel-send :model "sonnet" :tools 'none
                       :system-prompt-fn #'claudegel-prompt-continue
                       :label "continue"))

;;;###autoload
(defun claudegel-send-translate () (interactive)
       (claudegel-send :model "haiku" :tools 'none
                       :system-prompt-fn #'claudegel-prompt-translation
                       :mode 'replace :label "translate"))

;;;###autoload
(defun claudegel-send-prose () (interactive)
       (claudegel-send :model "sonnet" :tools 'none
                       :system-prompt-fn #'claudegel-prompt-prose
                       :label "prose"))

;; --- subprocess plumbing ---------------------------------------------------

(defun claudegel--build-args (model effort system-prompt-fn tools)
  (let ((args (list "-p"
                    "--input-format" "stream-json"
                    "--output-format" "stream-json"
                    "--verbose"
                    "--model" model
                    "--permission-mode" claudegel-default-permission-mode
                    "--system-prompt" (funcall system-prompt-fn))))
    (when claudegel-include-partial-messages
      (setq args (append args (list "--include-partial-messages"))))
    (pcase tools
      ('none      (setq args (append args (list "--tools" ""))))
      ('read-only (setq args (append args (cons "--tools" claudegel-read-only-tools))))
      ('all       nil)
      (_          nil))
    (when effort
      (setq args (append args (list "--effort" effort))))
    (when claudegel--session-id
      (setq args (append args (list "--resume" claudegel--session-id))))
    args))

(defun claudegel--start-process (prompt-text model effort system-prompt-fn
                                              tools insert-trailing-header)
  (let* ((process-environment
          (cons "ANTHROPIC_API_KEY="     ; clear it so OAuth keychain is used
                process-environment))
         (proc (make-process
                :name "claudegel"
                :buffer nil
                :command (cons claudegel-program
                               (claudegel--build-args model effort system-prompt-fn tools))
                :connection-type 'pipe
                :coding 'utf-8
                :noquery t
                :filter (claudegel--make-filter (current-buffer)
                                                insert-trailing-header)
                :sentinel (claudegel--make-sentinel (current-buffer))
                :stderr (claudegel--stderr-buffer))))
    (setq claudegel--process proc)
    (let ((msg (json-encode
                `(:type "user"
                  :message (:role "user"
                            :content ,prompt-text)))))
      (process-send-string proc (concat msg "\n"))
      (process-send-eof proc))))

(defun claudegel--stderr-buffer ()
  (let ((buf (get-buffer-create " *claudegel-stderr*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n--- " (format-time-string "%H:%M:%S") " ---\n"))
    buf))

(defun claudegel--make-sentinel (target-buffer)
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (when (buffer-live-p target-buffer)
        (with-current-buffer target-buffer
          (when (eq claudegel--process proc)
            (setq claudegel--process nil))
          (setq claudegel--line-buffer "")
          (when (and claudegel--insertion-marker
                     (not (string-prefix-p "finished" event)))
            (save-excursion
              (goto-char claudegel--insertion-marker)
              (insert (format "\n[claudegel: %s]" (string-trim event))))))))))

;; --- filter / event dispatch ----------------------------------------------

(defun claudegel--make-filter (target-buffer insert-trailing-header)
  (lambda (_proc chunk)
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (setq claudegel--line-buffer (concat claudegel--line-buffer chunk))
        (let ((lines (split-string claudegel--line-buffer "\n")))
          (setq claudegel--line-buffer (car (last lines)))
          (dolist (line (butlast lines))
            (unless (string-empty-p line)
              (claudegel--handle-line line insert-trailing-header))))))))

(defun claudegel--handle-line (line insert-trailing-header)
  (let ((event (condition-case err
                   (let ((json-object-type 'alist)
                         (json-array-type 'list)
                         (json-key-type 'symbol))
                     (json-read-from-string line))
                 (error
                  (with-current-buffer (claudegel--stderr-buffer)
                    (insert (format "JSON parse error: %s\nline: %s\n"
                                    (error-message-string err) line)))
                  nil))))
    (when event
      (claudegel--handle-event event insert-trailing-header))))

(defun claudegel--handle-event (event insert-trailing-header)
  (let ((type (alist-get 'type event)))
    (pcase type
      ("system"
       (when-let* ((sid (alist-get 'session_id event)))
         (setq claudegel--session-id sid)))
      ("stream_event"
       ;; Token-level deltas from --include-partial-messages.
       (claudegel--render-stream-event (alist-get 'event event)))
      ("assistant"
       ;; Consolidated message. With partial mode on, text/thinking already
       ;; arrived via stream_event; only tool_use needs rendering here
       ;; (the input json is much cleaner from the consolidated block).
       (let* ((message (alist-get 'message event))
              (content (alist-get 'content message)))
         (dolist (block content)
           (claudegel--render-block block))))
      ("user"
       (let* ((message (alist-get 'message event))
              (content (alist-get 'content message)))
         (when (listp content)
           (dolist (block content)
             (when (equal (alist-get 'type block) "tool_result")
               (claudegel--render-tool-result block))))))
      ("result"
       (claudegel--finalize event insert-trailing-header))
      (_ nil))))

(defun claudegel--render-stream-event (sev)
  (let ((etype (alist-get 'type sev)))
    (pcase etype
      ("content_block_start"
       (let* ((block (alist-get 'content_block sev))
              (btype (alist-get 'type block)))
         (pcase btype
           ("thinking" (setq claudegel--phase "thinking"))
           ("text"     (setq claudegel--phase "writing"))
           ("tool_use" (setq claudegel--phase
                             (format "tool: %s" (alist-get 'name block)))))))
      ("content_block_delta"
       (let* ((delta (alist-get 'delta sev))
              (dtype (alist-get 'type delta)))
         (pcase dtype
           ("text_delta"
            (claudegel--insert-text (alist-get 'text delta)))
           ("thinking_delta"
            (when claudegel-show-thinking
              (claudegel--insert-text (alist-get 'thinking delta))))
           (_ nil))))
      (_ nil))))

(defun claudegel--render-block (block)
  (let ((type (alist-get 'type block)))
    (pcase type
      ("text"
       (unless claudegel-include-partial-messages
         (claudegel--insert-text (alist-get 'text block))))
      ("thinking"
       (when (and claudegel-show-thinking
                  (not claudegel-include-partial-messages))
         (claudegel--insert-tool-folded
          "thinking" (alist-get 'thinking block) nil)))
      ("tool_use"
       (claudegel--insert-tool-use block))
      (_ nil))))

(defun claudegel--insert-text (text)
  (when (and text claudegel--insertion-marker)
    (save-excursion
      (goto-char claudegel--insertion-marker)
      (insert text)
      (set-marker claudegel--insertion-marker (point)))))

(defun claudegel--insert-tool-use (block)
  (let* ((id (alist-get 'id block))
         (name (alist-get 'name block))
         (input (alist-get 'input block))
         (summary (claudegel--tool-summary name input))
         (detail (json-encode input))
         (overlay (claudegel--insert-tool-folded name summary detail)))
    (when (member name '("Edit" "Write" "NotebookEdit"))
      (when-let* ((path (alist-get 'file_path input)))
        (cl-pushnew path claudegel--edited-files :test #'string=)))
    (when id
      (push (cons id overlay) claudegel--tool-overlays))))

(defun claudegel--tool-summary (name input)
  (let ((arg (or (alist-get 'file_path input)
                 (alist-get 'path input)
                 (alist-get 'pattern input)
                 (alist-get 'command input)
                 (alist-get 'url input)
                 "")))
    (format "%s%s" name
            (if (and (stringp arg) (not (string-empty-p arg)))
                (format ": %s" (truncate-string-to-width arg 80 nil nil "…"))
              ""))))

(defun claudegel--insert-tool-folded (kind summary detail)
  (save-excursion
    (goto-char claudegel--insertion-marker)
    (let* ((start (point)))
      (insert (propertize (format "\n  ▶ %s\n" summary)
                          'face 'claudegel-tool-face
                          'claudegel-tool-kind kind))
      (let* ((body-start (point))
             (body (or detail "")))
        (insert (propertize (concat body "\n")
                            'face 'claudegel-tool-detail-face))
        (let ((ov (make-overlay body-start (point))))
          (overlay-put ov 'invisible 'claudegel-fold)
          (overlay-put ov 'claudegel-tool t)
          (overlay-put ov 'claudegel-summary-pos start)
          (set-marker claudegel--insertion-marker (point))
          ov)))))

(defun claudegel--render-tool-result (block)
  (let* ((id (alist-get 'tool_use_id block))
         (content (alist-get 'content block))
         (text (cond
                ((stringp content) content)
                ((listp content)
                 (mapconcat (lambda (c) (or (alist-get 'text c) "")) content "\n"))
                (t "")))
         (ov (alist-get id claudegel--tool-overlays nil nil #'equal)))
    (when ov
      (save-excursion
        (goto-char (overlay-end ov))
        (insert (propertize (concat "→ " text "\n")
                            'face 'claudegel-tool-detail-face))
        (move-overlay ov (overlay-start ov) (point))
        (set-marker claudegel--insertion-marker (point))))))

(defun claudegel--finalize (event insert-trailing-header)
  (let ((is-error (eq (alist-get 'is_error event) t))
        (cost (alist-get 'total_cost_usd event)))
    (save-excursion
      (goto-char claudegel--insertion-marker)
      (when insert-trailing-header
        (insert claudegel-prompt-header))
      (when is-error
        (insert (propertize (format "[claudegel error: %s]\n"
                                    (alist-get 'result event))
                            'face 'error)))
      (set-marker claudegel--insertion-marker (point)))
    (add-to-invisibility-spec '(claudegel-fold . t))
    (when (and claudegel-auto-revert-on-edit claudegel--edited-files)
      (claudegel--revert-edited-buffers claudegel--edited-files))
    ;; Remember end-of-turn so the next send ships only the new user input.
    (setq claudegel--sent-end (copy-marker (point-max) nil)
          claudegel--phase nil)
    (message "claudegel: done%s%s"
             (if cost (format " ($%.4f)" cost) "")
             (if claudegel--edited-files
                 (format " — touched %d file(s)" (length claudegel--edited-files))
               ""))))

(defun claudegel--revert-edited-buffers (paths)
  (dolist (path paths)
    (when-let* ((buf (find-buffer-visiting path)))
      (with-current-buffer buf
        (when (and (not (buffer-modified-p))
                   (file-readable-p path))
          (revert-buffer t t t))))))

;; --- fold UI ---------------------------------------------------------------

;;;###autoload
(defun claudegel-toggle-fold-at-point ()
  "Toggle the fold of the tool call whose summary is at point."
  (interactive)
  (let* ((line-end (line-end-position))
         (ov (cl-find-if
              (lambda (o) (overlay-get o 'claudegel-tool))
              (overlays-in (line-beginning-position) (1+ line-end)))))
    (unless ov
      (setq ov (cl-find-if (lambda (o) (overlay-get o 'claudegel-tool))
                           (overlays-at (point)))))
    (if (not ov)
        (user-error "No tool call at point")
      (let ((hidden (eq (overlay-get ov 'invisible) 'claudegel-fold)))
        (overlay-put ov 'invisible (unless hidden 'claudegel-fold))
        (let ((sum-pos (overlay-get ov 'claudegel-summary-pos)))
          (when (and sum-pos (< sum-pos (point-max)))
            (save-excursion
              (goto-char sum-pos)
              (when (re-search-forward "[▶▼]" (line-end-position) t)
                (replace-match (if hidden "▼" "▶"))))))))))

;;;###autoload
(defun claudegel-scrub-tools ()
  "Delete every tool-call block in this buffer."
  (interactive)
  (let ((overlays (cl-remove-if-not
                   (lambda (o) (overlay-get o 'claudegel-tool))
                   (overlays-in (point-min) (point-max)))))
    (dolist (ov overlays)
      (let* ((sum (overlay-get ov 'claudegel-summary-pos))
             (start (or sum (overlay-start ov)))
             (end (overlay-end ov)))
        (when (and start end)
          (delete-region start end))
        (delete-overlay ov)))
    (message "claudegel: scrubbed %d tool calls" (length overlays))))

(provide 'claudegel)
;;; claudegel.el ends here
