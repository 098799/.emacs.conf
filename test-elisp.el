;;; test-elisp.el --- ERT tests for custom elisp functions -*- lexical-binding: t -*-

;; Run with: emacs -batch -l ert -l elisp.el -l autoimport.el -l test-elisp.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cua-base)
(require 'python)
(cua-mode 1)
(transient-mark-mode 1)

;; Mock ci--flash-region (from change-inner package, not available in batch)
(unless (fboundp 'ci--flash-region)
  (defun ci--flash-region (start end)
    "Mock flash region for testing."
    nil))

;;; Tests for my-forward-word and my-backward-word

(ert-deftest test-my-forward-word-same-line ()
  "Forward word stays on same line when possible."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (my-forward-word nil)
    (should (= (line-number-at-pos) 1))
    (should (= (current-column) 5))))

(ert-deftest test-my-forward-word-stops-at-eol ()
  "Forward word stops at end of line when in middle of line."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char (point-min))
    (forward-char 2)  ; middle of "hello"
    (my-forward-word nil)  ; should stop at end of "hello", not wrap
    (should (= (line-number-at-pos) 1))
    (should (= (current-column) 5))))

(ert-deftest test-my-forward-word-crosses-when-at-eol ()
  "Forward word crosses to next line when already at end of line."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char 6)  ; after "hello", at newline
    (my-forward-word nil)
    (should (= (line-number-at-pos) 2))))

(ert-deftest test-my-backward-word-same-line ()
  "Backward word stays on same line when possible."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-max))
    (my-backward-word nil)
    (should (= (line-number-at-pos) 1))
    (should (= (current-column) 12))))

(ert-deftest test-my-backward-word-stops-at-indentation ()
  "Backward word stops at indentation when in middle of line."
  (with-temp-buffer
    (insert "hello\n  world")
    (goto-char (point-max))
    (backward-char 2)  ; middle of "world"
    (my-backward-word nil)  ; should stop at indentation, not wrap
    (should (= (line-number-at-pos) 2))
    (should (= (current-column) 2))))  ; at indentation

(ert-deftest test-my-backward-word-crosses-when-at-indentation ()
  "Backward word crosses to previous line when at indentation."
  (with-temp-buffer
    (insert "hello\n  world")
    (goto-char 9)  ; at start of "world" after indentation
    (my-backward-word nil)  ; goes to indentation
    (my-backward-word nil)  ; should now cross to previous line
    (should (= (line-number-at-pos) 1))))

;;; Tests for is-beginning-of-word and is-end-of-word

(ert-deftest test-is-beginning-of-word ()
  "Correctly identifies beginning of word."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (should (is-beginning-of-word))
    (goto-char 7)  ; at 'w' of world
    (should (is-beginning-of-word))
    (goto-char 3)  ; middle of hello
    (should-not (is-beginning-of-word))))

(ert-deftest test-is-end-of-word ()
  "Correctly identifies end of word."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 6)  ; after 'o' of hello
    (should (is-end-of-word))
    (goto-char (point-max))
    (should (is-end-of-word))
    (goto-char 3)  ; middle of hello
    (should-not (is-end-of-word))))

;;; Tests for autoimport

(ert-deftest test-autoimport-lookup-known-symbol ()
  "Autoimport finds symbol in my-symbol-map."
  (with-temp-buffer
    (python-mode)
    (insert "tqdm")
    (goto-char (point-min))
    (autoimport)
    (should (string-match-p "from tqdm import tqdm" (buffer-string)))))

(ert-deftest test-autoimport-inserts-after-imports ()
  "Autoimport inserts after existing imports."
  (with-temp-buffer
    (python-mode)
    (insert "import os\nimport sys\n\ntqdm")
    (goto-char (- (point-max) 4))  ; at "tqdm"
    (autoimport)
    (goto-char (point-min))
    (should (re-search-forward "^import sys\nfrom tqdm import tqdm" nil t))))

(ert-deftest test-autoimport-at-top-when-no-imports ()
  "Autoimport inserts at top when no existing imports."
  (with-temp-buffer
    (python-mode)
    (insert "def foo():\n    tqdm")
    (goto-char (- (point-max) 4))  ; at "tqdm"
    (autoimport)
    (goto-char (point-min))
    (should (looking-at "from tqdm import tqdm"))))

(ert-deftest test-autoimport-multiple-known-symbols ()
  "Autoimport correctly handles multiple known symbols."
  (with-temp-buffer
    (python-mode)
    (insert "Path")
    (goto-char (point-min))
    (autoimport)
    (should (string-match-p "from pathlib import Path" (buffer-string)))
    (goto-char (point-max))
    (insert "\nnp")
    (goto-char (- (point-max) 2))
    (autoimport)
    (should (string-match-p "import numpy as np" (buffer-string)))))

;;; Tests for beginning-of-line-or-indentation

(ert-deftest test-beginning-of-line-or-indentation-from-middle ()
  "Goes to indentation when in middle of line."
  (with-temp-buffer
    (insert "    hello world")
    (goto-char (point-max))
    (beginning-of-line-or-indentation)
    (should (= (current-column) 4))))

(ert-deftest test-beginning-of-line-or-indentation-from-indentation ()
  "Goes to column 0 when already at indentation."
  (with-temp-buffer
    (insert "    hello world")
    (goto-char 5)  ; at 'h', the indentation point
    (beginning-of-line-or-indentation)
    (should (= (current-column) 0))))

;;; Tests for char-up-or-replace (quote wrapping)

(ert-deftest test-quote-wrapping-with-selection ()
  "Wraps selection in quotes when region is active."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))
    (char-up-or-replace ?\')
    (should (string= (buffer-string) "'hello'"))))

(ert-deftest test-quote-insert-without-selection ()
  "Inserts single quote when no region."
  (with-temp-buffer
    (char-up-or-replace ?\')
    (should (string= (buffer-string) "'"))))

;;; Tests for pair-up-or-replace (bracket wrapping)

(ert-deftest test-bracket-wrapping-with-selection ()
  "Wraps selection in brackets when region is active."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))
    (pair-up-or-replace ?\[ ?\])
    (should (string= (buffer-string) "[hello]"))))

;;; Tests for copy-whole-line

(ert-deftest test-copy-whole-line ()
  "Copies entire line to kill ring."
  (with-temp-buffer
    (insert "line one\nline two\nline three")
    (goto-char 10)  ; middle of line two
    (copy-whole-line)
    (should (string= (car kill-ring) "line two\n"))))

;;; Tests for kill-whole-line-or-region

(ert-deftest test-kill-whole-line-or-region-line ()
  "Kills entire line when no region."
  (with-temp-buffer
    (insert "line one\nline two\nline three")
    (goto-char 10)  ; line two
    (kill-whole-line-or-region)
    (should (string= (buffer-string) "line one\nline three"))))

(ert-deftest test-kill-whole-line-or-region-region ()
  "Kills region when active using transient-mark-mode."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "hello world")
    (goto-char 1)
    (push-mark (point) t t)  ; activate mark properly
    (goto-char 6)  ; select "hello"
    ;; Verify region is active before killing
    (should (region-active-p))
    (kill-region (region-beginning) (region-end))
    (should (string= (buffer-string) " world"))))

;;; Tests for delete-horizontal-and-vertical-space

(ert-deftest test-delete-horizontal-space-basic ()
  "Deletes horizontal whitespace."
  (with-temp-buffer
    (insert "hello     world")
    (goto-char 6)  ; after "hello"
    (delete-horizontal-space)
    (should (string= (buffer-string) "helloworld"))))

;;; Tests for er-switch-to-previous-buffer

(ert-deftest test-er-switch-to-previous-buffer ()
  "Switches to other buffer."
  (let ((buf1 (generate-new-buffer "test-buf-1"))
        (buf2 (generate-new-buffer "test-buf-2")))
    (unwind-protect
        (progn
          (switch-to-buffer buf1)
          (switch-to-buffer buf2)
          (er-switch-to-previous-buffer)
          (should (eq (current-buffer) buf1)))
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; Tests for count-initial-spaces

(ert-deftest test-count-initial-spaces ()
  "Counts leading spaces correctly."
  (with-temp-buffer
    (insert "    hello")
    (goto-char (point-max))
    (should (= (count-initial-spaces) 4))))

(ert-deftest test-count-initial-spaces-no-indent ()
  "Returns 0 for no indentation."
  (with-temp-buffer
    (insert "hello")
    (should (= (count-initial-spaces) 0))))

;;; ============================================================
;;; Tests for triple-quoted string functions
;;; ============================================================

(ert-deftest test-mark-triple-quoted-string-inside-basic ()
  "Mark inside triple-quoted string."
  (with-temp-buffer
    (python-mode)
    (insert "\"\"\"Hello world\"\"\"")
    (goto-char 8)  ;; inside "Hello"
    (let ((result (mark-triple-quoted-string-inside)))
      (should result)
      (should (region-active-p))
      (should (string= (buffer-substring (region-beginning) (region-end))
                       "Hello world")))))

(ert-deftest test-mark-triple-quoted-string-inside-single-quotes ()
  "Mark inside triple single-quoted string."
  (with-temp-buffer
    (python-mode)
    (insert "'''Hello world'''")
    (goto-char 8)
    (let ((result (mark-triple-quoted-string-inside)))
      (should result)
      (should (region-active-p))
      (should (string= (buffer-substring (region-beginning) (region-end))
                       "Hello world")))))

(ert-deftest test-mark-triple-quoted-string-inside-multiline ()
  "Mark inside multiline triple-quoted string."
  (with-temp-buffer
    (python-mode)
    (insert "\"\"\"Line 1\nLine 2\nLine 3\"\"\"")
    (goto-char 10)  ;; somewhere in the middle
    (let ((result (mark-triple-quoted-string-inside)))
      (should result)
      (should (region-active-p))
      (should (string= (buffer-substring (region-beginning) (region-end))
                       "Line 1\nLine 2\nLine 3")))))

(ert-deftest test-mark-triple-quoted-string-inside-not-in-string ()
  "Returns nil when not inside triple-quoted string."
  (with-temp-buffer
    (python-mode)
    (insert "regular text here")
    (goto-char 8)
    (should (null (mark-triple-quoted-string-inside)))
    (should-not (region-active-p))))

(ert-deftest test-mark-triple-quoted-string-outside-basic ()
  "Mark outside triple-quoted string (including quotes)."
  (with-temp-buffer
    (python-mode)
    (insert "\"\"\"Hello world\"\"\"")
    (goto-char 8)
    (let ((result (mark-triple-quoted-string-outside)))
      (should result)
      (should (region-active-p))
      (should (string= (buffer-substring (region-beginning) (region-end))
                       "\"\"\"Hello world\"\"\"")))))

(ert-deftest test-mark-triple-quoted-string-outside-single-quotes ()
  "Mark outside triple single-quoted string."
  (with-temp-buffer
    (python-mode)
    (insert "'''Hello world'''")
    (goto-char 8)
    (let ((result (mark-triple-quoted-string-outside)))
      (should result)
      (should (region-active-p))
      (should (string= (buffer-substring (region-beginning) (region-end))
                       "'''Hello world'''")))))

(ert-deftest test-mark-inside-or-not-triple-quoted ()
  "mark-inside-or-not handles triple-quoted strings."
  (with-temp-buffer
    (python-mode)
    (insert "x = \"\"\"docstring\"\"\"")
    (goto-char 10)  ;; inside "docstring"
    (mark-inside-or-not nil)
    (should (region-active-p))
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "docstring"))))

(ert-deftest test-mark-outside-or-not-triple-quoted ()
  "mark-outside-or-not handles triple-quoted strings."
  (with-temp-buffer
    (python-mode)
    (insert "x = \"\"\"docstring\"\"\"")
    (goto-char 10)
    (mark-outside-or-not nil)
    (should (region-active-p))
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "\"\"\"docstring\"\"\""))))

;;; ============================================================
;;; Tests for bracket operations (inner/outer with paren/square/curly)
;;; ============================================================

(ert-deftest test-mark-inner-with-paren ()
  "Mark inside parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(hello world)")
    (goto-char 7)
    (mark-inner-with-paren)
    (should (region-active-p))
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "hello world"))))

(ert-deftest test-mark-inner-with-square ()
  "Mark inside square brackets."
  (with-temp-buffer
    (python-mode)
    (insert "[item1, item2]")
    (goto-char 5)
    (mark-inner-with-square)
    (should (region-active-p))
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "item1, item2"))))

(ert-deftest test-mark-inner-with-curly ()
  "Mark inside curly braces."
  (with-temp-buffer
    (python-mode)
    (insert "{key: value}")
    (goto-char 5)
    (mark-inner-with-curly)
    (should (region-active-p))
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "key: value"))))

(ert-deftest test-mark-outer-with-paren ()
  "Mark outside parentheses (including parens)."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "func(hello world)")
    (goto-char 10)
    (mark-outer-with-paren)
    (should (region-active-p))
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "(hello world)"))))

(ert-deftest test-copy-inner-with-paren ()
  "Copy inside parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(hello world)")
    (goto-char 7)
    (copy-inner-with-paren)
    (should (string= (car kill-ring) "hello world"))))

(ert-deftest test-cut-inner-with-paren ()
  "Cut inside parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(hello world)")
    (goto-char 7)
    (cut-inner-with-paren nil)
    (should (string= (buffer-string) "()"))
    (should (string= (car kill-ring) "hello world"))))

(ert-deftest test-cut-outer-with-paren ()
  "Cut outside parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "func(hello world)")
    (goto-char 10)
    (cut-outer-with-paren nil)
    (should (string= (buffer-string) "func"))
    (should (string= (car kill-ring) "(hello world)"))))

(provide 'test-elisp)
;;; test-elisp.el ends here
