(defun beginning-of-line-or-indentation ()
  (interactive)
  (let ((previous-point (point)))
    (back-to-indentation)
    (if (equal previous-point (point))
        (beginning-of-line))))

(defun python-add-breakpoint ()
  "Adding a breakpoint to a python code."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "breakpoint()")
  (highlight-lines-matching-regexp "^[ ]*breakpoint()"))

(defun ci--flash-region (start end)
  "This time with a different face"
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'shadow)
    (overlay-put overlay 'priority 100)
    (run-with-timer 0.2 nil 'delete-overlay overlay)))

(defun my-copy-region (arg)
  "Copy with a flash"
  (interactive "P")
  (ci--flash-region (region-beginning) (region-end))
  (cua-copy-region arg)
  )

(defun add-correct-start-of-commit (arg)
  "Copy branch name and insert it at the beginning of commit."
  (interactive "P")
  (save-excursion
    (search-forward "On branch ")
    ;; (forward-line 5)
    ;; (forward-word 3)
    ;; (forward-char)
    (cua-set-mark)
    (move-end-of-line arg)
    (my-copy-region arg)
    )
  (cua-paste arg)
  )

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent)
  )

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (forward-line -1)
  (vi-open-line-below)
  )

(defun backward-kill-word-or-region (arg)
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (cua-cut-region arg)
      (kill-word
       (if (boundp 'arg)
           -1
         (* -1 arg)))
      )
    )
  )

(defun kill-word-or-region (arg)
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (cua-cut-region arg)
      (kill-word arg)
      )
    )
  )

(defun my-copy-word-or-region (arg)
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (my-copy-region arg)
      (my-copy-word arg)
      )
    )
  )

(defun my-copy-word (arg)
  (interactive "P")
  (save-excursion
    (cua-set-mark)
    (right-word)
    (my-copy-region arg)
    )
  )

(defun my-backward-copy-word-or-region (arg)
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (my-copy-region arg)
      (my-backward-copy-word arg)
      )
    )
  )

(defun my-backward-copy-word (arg)
  (interactive "P")
  (save-excursion
    (cua-set-mark)
    (left-word)
    (my-copy-region arg)
    )
  )

(defun copy-whole-line (arg)
  "Copy whole line"
  (interactive "P")
  (save-excursion
    (move-beginning-of-line arg)
    (cua-set-mark)
    (forward-line)
    (my-copy-region arg)
    )
  )

(defun copy-whole-line-or-region (arg)
  "Copy line or region"
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (my-copy-region arg)
      (copy-whole-line arg)
      )
    )
   )

(defun kill-whole-line-or-region (arg)
  "Kills line or region"
  (interactive "P")
  (if (region-active-p)
      (cua-cut-region arg)
    (kill-whole-line arg)
    )
  )

(defun paste-in-new-line (arg)
  "Pasting for full line edits"
  (interactive "P")
  (move-beginning-of-line arg)
  (forward-line)
  (cua-paste arg)
  (if (/= (current-column) 0)
      (newline)
      )
  (forward-line -1)
  )

(defun paste-from-kill-ring-new-line (arg)
  "Move to the new line and paste from the kill ring"
  (interactive "P")
  (move-beginning-of-line arg)
  (forward-line)
  (counsel-yank-pop)
  ;; (helm-show-kill-ring)
  (forward-line -1)
  )

(defun my-backward-word (arg)
  "Go backward by word unless doing so would put you in another line.
  Then, move to the beginning of the line."
  (interactive "P")
  (let ((wrong-flag 0))
    (let ((line-before-move (line-number-at-pos)))
      (save-excursion
        (backward-word)
        (when (/= line-before-move (line-number-at-pos))
          (setq-local wrong-flag 1)
          )
        )
      (if (= wrong-flag 1)
          (progn
            (let ((column-before-move (current-column)))
              (defvar column-after-back-to-indentation)
              (setq-local column-after-back-to-indentation
                          (save-excursion
                            (back-to-indentation)
                            (current-column)
                            )
                          )
              (if (= column-before-move column-after-back-to-indentation)
                  (backward-word)
                (back-to-indentation))))
        (backward-word))))
  )

(defun my-forward-word (arg)
  "Go forward by word unless doing so would put you in another line.
  Then, move to the end of the line."
  (interactive "P")
  (let ((wrong-flag 0))
    (let ((line-before-move (line-number-at-pos)))
      (save-excursion
        (forward-word)
        (when (/= line-before-move (line-number-at-pos))
          (setq-local wrong-flag 1)
          )
        )
      (if (= wrong-flag 1)
          (progn
            (let ((column-before-move (current-column)))
              (defvar column-after-end-of-line)
              (setq-local column-after-end-of-line
                          (save-excursion
                            (move-end-of-line arg)
                            (current-column)
                            )
                          )
              (if (= column-before-move column-after-end-of-line)
                  (forward-word)
                (move-end-of-line arg))))
        (forward-word))))
  )

(defun mark-inside-or-not (arg)
  "Mark inside python string, but if not, just the word."
  (interactive "P")
  (er/mark-inside-python-string)
  (unless (region-active-p)
    (progn
      (superword-mode t)
      (my-forward-word arg)
      (cua-set-mark)
      (my-backward-word arg)
      (superword-mode nil)
      )
    )
  )

(defun copy-inside-or-not (arg)
  "Copy inside python string, but if not, just the word."
  (interactive "P")
  (save-excursion
    (mark-inside-or-not arg)
    (ci--flash-region (region-beginning) (region-end))
    (copy-whole-line-or-region arg)
    )
  )

(defun kill-inside-or-not (arg)
  "Kill inside python string, but if not, just the word."
  (interactive "P")
  (er/mark-inside-python-string)
  (unless (region-active-p)
    (progn
      (superword-mode t)
      (my-forward-word arg)
      (cua-set-mark)
      (my-backward-word arg)
      (superword-mode nil)
      )
    )
  (kill-whole-line-or-region arg)
  )

(defun mark-outside-or-not (arg)
  "Mark outside python string, but if not, just the word."
  (interactive "P")
  (er/mark-outside-python-string)
  (unless (region-active-p)
    (progn
      (superword-mode t)
      (my-forward-word arg)
      (right-char)
      (cua-set-mark)
      (my-backward-word arg)
      (left-char)
      (superword-mode nil)
      )
    )
  )

(defun copy-outside-or-not (arg)
  "Copy outsize python string, but if not, just the word."
  (interactive "P")
  (save-excursion
    (mark-outside-or-not arg)
    (let ((overlay (make-overlay (region-beginning) (region-end))))
      (overlay-put overlay 'face 'shadow)
      (overlay-put overlay 'priority 100)
      (run-with-timer 0.2 nil 'delete-overlay overlay))
    (copy-whole-line-or-region arg)
    )
  )

(defun kill-outside-or-not (arg)
  "Kill outside python string, but if not, just the word."
  (interactive "P")
  (er/mark-outside-python-string)
  (unless (region-active-p)
    (progn
      (superword-mode t)
      (my-forward-word arg)
      (cua-set-mark)
      (my-backward-word arg)
      (superword-mode nil)
      )
    )
  (kill-whole-line-or-region arg)
  )

(defun mark-until-end-of-line (arg)
  "Mark from point until eol."
  (interactive "P")
  (cua-set-mark)
  (move-end-of-line arg)
  )

(defun blacken-region (arg)
  "Select a black-able piece of code"
  (interactive "P")
  (let* (
         (original-buffer (current-buffer))
         (original-point (point))
         )
    (er/expand-region 10)
    (er/expand-region -1)
    (cua-cut-region arg)
    (generate-new-buffer "new_buffer.py")
    (switch-to-buffer "new_buffer.py")
    (cua-paste arg)
    (blacken-buffer arg)
    (mark-whole-buffer)
    (cua-cut-region arg)
    (kill-buffer)
    (cua-paste arg)
    (goto-char original-point)
    )
  )

(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

(defun counsel-projectile-ag-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-projectile-ag))

(defun swiper-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'swiper))

(defun change-inner-with-fixed-arg* (argument yank? search-forward-char)
  "My fork for change-inner. Will be used for parens."
  (let* ((expand-region-fast-keys-enabled nil)
         (char (or search-forward-char argument))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (when search-forward-char
      (search-forward char (point-at-eol)))
    (flet ((message (&rest args) nil))
      (er--expand-region-1)
      (er--expand-region-1)
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er--expand-region-1))
      (if (not (looking-at q-char))
          (if search-forward-char
              (error "Couldn't find any expansion starting with %S" char)
            (goto-char starting-point)
            (setq mark-active nil)
            (change-inner* yank? char))
        (er/contract-region 1)
        (if yank?
            (progn
              (copy-region-as-kill (region-beginning) (region-end))
              (ci--flash-region (region-beginning) (region-end))
              (goto-char starting-point))
          (kill-region (region-beginning) (region-end)))))))

(defun change-inner-with-paren (arg)
  (interactive "P")
  (change-inner-with-fixed-arg* "(" arg nil))

(defun copy-inner-with-paren ()
  (interactive)
  (change-inner-with-fixed-arg* "(" t nil))

(defun change-inner-with-square (arg)
  (interactive "P")
  (change-inner-with-fixed-arg* "[" arg nil))

(defun copy-inner-with-square ()
  (interactive)
  (change-inner-with-fixed-arg* "[" t nil))

(defun change-inner-with-curly (arg)
  (interactive "P")
  (change-inner-with-fixed-arg* "{" arg nil))

(defun copy-inner-with-curly ()
  (interactive)
  (change-inner-with-fixed-arg* "{" t nil))

(defun change-outer-with-fixed-arg* (argument yank? search-forward-char)
  "My fork for change-outer. Will be used for parens."
  (let* ((expand-region-fast-keys-enabled nil)
         (char (or search-forward-char argument))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (when search-forward-char
      (search-forward char (point-at-eol)))
    (flet ((message (&rest args) nil))
      (when (looking-at q-char)
        (er/expand-region 1))
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er/expand-region 1))
      (if (not (looking-at q-char))
          (if search-forward-char
              (error "Couldn't find any expansion starting with %S" char)
            (goto-char starting-point)
            (setq mark-active nil)
            (change-outer* yank? char))
        (if yank?
            (progn
              (copy-region-as-kill (region-beginning) (region-end))
              (ci--flash-region (region-beginning) (region-end))
              (goto-char starting-point))
          (kill-region (region-beginning) (region-end)))))))

(defun change-outer-with-paren (arg)
  (interactive "P")
  (change-outer-with-fixed-arg* "(" arg nil))

(defun copy-outer-with-paren ()
  (interactive)
  (change-outer-with-fixed-arg* "(" t nil))

(defun change-outer-with-square (arg)
  (interactive "P")
  (change-outer-with-fixed-arg* "[" arg nil))

(defun copy-outer-with-square ()
  (interactive)
  (change-outer-with-fixed-arg* "[" t nil))

(defun change-outer-with-curly (arg)
  (interactive "P")
  (change-outer-with-fixed-arg* "{" arg nil))

(defun copy-outer-with-curly ()
  (interactive)
  (change-outer-with-fixed-arg* "{" t nil))

(defun awesome-tab-switch-group (&optional groupname)
  "Fork of awesome-tab's function to use ivy, not ido"
  (interactive)
  (let* ((tab-buffer-list (mapcar
                           #'(lambda (b)
                               (with-current-buffer b
                                 (list (current-buffer)
                                       (buffer-name)
                                       (funcall awesome-tab-buffer-groups-function) )))
                           (funcall awesome-tab-buffer-list-function)))
         (groups (awesome-tab-get-groups))
         (group-name (or groupname (completing-read "Groups: " groups))) )
    (catch 'done
      (mapc
       #'(lambda (group)
           (when (equal group-name (car (car (cdr (cdr group)))))
             (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list) )))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting.
Taken from https://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun nav-backward-indent (arg)
  "Move backward to the previous indent level."
  (interactive "P")
  (let ((current (current-column))
        (modulo (mod (current-column) 4))
        )
    (when (= modulo 0)
      (setq modulo 4))
    (move-to-column (- current modulo))
    )
  )

(defun nav-forward-indent (arg)
  "Move backward to the previous indent level."
  (interactive "P")
  (let ((current (current-column))
        (modulo (mod (current-column) 4))
        )
    (when (= modulo 0)
      (setq modulo 4))
    (move-to-column (+ current modulo))
    )
  )

(defun scroll-down-and-recenter (arg)
  (interactive "P")
  (forward-line -30)
  (recenter)
  )

(defun scroll-up-and-recenter (arg)
  (interactive "P")
  (forward-line 30)
  (recenter)
  )

(defun what-line ()
  (save-excursion
    (beginning-of-line)
    ;; (message "%d"
    (1+ (count-lines 1 (point)))))

(defun is-last-char-in-line (arg)
  (interactive "P")
  (save-excursion
    (let ((line-first (what-line)))
      (right-char)
      (listp
       (= line-first (what-line))
       )
      )
    )
  )

(defun delete-horizontal-and-vertical-space (arg)
  (interactive "P")
  (delete-horizontal-space)
  (if (= (current-column) 0)
      (progn
        (backward-delete-char-untabify 1)
        (delete-horizontal-and-vertical-space arg)
        )
    )
  (if (is-last-char-in-line 1)
      (progn
        (delete-char 1)
        (delete-horizontal-and-vertical-space arg)
        )
    )
  )

(defun delete-horizontal-and-vertical-space-but-leave-one-space (arg)
  (interactive "P")
  (delete-horizontal-space)
  (if (= (current-column) 0)
      (progn
        (backward-delete-char-untabify 1)
        (delete-horizontal-and-vertical-space arg)
        )
    )
  (if (is-last-char-in-line 1)
      (progn
        (delete-char 1)
        (delete-horizontal-and-vertical-space arg)
        )
    )
  (insert " ")
  (backward-char 1)
  )

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ryo-modal-on ()
  (interactive)
  (when (not (bound-and-true-p ryo-modal-mode))
    (ryo-modal-mode 1))
  )

(defun ryo-modal-off ()
  (interactive)
  (when (bound-and-true-p ryo-modal-mode)
    (ryo-modal-mode 0))
  )

(defun save-and-enter-ryo ()
  (interactive)
  (save-buffer)
  (ryo-modal-on)
  )

(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(defun go-to-119 ()
  (interactive)
  (move-to-column 119 t))

(defun wrap-python-string (arg)
  (interactive "P")
  (mark-inside-or-not arg)
  (let ((end-of-string (region-end)))
    (go-to-column (- fill-column 1))
    (while (> end-of-string (point))
      (insert "'")
      (smart-newline)
      (insert "'")
      (go-to-column (- fill-column 1))
      )
    )
  )

(defun kill-all-buffers-but-scratch ()
  "Kill all buffers."
  (interactive)
  (switch-to-buffer "*scratch*")
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun magic-elpy-nav-forward-block (arg)
  (interactive "P")
  (let ((current (current-column)))
    (move-beginning-of-line arg)
    (elpy-nav-forward-block)
    (move-to-column current t)
    )
  )

(defun magic-elpy-nav-backward-block (arg)
  (interactive "P")
  (let ((current (current-column)))
    (move-beginning-of-line arg)
    (elpy-nav-backward-block)
    (move-to-column current t)
    )
  )
