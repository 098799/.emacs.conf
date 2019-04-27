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
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defun add-correct-start-of-commit (arg)
  "Copy branch name and insert it at the beginning of commit."
  (interactive "P")
  (save-excursion
    (forward-line 5)
    (forward-word 3)
    (forward-char)
    (cua-set-mark)
    (forward-word 2)
    (cua-copy-region arg)
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
        (cua-copy-region arg)
      (my-copy-word arg)
      )
    )
  )

(defun my-copy-word (arg)
  (interactive "P")
  (save-excursion
    (cua-set-mark)
    (right-word)
    (cua-copy-region arg)
    )
  )

(defun my-backward-copy-word-or-region (arg)
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (cua-copy-region arg)
      (my-backward-copy-word arg)
      )
    )
  )

(defun my-backward-copy-word (arg)
  (interactive "P")
  (save-excursion
    (cua-set-mark)
    (left-word)
    (cua-copy-region arg)
    )
  )

(defun copy-whole-line (arg)
  "Copy whole line"
  (interactive "P")
  (save-excursion
    (move-beginning-of-line arg)
    (cua-set-mark)
    (forward-line)
    (cua-copy-region arg)
    )
  )

(defun copy-whole-line-or-region (arg)
  "Copy line or region"
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (cua-copy-region arg)
      (copy-whole-line arg)
      )
    )
   )

(defun kill-whole-line-or-region (arg)
  "Kills line or region"
  (interactive "P")
  (if (region-active-p)
      (cua-cut-region arg)
    (kill-whole-line)
    )
  )

(defun paste-in-new-line (arg)
  "Pasting for full line edits"
  (interactive "P")
  (move-beginning-of-line arg)
  (forward-line)
  (cua-paste arg)
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
