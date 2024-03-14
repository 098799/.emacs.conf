(defun beginning-of-line-or-indentation ()
  (interactive)
  (let ((previous-point (point)))
    (back-to-indentation)
    (if (equal previous-point (point))
        (beginning-of-line))))

(defun highlight-breakpoint ()
  (interactive)
  (highlight-lines-matching-regexp "^[ ]*breakpoint()"))

(defun python-add-breakpoint ()
  "Adding a breakpoint to a python code."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "breakpoint()")
  (highlight-breakpoint)
  )

(defun python-add-return ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "return")
  )

(defun python-add-pass ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "pass")
  )

(defun my-copy-region ()
  "Copy with a flash"
  (interactive)
  (ci--flash-region (region-beginning) (region-end))
  (cua-copy-region t)
  )

(defun add-correct-start-of-commit (arg)
  "Copy branch name and insert it at the beginning of commit."
  (interactive "P")
  (save-excursion
    (search-forward "On branch ")
    ;; (forward-line 5)
    (forward-word)
    (forward-word)
    (forward-char)
    (cua-set-mark)
    (subword-mode t)
    (forward-word 2)
    ;; (move-end-of-line arg)
    (my-copy-region)
    )
  (self-insert-command 1 91)
  (cua-paste arg)
  (self-insert-command 1 93)
  (self-insert-command 1 32)
  (ryo-modal-off)
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

(defun my-mark-word ()
  (interactive)
  (cua-set-mark)
  (my-forward-word 1)
  )

(defun my-copy-word ()
  (interactive)
  (save-excursion
    (my-mark-word)
    (my-copy-region)
    )
  )

(defun my-cut-word ()
  (interactive)
  (save-excursion
    (my-mark-word)
    (cua-cut-region t)
    )
  )

(defun my-copy-word-or-region ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (my-copy-region)
      (my-copy-word)
      )
    )
  )

(defun my-cut-word-or-region ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (cua-cut-region t)
      (my-cut-word)
      )
    )
  )

(defun my-change-word-or-region ()
  (interactive)
  (my-cut-word-or-region)
  (ryo-modal-off)
  )

(defun my-substitute-word-or-region ()
  (interactive)
  (my-cut-word-or-region)
  (insert (current-kill 1))
  )

(defun my-backward-mark-word ()
  (interactive)
  (cua-set-mark)
  (left-word)
  )

(defun my-backward-copy-word ()
  (interactive)
  (save-excursion
    (my-backward-mark-word)
    (my-copy-region)
    )
  )

(defun my-backward-cut-word ()
  (interactive)
  (save-excursion
    (my-backward-mark-word)
    (cua-cut-region t)
    )
  )

(defun my-backward-copy-word-or-region ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (my-copy-region)
      (my-backward-copy-word)
      )
    )
  )

(defun my-backward-cut-word-or-region ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (cua-cut-region t)
      (my-backward-cut-word)
      )
    )
  )

(defun my-backward-change-word-or-region ()
  (interactive)
  (my-backward-cut-word-or-region)
  (ryo-modal-off)
  )

(defun my-backward-substitute-word-or-region ()
  (interactive)
  (my-backward-cut-word-or-region)
  (insert (current-kill 1))
  )

(defun copy-whole-line ()
  "Copy whole line"
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (cua-set-mark)
    (forward-line)
    (my-copy-region)
    )
  )

(defun copy-whole-line-or-region ()
  "Copy line or region"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (my-copy-region)
      (copy-whole-line)
      )
    )
   )

(defun kill-whole-line-or-region ()
  "Kills line or region"
  (interactive)
  (if (region-active-p)
      (cua-cut-region t)
    (kill-whole-line nil)
    )
  )



(defun paste-in-new-line (arg)
  "Pasting for full line edits"
  (interactive "P")
  (move-beginning-of-line 1)
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
  (move-beginning-of-line 1)
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

(defun is-beginning-of-word ()
  (save-excursion
    (let ((previous-point (point)))
      (my-forward-word nil)
      (my-backward-word nil)
      (if (eq (point) previous-point)
          t
        nil
        )
      )
    )
  )

(defun is-end-of-word ()
  (save-excursion
    (let ((previous-point (point)))
      (my-backward-word nil)
      (my-forward-word nil)
      (if (eq (point) previous-point)
          t
        nil
        )
      )
    )
  )


(defun mark-inside-string-or-not (arg)
  "Mark just the word"
  (interactive "P")
  (let ((current-superword-state (bound-and-true-p superword-mode)))
    (progn
      (if (is-end-of-word)
          (progn
            (superword-mode t)
            (my-backward-word arg)
            (cua-set-mark)
            (my-forward-word arg)
            )
        (progn
          (superword-mode t)
          (my-forward-word arg)
          (cua-set-mark)
          (my-backward-word arg)
          )
        )
      (superword-mode current-superword-state)
      )
    )
  )

(defun mark-inside-or-not (arg)
  "Mark inside python string, but if not, just the word."
  (interactive "P")
  (er/mark-inside-python-string)
  (let ((current-superword-state (bound-and-true-p superword-mode)))
    (unless (region-active-p)
      (if (or (looking-at "'") (looking-at "\""))
          (progn (forward-char)
                 (er/mark-inside-python-string)
                 (unless (region-active-p)
                   (backward-char 2)
                   (er/mark-inside-python-string)
                   )
                 )
        (progn
          (mark-inside-string-or-not arg)
          (superword-mode current-superword-state)
          )
        )
      )
    )
  )

(defun copy-inside-string-or-not (arg)
  (interactive "P")
  (save-excursion
    (mark-inside-string-or-not arg)
    (ci--flash-region (region-beginning) (region-end))
    (copy-whole-line-or-region)
    )
  )

(defun copy-inside-or-not (arg)
  "Copy inside python string, but if not, just the word."
  (interactive "P")
  (save-excursion
    (mark-inside-or-not arg)
    (ci--flash-region (region-beginning) (region-end))
    (copy-whole-line-or-region)
    )
  )

;; (defun cut-inside-or-not (arg)
;;   "Kill inside python string, but if not, just the word."
;;   (interactive "P")
;;   (er/mark-inside-python-string)
;;   (unless (region-active-p)
;;     (progn
;;       (superword-mode t)
;;       (my-forward-word arg)
;;       (cua-set-mark)
;;       (my-backward-word arg)
;;       (superword-mode nil)
;;       )
;;     )
;;   (kill-whole-line-or-region)
;;   )

(defun cut-inside-string-or-not (arg)
  (interactive "P")
  (save-excursion
    (mark-inside-string-or-not arg)
    (kill-whole-line-or-region)
    )
  )

(defun cut-inside-or-not (arg)
  "Kill inside python string, but if not, just the word."
  (interactive "P")
  (save-excursion
    (mark-inside-or-not arg)
    (kill-whole-line-or-region)
    )
  )

(defun change-inside-string-or-not (arg)
  (interactive "P")
  (cut-inside-string-or-not arg)
  (ryo-modal-off)
  )

(defun change-inside-or-not (arg)
  "Cut inside or not, exit ryo."
  (interactive "P")
  (cut-inside-or-not arg)
  (ryo-modal-off)
  )

(defun substitute-inside-string-or-not ()
  (interactive)
  (cut-inside-string-or-not nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

(defun substitute-inside-or-not ()
  (interactive)
  (cut-inside-or-not nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

(defun substitute-with-kill-ring (closure)
  (let ((to-be-pasted (read-from-kill-ring)))
    (funcall closure nil)
    (insert to-be-pasted)
    )
  (pop kill-ring)
  )

(defun substitute-inside-or-not-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-inside-or-not)
  )

(defun substitute-inner-with-paren-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-inner-with-paren)
  )

(defun substitute-inner-with-square-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-inner-with-square)
  )

(defun substitute-inner-with-curly-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-inner-with-curly)
  )

(defun substitute-outside-or-not-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-outside-or-not)
  )

(defun substitute-outer-with-paren-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-outer-with-paren)
  )

(defun substitute-outer-with-square-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-outer-with-square)
  )

(defun substitute-outer-with-curly-with-kill-ring ()
  (interactive)
  (substitute-with-kill-ring 'cut-outer-with-curly)
  )

(defun mark-outside-or-not (arg)
  "Mark outside python string, but if not, just the word."
  (interactive "P")
  (er/mark-outside-python-string)
  (let ((current-superword-state (bound-and-true-p superword-mode)))
    (unless (region-active-p)
      (if (or (looking-at "'") (looking-at "\""))
          (progn (forward-char)
                 (er/mark-outside-python-string)
                 (unless (region-active-p)
                   (backward-char 2)
                   (er/mark-outside-python-string)
                   )
                 )
        (if (is-end-of-word)
            (progn
              (superword-mode t)
              (my-backward-word arg)
              (cua-set-mark)
              (my-forward-word arg)
              (superword-mode current-superword-state)
              )
          (progn
            (superword-mode t)
            (my-forward-word arg)
            (cua-set-mark)
            (my-backward-word arg)
            (superword-mode current-superword-state)
            )
          )
        )
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
    (copy-whole-line-or-region)
    )
  )

(defun cut-outside-or-not (arg)
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
  (kill-whole-line-or-region)
  )

(defun change-outside-or-not (arg)
  "Cut outside or not, exit ryo."
  (interactive "P")
  (cut-outside-or-not arg)
  (ryo-modal-off)
  )

(defun substitute-outside-or-not ()
  (interactive)
  (cut-outside-or-not nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

(defun mark-until-end-of-line (arg)
  "Mark from point until eol."
  (interactive "P")
  (cua-set-mark)
  (move-end-of-line arg)
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
          (kill-region (region-beginning) (region-end)))
        ))))

(defun mark-inner-with-fixed-arg* (argument search-forward-char)
  "Mark inner."
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
            (mark-inner-with-fixed-arg* argument char))
        (er/contract-region 1)
        ))))

(defun mark-inner-with-paren ()
  (interactive)
  (mark-inner-with-fixed-arg* "(" nil)
  )

(defun mark-inner-with-square ()
  (interactive)
  (mark-inner-with-fixed-arg* "[" nil)
  )

(defun mark-inner-with-curly ()
  (interactive)
  (mark-inner-with-fixed-arg* "{" nil)
  )

(defun cut-inner-with-paren (arg)
  (interactive "P")
  (change-inner-with-fixed-arg* "(" arg nil))

(defun change-inner-with-paren (arg)
  (interactive "P")
  (cut-inner-with-paren arg)
  (ryo-modal-off)
  )

(defun substitute-inner-with-paren ()
  (interactive)
  (cut-inner-with-paren nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

(defun copy-inner-with-paren ()
  (interactive)
  (change-inner-with-fixed-arg* "(" t nil))

(defun cut-inner-with-square (arg)
  (interactive "P")
  (change-inner-with-fixed-arg* "[" arg nil))

(defun change-inner-with-square (arg)
  (interactive "P")
  (cut-inner-with-square arg)
  (ryo-modal-off)
  )

(defun substitute-inner-with-square ()
  (interactive)
  (cut-inner-with-square nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

(defun copy-inner-with-square ()
  (interactive)
  (change-inner-with-fixed-arg* "[" t nil))

(defun cut-inner-with-curly (arg)
  (interactive "P")
  (change-inner-with-fixed-arg* "{" arg nil))

(defun change-inner-with-curly (arg)
  (interactive "P")
  (cut-inner-with-curly arg)
  (ryo-modal-off)
  )

(defun substitute-inner-with-curly ()
  (interactive)
  (cut-inner-with-curly nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

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

(defun mark-outer-with-fixed-arg* (argument search-forward-char)
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
            (mark-inner-with-fixed-arg* argument char))
        ))))

(defun mark-outer-with-paren ()
  (interactive)
  (mark-outer-with-fixed-arg* "(" nil)
  )

(defun mark-outer-with-square ()
  (interactive)
  (mark-outer-with-fixed-arg* "[" nil)
  )

(defun mark-outer-with-curly ()
  (interactive)
  (mark-outer-with-fixed-arg* "{" nil)
  )

(defun cut-outer-with-paren (arg)
  (interactive "P")
  (change-outer-with-fixed-arg* "(" arg nil))

(defun change-outer-with-paren (arg)
  (interactive "P")
  (cut-outer-with-paren arg)
  (ryo-modal-off)
  )

(defun substitute-outer-with-paren ()
  (interactive)
  (cut-outer-with-paren nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

(defun copy-outer-with-paren ()
  (interactive)
  (change-outer-with-fixed-arg* "(" t nil))

(defun cut-outer-with-square (arg)
  (interactive "P")
  (change-outer-with-fixed-arg* "[" arg nil))

(defun change-outer-with-square (arg)
  (interactive "P")
  (cut-outer-with-square arg)
  (ryo-modal-off)
  )

(defun substitute-outer-with-square ()
  (interactive)
  (cut-outer-with-square nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

(defun copy-outer-with-square ()
  (interactive)
  (change-outer-with-fixed-arg* "[" t nil))

(defun cut-outer-with-curly (arg)
  (interactive "P")
  (change-outer-with-fixed-arg* "{" arg nil))

(defun change-outer-with-curly (arg)
  (interactive "P")
  (cut-outer-with-curly arg)
  (ryo-modal-off)
  )

(defun substitute-outer-with-curly ()
  (interactive)
  (cut-outer-with-curly nil)
  (insert (current-kill 1))
  (pop kill-ring)
  )

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
  (forward-line -40)
  (recenter)
  )

(defun scroll-up-and-recenter (arg)
  (interactive "P")
  (forward-line 40)
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

(defun ryo-modal-toggle ()
  (interactive)
  (if (bound-and-true-p ryo-modal-mode)
      (ryo-modal-mode 0)
    (ryo-modal-mode 1)
    )
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

(defun find-string-delimiter ()
  (interactive)
  (defvar return-column)
  (let ((found-flag nil))
    (while (not found-flag)
      (if (= ?' (char-after))
          (progn
            (setq found-flag t)
            (setq return-column (char-after))
            )
        (right-char)
        )
      )
    )
  return-column
  )

(defun split-string-if-over-120 (arg)
  (interactive "P")
    (let ((delimiter (find-string-delimiter)))
      (move-end-of-line arg)
      (let ((last-column (current-column)))
        (when (> last-column 121)
          (progn
            (go-to-119)
            (insert-char delimiter)
            (insert-char delimiter)
            (left-char)
            (smart-newline)
            (indent-for-tab-command)
            (split-string-if-over-120 arg)
            )
          )
        )
      )
    )

(defun count-initial-spaces ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (while (eq ?\s (char-after))
      (right-char)
      )
    (defvar col)
    (setq col (current-column))
    col
    )
  )

(defun how-many-lines-with-same-indent ()
  (interactive)
  (defvar how-many)
  (save-excursion
    (let ((current-line (what-line))
          (current-indent (count-initial-spaces)))
      (forward-line)
      (while (eq current-indent (count-initial-spaces))
        (forward-line)
        )
      (setq how-many (- (what-line) (+ current-line 1)))
      )
    (message "%s" how-many)
    how-many
    )
  )

(defun sort-indentation (arg)
  "I use it for sorting dictionaries in python tests."
  (interactive "P")
  (save-excursion
    (move-beginning-of-line 1)
    (cua-set-mark)
    (let ((how-many-lines (how-many-lines-with-same-indent)))
      ;; (forward-line how-many-lines)
      (forward-line how-many-lines)
      )
    (move-end-of-line arg)
    (sort-lines nil (region-beginning) (region-end))
    )
  )

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

(defun magic-elpy-nav-forward-class ()
  (interactive)
  (let ((current (current-column)))
    (move-beginning-of-line 1)
    (elpy-nav-forward-block)
    (move-to-column current t)
    )
  )

(defun magic-elpy-nav-backward-class ()
  (interactive)
  (let ((current (current-column)))
    (move-beginning-of-line 1)
    (elpy-nav-backward-block)
    (move-to-column current t)
    )
  )

(defun magic-elpy-nav-forward-method ()
  (interactive)
  (let ((current (current-column)))
    (move-to-column 4 t)
    (elpy-nav-forward-block)
    (move-to-column current t)
    )
  )

(defun magic-elpy-nav-backward-method ()
  (interactive)
  (let ((current (current-column)))
    (move-to-column 4 t)
    (elpy-nav-backward-block)
    (move-to-column current t)
    )
  )

;; Font?
;; FiraCode Nerd Font
;; RobotoMono Nerd Font
;; Ubuntu Mono

;; (defun big-font ()
;;   (interactive)
;; (custom-set-faces
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 170 :width normal :family "RobotoMono Nerd Font")))))
;;   )

;; (defun middle-font ()
;;   (interactive)
;; (custom-set-faces
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "RobotoMono Nerd Font")))))
;;   )

;; (defun small-font ()
;;   (interactive)
;; (custom-set-faces
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 106 :width normal :family "RobotoMono Nerd Font")))))
;;   )

;; (defun big-font ()
;;   (interactive)
;; (custom-set-faces
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 170 :width normal :family "FiraCode Nerd Font")))))
;;   )

;; (defun middle-font ()
;;   (interactive)
;; (custom-set-faces
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "FiraCode Nerd Font")))))
;;   )

;; (defun small-font ()
;;   (interactive)
;; (custom-set-faces
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 106 :width normal :family "FiraCode Nerd Font")))))
;;   )


(defun small-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "Ubuntu Mono")))))
  )

(defun middle-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :family "Ubuntu Mono")))))
  )

(defun big-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :family "Ubuntu Mono")))))
  )

(defun huge-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 220 :width normal :family "Ubuntu Mono")))))
  )

(defun char-up-or-replace (arg)
  (interactive "P")
  (if mark-active
      (progn
        (save-excursion
          (let ((reg-beg (region-beginning))
                (reg-end (region-end)))
            (goto-char reg-end)
            (insert-char arg)
            (goto-char reg-beg)
            (insert-char arg)
            )
          )
        (right-char)
        )
    (insert-char arg)
    )
  )

(defun pair-up-or-replace (arg1 arg2)
  (interactive "P")
  (if mark-active
      (progn
        (save-excursion
          (let ((reg-beg (region-beginning))
                (reg-end (region-end)))
            (goto-char reg-end)
            (insert-char arg2)
            (goto-char reg-beg)
            (insert-char arg1)
            )
          )
        (right-char)
        )
    (insert-char arg1)
    )
  )

(defun tild-up-or-replace (arg)
  (interactive "P")
  (char-up-or-replace ?~)
  )

(defun double-quote-up-or-replace (arg)
  (interactive "P")
  (char-up-or-replace ?\")
  )

(defun quote-up-or-replace (arg)
  (interactive "P")
  (char-up-or-replace ?\')
  )

(defun square-bracket-up-or-replace (arg)
  (interactive "P")
  (pair-up-or-replace ?\[ ?\])
  )

(defun curly-bracket-up-or-replace (arg)
  (interactive "P")
  (pair-up-or-replace ?\{ ?\})
  )

(defun query-replace-thing-at-point-or-selection (replace-str)
  (interactive "sDo query-replace current word with: ")
  (mark-inside-or-not nil)
  (copy-whole-line-or-region)
  (goto-char (region-beginning))
  (query-replace (current-kill 0) replace-str))

(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun put-into-clipboard (arg)
  "Put arg into clipboard"
  (when arg
    (with-temp-buffer
      (insert arg)
      (clipboard-kill-region (point-min) (point-max)))
    (message arg)
    )
  )

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (put-into-clipboard filename)
    filename
    ))

(defun get-class-name ()
  "Get the name of the python class in which you're currently."
  (interactive)
  (save-excursion
    (re-search-backward "class [A-Z][a-z]+")
    (right-char 6)
    (superword-mode t)
    (setq class-name (thing-at-point 'word))
    )
  class-name
  )

(defun test-class-string ()
  (interactive)
  (setq document-mapping
        '(
          ("document" . "ds")
          ("ml" . "ml")
          ("workflow" . "ws")
          ("annotation" . "as")
          ("ontology" . "os")
          ("user" . "us")
          ("resource" . "rs")
          ("search" . "ss")
          ("quota" . "qs")
          ("pythia" . "ps")
          )
        )
  (let ((service-list (split-string (my-put-file-name-on-clipboard) "_service/")))
    (let (
          (path (s-replace ".py" "" (s-replace "/" "." (car (last service-list)))))
          (service (nth 1 service-list))
          (class-name (get-class-name))
          )
      (concat "ltest " (cdr (assoc service document-mapping)) " " path "." class-name)
      )
    )
  )

(defun test-string ()
  (interactive)
  (concat (test-class-string) "." (get-test-name))
  )

(defun get-class-string ()
  (interactive)
  (put-into-clipboard (test-class-string))
  )

(defun get-test-name ()
  "Get the name of the unittest test function you're currently in."
  (interactive)
  (save-excursion
    (re-search-backward "def test_[a-z]+")
    (right-char 4)
    (superword-mode t)
    (setq test-name (thing-at-point 'word))
    )
  test-name
  )

(defun get-test-string ()
  "Create an appropriate testing string for legartis unittest"
  (interactive)
  (put-into-clipboard (test-string))
  )

(defun insert-class ()
  (interactive)
  (insert "class\\s")
  )

(defun swiper-region (arg)
  (interactive "P")
  (if (use-region-p)
      (swiper-thing-at-point)
    (swiper)
    )
  )

(defun comment-paragraph ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (comment-dwim-2)
    )
  )

(defun cdsitepackages ()
  (interactive)
  (dired "/home/tgrining/.virtualenvs/legartis/lib/python3.11/site-packages")
  )

(defun get-buffer-path ()
  (nth 1 (split-string (concat (pwd) (buffer-name))))
  )

(defun copy-buffer-path ()
  (interactive)
  (put-into-clipboard (get-buffer-path))
  )

(defun copy-thing-at-point ()
  (interactive)
  (mark-inside-or-not nil)
  (ci--flash-region (region-beginning) (region-end))
  (cua-copy-region t)
  )

(defun kill-thing-at-point ()
  (interactive)
  (mark-inside-or-not nil)
  (cua-cut-region t)
  )

(defun subword-on ()
  (interactive)
  (subword-mode 1)
  )

(defun subword-off ()
  (interactive)
  (subword-mode nil)
  )

(defun superword-on ()
  (interactive)
  (superword-mode 1)
  )

(defun superword-off ()
  (interactive)
  (superword-mode nil)
  )

(defun dired-mark-undo-tree ()
  (interactive)
  (dired-mark-files-regexp "~undo-tree~")
  )

(defun magit-diff-develop ()
  (interactive)
  (magit-diff-range "develop")
  (delete-other-windows)
  (ryo-modal-off)
  )

(defun helm-rg-not-at-point ()
  (interactive)
  (helm-rg nil)
  )

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (let ((value (file-truename buffer-file-name)))
          (kill-new value)
          (message value)
          )
  )
)

(defun join-with-slashes (a b)
  "Join strings a and b with a slash"
  (concat a "/" b)
  )

(defun copy-folder-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (let ((value (reduce #'join-with-slashes
             (let ((path-list (split-string (file-truename buffer-file-name) "/")))
               (let ((file-name (car (last path-list))))
                 (remove file-name path-list)
                 )
               )
          )))
      (kill-new value)
      (message value)
      )
    )
  )

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun importmagic-save-revert-and-fix ()
  (interactive)
  (save-buffer)
  ;; (revert-buffer-no-confirm)
  (importmagic-mode)
  (importmagic-mode)
  (importmagic-fix-symbol-at-point)
  )

(defun remove-imports ()
  (interactive)
  (shell-command (concat "remove_imports " (buffer-file-name)))
  )

(defun autoflake ()
  (interactive)
  (when (eq major-mode 'python-mode)
    (let ((buffer-name (file-truename buffer-file-name)))
      (shell-command (concat "/home/tgrining/.virtualenvs/legartis/bin/autoflake " buffer-name " --remove-all-unused-imports " "--in-place"))))
  )

;; (defun ivy-call-second-action ()
;;   (interactive)
;;   (ivy-exit-with-action
;;    (cadr (nth 1 (cdr (ivy-state-action ivy-last))))))

;; (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-call-second-action)

(defun centaur-restart ()
  (interactive)
  (centaur-tabs-mode 0)
  (centaur-tabs-mode 1)
  )

(defun xref-find-references-at-point ()
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd "RET")))
  (call-interactively 'xref-find-references)
  )

(defun example-function (arg)
  (interactive
   (list (read-string "Enter your value (default is foo): " nil nil "foo")))
  (message "Value is: %s" arg))

(defun delete-and-paste ()
  (interactive)
  (if (region-active-p)
      (progn
        (delete-region (region-beginning) (region-end))
        (cua-paste nil)
        )
      (cua-paste nil)
    )
  )

(defun invalidate-cache-and-counsel-projectile-find-file (arg)
  (interactive "P")
  (projectile-invalidate-cache arg)
  (counsel-projectile-find-file arg)
  )



(defvar my-symbol-map
  '(
    ("glob" . "from glob import glob")
    ("json" . "import json")
    ("requests" . "import requests")
    ("pytest" . "import pytest")
    ("tqdm" . "from tqdm import tqdm")
    ("Path" . "from pathlib import Path")
    ("partial" . "from functools import partial")
    ("cache" . "from functools import cache")
    ("count" . "from itertools import count")
    ("profile" . "from memory_profiler import profile")
    ("load_dotenv" . "from dotenv import load_dotenv")
    ("st" . "import streamlit as st")
    ("re" . "import re")
    ("pd" . "import pandas as pd")
    ("np" . "import numpy as np")
    ("torch" . "import torch")
    ("Q" . "from django.db.models import Q")
    ("Query" . "from fastapi import Query")
    ("BaseModel" . "from pydantic import BaseModel")
    ("Text" . "from pythia_service.document.models import Text")
    ("AutoModel" . "from transformers import AutoModel")
    ("AutoModelForQuestionAnswering" . "AutoModelForQuestionAnswering")
    ("AutoModelForSeq2SeqLM" . "from transformers import AutoModelForSeq2SeqLM")
    ("AutoTokenizer" . "from transformers import AutoTokenizer")
    ("T5ForConditionalGeneration" . "from transformers import T5ForConditionalGeneration")
    ("pipeline" . "from transformers import pipeline")
    ("Provision" . "from pythia_service.ontology.models import Provision")
    )
  )

(defun autoimport ()
  "Auto-import a symbol at point by finding the suitable place to insert the import statement in a Python buffer.
If no existing 'import' or 'from' statement is found, insert at the top of the file."
  (interactive)
  (let* ((sym (thing-at-point 'symbol))
         (import-stmt (cdr (assoc sym my-symbol-map))))
    (when import-stmt
      (save-excursion
        (goto-char (point-max)) ; Start from the end of the buffer
        ;; Search backward for the import or from patterns. If not found, go to the buffer's start.
        (unless (re-search-backward "^\\(import \\|from \\)" nil t)
          (goto-char (point-min)))
        ;; If found, move to the next line; if not found, we're already at the top.
        (when (looking-at "^\\(import \\|from \\)")
          (forward-line 1))
        ;; Insert the import statement.
        (insert import-stmt "\n")))))

(defun gptel-send-to-gpt4 ()
  (interactive)
  (let ((gptel-backend (gptel-make-openai
                         "Custom ChatGPT"
                         :models '("gpt-4-turbo-preview")
                         :key 'gptel-api-key
                         :stream t))
        (gptel-model "gpt-4-turbo-preview")
        (gptel--system-message "You are an AI assistant named ChatGPT. Please respond concisely and helpfully."))
    (gptel-send)
    (message "Query sent with custom model and directive.")))


;; Given this function, could you write a similar one that calls claude 3 opus?
