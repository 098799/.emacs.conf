(defun ci--flash-region (start end)
  "This time with a different face"
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'shadow)
    (overlay-put overlay 'priority 100)
    (run-with-timer 0.2 nil 'delete-overlay overlay)))

(defun harpoon--hydra-candidates (method)
  "ChatGPT on harpoon"
  (let ((line-number 0)
        (keys '("a" "s" "d" "f" "q" "w" "e" "r" "z" "x" "c" "v"))
        (full-candidates (seq-take (delete "" (split-string (harpoon--get-file-text) "\n")) 10)))
    (mapcar (lambda (item)
              (setq line-number (+ 1 line-number))
              (list (nth (- line-number 1) keys)
                    (intern (concat method (nth (- line-number 1) keys)))
                    (harpoon--format-item-name item)
                    :column (if (< line-number 6) "1-5" "6-10")))
            full-candidates)))

;;;###autoload
(defun harpoon-delete-a ()
  (interactive)
  (harpoon--delete 1))

;;;###autoload
(defun harpoon-delete-s ()
  (interactive)
  (harpoon--delete 2))

;;;###autoload
(defun harpoon-delete-d ()
  (interactive)
  (harpoon--delete 3))

;;;###autoload
(defun harpoon-delete-f ()
  (interactive)
  (harpoon--delete 4))

;;;###autoload
(defun harpoon-delete-q ()
  (interactive)
  (harpoon--delete 5))

;;;###autoload
(defun harpoon-delete-w ()
  (interactive)
  (harpoon--delete 6))

;;;###autoload
(defun harpoon-delete-e ()
  (interactive)
  (harpoon--delete 7))

;;;###autoload
(defun harpoon-delete-r ()
  (interactive)
  (harpoon--delete 8))

;;;###autoload
(defun harpoon-delete-z ()
  (interactive)
  (harpoon--delete 9))

;;;###autoload
(defun harpoon-delete-x ()
  (interactive)
  (harpoon--delete 10))

;;;###autoload
(defun harpoon-delete-c ()
  (interactive)
  (harpoon--delete 11))

;;;###autoload
(defun harpoon-delete-v ()
  (interactive)
  (harpoon--delete 12))

;;;###autoload
(defun harpoon-go-to-a ()
  (interactive)
  (harpoon-go-to 1))

;;;###autoload
(defun harpoon-go-to-s ()
  (interactive)
  (harpoon-go-to 2))

;;;###autoload
(defun harpoon-go-to-d ()
  (interactive)
  (harpoon-go-to 3))

;;;###autoload
(defun harpoon-go-to-f ()
  (interactive)
  (harpoon-go-to 4))

;;;###autoload
(defun harpoon-go-to-q ()
  (interactive)
  (harpoon-go-to 5))

;;;###autoload
(defun harpoon-go-to-w ()
  (interactive)
  (harpoon-go-to 6))

;;;###autoload
(defun harpoon-go-to-e ()
  (interactive)
  (harpoon-go-to 7))

;;;###autoload
(defun harpoon-go-to-r ()
  (interactive)
  (harpoon-go-to 8))

;;;###autoload
(defun harpoon-go-to-z ()
  (interactive)
  (harpoon-go-to 9))

;;;###autoload
(defun harpoon-go-to-x ()
  (interactive)
  (harpoon-go-to 10))

;;;###autoload
(defun harpoon-go-to-c ()
  (interactive)
  (harpoon-go-to 11))

;;;###autoload
(defun harpoon-go-to-v ()
  (interactive)
  (harpoon-go-to 12))

;;;###autoload
(defun harpoon-quick-menu-hydra ()
  "Open harpoon quick menu with hydra."
  (interactive)
  (require 'hydra)
  (let ((candidates (harpoon--hydra-candidates "harpoon-go-to-")))
    (eval `(defhydra harpoon-hydra (:exit t :column 1)
             "

        ██╗  ██╗ █████╗ ██████╗ ██████╗  ██████╗  ██████╗ ███╗   ██╗
        ██║  ██║██╔══██╗██╔══██╗██╔══██╗██╔═══██╗██╔═══██╗████╗  ██║
        ███████║███████║██████╔╝██████╔╝██║   ██║██║   ██║██╔██╗ ██║
        ██╔══██║██╔══██║██╔══██╗██╔═══╝ ██║   ██║██║   ██║██║╚██╗██║
        ██║  ██║██║  ██║██║  ██║██║     ╚██████╔╝╚██████╔╝██║ ╚████║
        ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝      ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝
                                                            "
             ,@candidates
             ("SPC" harpoon-toggle-quick-menu "Open Menu" :column "Other Actions")
             ("j" harpoon-add-file "Save Current File to Harpoon" :column "Other Actions")
             ("k" harpoon-delete-item "Delete some harpoon" :column "Other Actions")
             ("l" harpoon-clear "Clear Harpoon" :column "Other Actions")
             (";" harpoon-toggle-file "Open Harpoon File" :column "Other Actions"))))

  (when (fboundp 'harpoon-hydra/body) (harpoon-hydra/body)))
