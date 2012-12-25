(defvar darkroom-margins 0.15
  "Margins to use in darkroom-mode.

It's value can be:

- a floating point value betweeen 0 and 1, specifies percentage of
  window width in columns to use as a margin.

- a cons cell (LEFT RIGHT) specifying the left and right margins
  in columns.

- a function that returns a cons cell interpreted like the
  previous option.

Value is effective when `darkroom-minor-mode' is toggled, when
changing window or by calling `darkroom-set-margins'")

(defvar darkroom-turns-on-visual-line-mode t
  "If non-nil pair `visual-line-mode' with
  `darkroom-minor-mode'")
(defvar darkroom-fringes-outside-margins t
  "If non-nil use fringes outside margins for `darkroom-minor-mode'")

(defun darkroom-margins ()
  (cond ((functionp darkroom-margins)
         (funcall darkroom-margins))
        ((consp darkroom-margins)
         darkroom-margins)
        ((and (floatp darkroom-margins)
              (< darkroom-margins 1))
         (let ((delta (darkroom-float-to-columns darkroom-margins)))
           (cons delta delta)))))

(defun darkroom-float-to-columns (f)
  (ceiling (* (let ((edges (window-edges)))
                (- (nth 2 edges) (nth 0 edges)))
              f)))

(defun darkroom-set-margins (&optional margins)
  "Adjust margins to `darkroom-margins' or optional MARGINS."
  (let ((margins
         (or margins
             (darkroom-margins))))
    (when margins
      (set-window-margins (selected-window) (car margins) (cdr margins)))))

(defun darkroom-increase-margins ()
  (interactive)
  (when (floatp darkroom-margins)
    (setq darkroom-margins (+ 0.05 darkroom-margins))
    (darkroom-set-margins)))

(defun darkroom-decrease-margins ()
  (interactive)
  (when (floatp darkroom-margins)
    (setq darkroom-margins (- darkroom-margins 0.05))
    (darkroom-set-margins)))

(defun darkroom-fill-paragraph-maybe (really)
  (interactive "P")
  (cond (visual-line-mode
         (if (not really)
             (message "not filling paragraph")
           (call-interactively 'fill-paragraph)
           (message "filled paragraph even in visual-line-mode")))
        (t
         (call-interactively 'fill-paragraph))))

(defvar darkroom-minor-mode-map (let ((map (make-sparse-keymap)))
                                  (define-key map (kbd "C-M-+") 'darkroom-increase-margins)
                                  (define-key map (kbd "C-M--") 'darkroom-decrease-margins)
                                  (define-key map (kbd "M-q") 'darkroom-fill-paragraph-maybe)
                                  map))

(defvar darkroom-saved-mode-line-format nil)
(defvar darkroom-saved-visual-mode nil)
(define-minor-mode darkroom-minor-mode
  "A minor mode that emulates the darkroom editor."
  nil
  " dark"
  nil
  (cond (darkroom-minor-mode
         (setq darkroom-saved-mode-line-format mode-line-format
               mode-line-format nil)
         (setq fringes-outside-margins darkroom-fringes-outside-margins)
         (add-hook 'window-configuration-change-hook 'darkroom-set-margins nil t)
         (darkroom-set-margins)
         (setq header-line-format t)
         ;; a hack shoulnd't be needed but apparently is
         (set-window-buffer (selected-window) (current-buffer))
         (when darkroom-turns-on-visual-line-mode
           (visual-line-mode 1)))
        (t
         (setq header-line-format nil)
         (setq mode-line-format darkroom-saved-mode-line-format)
         (when darkroom-turns-on-visual-line-mode
           (visual-line-mode -1))
         (remove-hook 'window-configuration-change-hook 'darkroom-set-margins t)
         (set-window-margins (selected-window) 0 0))))




(provide 'darkroom)
