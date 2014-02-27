(defvar darkroom-margins 0.15
  "Margins to use in darkroom-mode.

It's value can be:

- a floating point value betweeen 0 and 1, specifies percentage of
  window width in columns to use as a margin.

- a cons cell (LEFT RIGHT) specifying the left and right margins
  in columns.

- a function that returns a cons cell interpreted like the
  previous option.

Value is effective when `darkroom-mode' is toggled, when
changing window or by calling `darkroom-set-margins'")

(defvar darkroom-turns-on-visual-line-mode t
  "If non-nil pair `visual-line-mode' with
  `darkroom-mode'")
(defvar darkroom-fringes-outside-margins t
  "If non-nil use fringes outside margins for `darkroom-mode'")

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
  (let* ((window-configuration-change-hook nil)
         (margins (or margins
                      (darkroom-margins))))
    (walk-windows #'(lambda (w)
                      (when (eq (window-buffer w) (current-buffer))
                        (setq fringes-outside-margins darkroom-fringes-outside-margins)
                        ;; See description of
                        ;; `fringes-outside-margins' for the reason
                        ;; for this apparent noop
                        (set-window-buffer w (current-buffer))
                        (set-window-margins w (car margins) (cdr margins))))
                  nil
                  'all-frames)))

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

(defvar darkroom-mode-map (let ((map (make-sparse-keymap)))
                                  (define-key map (kbd "C-M-+") 'darkroom-increase-margins)
                                  (define-key map (kbd "C-M--") 'darkroom-decrease-margins)
                                  (define-key map (kbd "M-q") 'darkroom-fill-paragraph-maybe)
                                  map))

(defvar darkroom-saved-mode-line-format nil)
(defvar darkroom-saved-header-line-format nil)
(defvar darkroom-saved-visual-line-mode nil)

(make-variable-buffer-local 'darkroom-saved-mode-line-format)
(make-variable-buffer-local 'darkroom-saved-header-line-format)
(make-variable-buffer-local 'darkroom-saved-visual-line-mode)

(defun darkroom-visual-mode-maybe-enable ()
  (when darkroom-turns-on-visual-line-mode
    (cond (darkroom-mode
           (setq darkroom-saved-visual-mode visual-line-mode)
           (visual-line-mode 1))
          (t
           (unless darkroom-saved-visual-line-mode
             (visual-line-mode -1))))))

(define-minor-mode darkroom-mode
  "Minor mode emulating the darkroom editor that I never used."
  nil nil nil
  (cond (darkroom-mode
         (setq darkroom-saved-mode-line-format mode-line-format
               mode-line-format nil
               darkroom-saved-header-line-format header-line-format
               header-line-format nil)
         (darkroom-set-margins)
         (darkroom-visual-mode-maybe-enable)
         (text-scale-increase 2)
         (add-hook 'window-configuration-change-hook 'darkroom-set-margins nil t))
        (t
         (setq mode-line-format darkroom-saved-mode-line-format
               header-line-format darkroom-saved-header-line-format)
         (text-scale-decrease 2)
         (darkroom-set-margins '(0 . 0))
         (darkroom-visual-mode-maybe-enable)
         (remove-hook 'window-configuration-change-hook 'darkroom-set-margins t))))

(defun darkroom-maybe-enable ()
  (cond ((and (not darkroom-mode) (= (count-windows) 1))
         (darkroom-mode 1)) 
        ((and darkroom-mode (> (count-windows) 1))
         (darkroom-mode -1))
        (t
         (message "Hmm buffer: %s windows: %s darkroom-mode: %s"
                  (current-buffer)
                  (count-windows)
                  darkroom-mode))))


(define-minor-mode darkroom-tentative-mode
  "Minor mode that enters `darkroom-mode' when all windws are deleted"
  nil "D" nil
  (cond (darkroom-tentative-mode
         (add-hook 'window-configuration-change-hook 'darkroom-maybe-enable nil t)
         (darkroom-maybe-enable))
        (t
         (if darkroom-mode (darkroom-mode -1))
         (remove-hook 'window-configuration-change-hook 'darkroom-maybe-enable t))))




(provide 'darkroom)
