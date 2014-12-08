;;; darkroom.el --- Remove visual distractions and focus on writing  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, emulations
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main entrypoints to this extension are two minor modes
;;
;;    M-x darkroom-mode
;;    M-x darkroom-tentative-mode
;;
;; The first makes current buffer to enter `darkroom-mode'
;; immediately: keeping the window configuration untouched, text is
;; enlarged, centered on the window with margins, and the modeline is
;; elided.
;;
;; The second, `darkroom-tentative-mode', makes the current buffer
;; turn on `darkroom-mode' whenever all other windows are deleted,
;; i.e. the buffer is solo on the current Emacs frame. Whenever the
;; window is split to display some other buffer, the original buffer's
;; configuration is reset.
;;
;; Personally, I always use `darkroom-tentative-mode'.
;;
;; See also the customization options `darkroom-margins' and
;; `darkroom-fringes-outside-margins', which affect both modes.

;;; Code:

(defgroup darkroom nil
  "Remove visual distractions and focus on writing"
  :prefix "darkroom-"
  :group 'emulations)

(defcustom darkroom-margins 0.15
  "Margins to use in `darkroom-mode'.

Its value can be:

- a floating point value betweeen 0 and 1, specifies percentage of
  window width in columns to use as a margin.

- a cons cell (LEFT RIGHT) specifying the left and right margins
  in columns.

- a function of no arguments that returns a cons cell interpreted
  like the previous option.

Value is effective when `darkroom-mode' is toggled, when
changing window or by calling `darkroom-set-margins'"
  :type 'float
  :group 'darkroom)

(defcustom darkroom-text-scale-increase 2
  "Steps to increase text size when in `darkroom-mode'.
Value is passed to `text-scale-increase'."
  :type 'integer
  :group 'darkroom)

(defcustom darkroom-fringes-outside-margins t
  "If non-nil use fringes outside margins for `darkroom-mode'"
  :type 'boolean
  :group 'darkroom)

(defcustom darkroom-margin-increment 0.05
  "Increment to add used in `darkroom-increase-margins'."
  :type 'float
  :group 'darkroom)

(defun darkroom-compute-margins ()
  (or darkroom-buffer-margins
      (cond ((functionp darkroom-margins)
             (funcall darkroom-margins))
            ((consp darkroom-margins)
             darkroom-margins)
            ((and (floatp darkroom-margins)
                  (< darkroom-margins 1))
             (let ((delta (darkroom-float-to-columns darkroom-margins)))
               (cons delta delta))))))

(defun darkroom-float-to-columns (f)
  (ceiling (* (let ((edges (window-edges)))
                (- (nth 2 edges) (nth 0 edges)))
              f)))

(defvar darkroom-buffer-margins nil
  "Buffer-local version of `darkroom-margins' defcustom.
Set by `darkroom-set-margins'")

(defun darkroom-set-margins (&optional margins)
  "Set margins from MARGINS or `darkroom-compute-margins'."
  (let* ((window-configuration-change-hook nil))
    (when margins
      (when (null (car margins)) (setcar margins 0))
      (when (null (cdr margins)) (setcdr margins 0)))
    (set (make-local-variable 'darkroom-buffer-margins)
         (or margins (darkroom-compute-margins)))
    (walk-windows #'(lambda (w)
                      (when (eq (window-buffer w) (current-buffer))
                        (setq fringes-outside-margins
                              darkroom-fringes-outside-margins)
                        ;; See description of
                        ;; `fringes-outside-margins' for the reason
                        ;; for this apparent noop
                        (set-window-buffer w (current-buffer))
                        (set-window-margins w (car darkroom-buffer-margins)
                                            (cdr darkroom-buffer-margins))))
                  nil
                  'all-frames)))

(defun darkroom-increase-margins (increment)
  (interactive (list darkroom-margin-increment))
  (unless (and (consp darkroom-buffer-margins)
               (numberp (car darkroom-buffer-margins))
               (numberp (cdr darkroom-buffer-margins)))
    (error "`darkroom-buffer-margins' corrupted. Must be a cons of numbers."))
  (setcar darkroom-buffer-margins
          (round (* (+ 1 increment) (car darkroom-buffer-margins))))
  (setcdr darkroom-buffer-margins
          (round (* (+ 1 increment) (cdr darkroom-buffer-margins))))
  (darkroom-set-margins darkroom-buffer-margins))

(defun darkroom-decrease-margins (decrement)
  (interactive (list darkroom-margin-increment))
  (darkroom-increase-margins (- decrement)))

(defvar darkroom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-+") 'darkroom-increase-margins)
    (define-key map (kbd "C-M--") 'darkroom-decrease-margins)
    map))

(defvar darkroom-saved-mode-line-format nil)
(defvar darkroom-saved-header-line-format nil)
(defvar darkroom-saved-margins nil)

(define-minor-mode darkroom-mode
  "Remove visual distractions and focus on writing.
When this mode is active, everything but the buffer's text is
elided from view. The buffer margins are set so that text is
centered on screen. Text size is increased (display engine
allowing) by `darkroom-text-scale-increase'." nil nil nil
(cond (darkroom-mode
       (set (make-local-variable 'darkroom-saved-mode-line-format)
            mode-line-format)
       (set (make-local-variable 'darkroom-saved-header-line-format)
            header-line-format)
       (set (make-local-variable 'darkroom-saved-margins) (window-margins))
       (setq mode-line-format nil)
       (setq header-line-format nil)
       (darkroom-set-margins)
       (text-scale-increase darkroom-text-scale-increase)
       (add-hook 'window-configuration-change-hook 'darkroom-set-margins
                 nil t))
      (t
       (setq mode-line-format darkroom-saved-mode-line-format
             header-line-format darkroom-saved-header-line-format)
       (text-scale-decrease 2)
       (let (darkroom-buffer-margins)
         (darkroom-set-margins darkroom-saved-margins))
       (remove-hook 'window-configuration-change-hook 'darkroom-set-margins
                    t))))

(defun darkroom-maybe-enable ()
  (cond ((and (not darkroom-mode) (= (count-windows) 1))
         (darkroom-mode 1))
        ((and darkroom-mode (> (count-windows) 1))
         (darkroom-mode -1))
        (t
         ;; (message "debug: buffer: %s windows: %s darkroom-mode: %s"
         ;;          (current-buffer) (count-windows) darkroom-mode)
         )))


(define-minor-mode darkroom-tentative-mode
  "Minor mode that enters `darkroom-mode' when all windws are deleted"
  nil "DarkroomT" nil
  (cond (darkroom-tentative-mode
         (add-hook 'window-configuration-change-hook
                   'darkroom-maybe-enable nil t)
         (darkroom-maybe-enable))
        (t
         (if darkroom-mode (darkroom-mode -1))
         (remove-hook 'window-configuration-change-hook
                      'darkroom-maybe-enable t))))



(provide 'darkroom)
;;; darkroom.el ends here
