;;; darkroom.el --- Remove visual distractions and focus on writing  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, emulations
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.3

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

;; The main entrypoints to this extension are two minor modes:
;;
;;    M-x darkroom-mode
;;    M-x darkroom-tentative-mode
;;
;; `darkroom-mode' makes visual distractions disappear: the
;; mode-line is temporarily elided, text is enlarged and margins are
;; adjusted so that it's centered on the window.
;;
;; `darkroom-tentative-mode' is similar, but it doesn't immediately
;; turn-on `darkroom-mode', unless the current buffer lives in the
;; sole window of the Emacs frame (i.e. all other windows are
;; deleted). Whenever the frame is split to display more windows and
;; more buffers, the buffer exits `darkroom-mode'. Whenever they are
;; deleted, the buffer re-enters `darkroom-mode'.
;;
;; Personally, I always use `darkroom-tentative-mode'.
;;
;; See also the customization options `darkroom-margins' and
;; `darkroom-fringes-outside-margins', which affect both modes.

;;; Code:

(require 'cl-lib)
(require 'face-remap)

(defgroup darkroom nil
  "Remove visual distractions and focus on writing"
  :prefix "darkroom-"
  :group 'emulations)

(defcustom darkroom-margins 'darkroom-guess-margins
  "Margins to use in `darkroom-mode'.

Its value can be:

- a floating point value betweeen 0 and 1, specifies percentage of
  window width in columns to use as a margin.

- a cons cell (LEFT RIGHT) specifying the left and right margins
  in columns.

- a function of a single argument, a window, that returns a cons
  cell interpreted like the previous option. An example is
  `darkroom-guess-margins', which see. Beware that this function
  is called very often, so if it does some non-trivial processing
  on the buffer's text, consider caching that value.

Value is effective when `darkroom-mode' is toggled."
  :type '(choice float
                 (cons integer integer)
                 (function-item darkroom-guess-margins :doc "Guess margins")
                 (function darkroom-guess-margins))
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

(defcustom darkroom-margins-if-failed-guess 0.15
  "Margins when `darkroom-guess-margins' fails.
If `darkroom-guess-margins' failed to figure out margins to
center the text, use this percentage of window width for the
symmetical margins."
  :type 'float
  :group 'darkroom)

(defcustom darkroom-verbose nil
  "If non-nil, be verbose about darkroom operations."
  :type 'boolean
  :group 'darkroom)

(defvar darkroom--guess-margins-statistics-cache nil
  "Cache used by `darkroom-guess-margins'.")

(defun darkroom--window-width (&optional window)
  "Calculate width of WINDOW in columns, considering text scaling.
WINDOW defaults to the currently selected window. The function
assumes the buffer to be filled with at least one character of an
arbitrary, but fixed width. Narrowing is taken in consideration.
The return value is a cons (COLS . SCALED-CHAR-WIDTH) where COLS
is the desired width in columns and SCALED-CHAR-WIDTH is the
width in pixels of a single character."
  (when (= (point-min) (point-max))
    (error "Cannot calculate the width of a single character"))
  (let* ((window (or window (selected-window)))
         (scaled-char-width (car (window-text-pixel-size
                                  window
                                  (point-min) (1+ (point-min)))))
         (char-width (frame-char-width))
         (margins (window-margins window)))
    (cons (truncate
           (+ (window-width window 'pixelwise)
              (* char-width (or (car margins) 0))
              (* char-width (or (cdr margins) 0)))
           scaled-char-width)
          scaled-char-width)))

(defun darkroom-guess-margins (window)
  "Guess suitable margins for `darkroom-margins'.
If in suitable conditions, collect some statistics about the
buffer's line lengths, and apply a heuristic to figure out how
wide to set the margins, comparing it to WINDOW's width in
columns. If the buffer's paragraphs are mostly filled to
`fill-column', margins should center it on the window, otherwise,
the margins specified in `darkroom-margins-if-failed-guess'.

In any of these conditions,`darkroom-margins-if-failed-guess' is
also used:

* if `visual-line-mode' is on;
* if `variable-pitch-mode' is on;
* if the buffer is empty.

For testing purposes, WINDOW can also be an integer number which
is a width in columns, in which case it will be used instead of a
window's geometry."
  (if (or visual-line-mode
          (and buffer-face-mode
               (eq 'variable-pitch buffer-face-mode-face))
          (= (point-min) (point-max)))
      darkroom-margins-if-failed-guess
    (let* ((window-width-info (if (integerp window)
                                  window
                                (darkroom--window-width window)))
           (window-width (car window-width-info))
           (scaled-char-width (cdr window-width-info))
           (top-quartile-avg
            (or darkroom--guess-margins-statistics-cache
                (set
                 (make-local-variable 'darkroom--guess-margins-statistics-cache)
                 (let* ((line-widths
                         (save-excursion
                           (goto-char (point-min))
                           (cl-loop for start = (point)
                                    while (search-forward "\n"
                                                          (+ 20000 (point-min))
                                                          'no-error)
                                    for width = (truncate
                                                 (car
                                                  (window-text-pixel-size
                                                   window
                                                   start (1- (point))))
                                                 scaled-char-width)
                                    unless (zerop width)
                                    collect width)))
                        (n4 (max 1 (/ (length line-widths) 4))))
                   (/ (apply '+ (cl-subseq (sort line-widths '>) 0 n4)) n4))))))
      (cond
       ((> top-quartile-avg
           window-width)
        (message "Long lines detected. Consider turning on `visual-line-mode'")
        darkroom-margins-if-failed-guess)
       ((> top-quartile-avg (* 0.9 fill-column))
        ;; calculate margins so that `fill-column' + 1 colums are
        ;; centered on the window.
        ;; 
        (let ((margin (truncate (* (- window-width (1+ fill-column))
                                   (/ (float scaled-char-width)
                                      (frame-char-width)))
                                2)))
          (if darkroom-verbose
              (message "Choosing %s-wide margins based on fill-column %s"
                       margin fill-column))
          (cons margin margin)))
       (t
        darkroom-margins-if-failed-guess)))))

(defun darkroom--compute-margins (window)
  "From `darkroom-margins', computes desired margins for WINDOW."
  (let ((darkroom-margins
         (if (functionp darkroom-margins)
             (funcall darkroom-margins window)
           darkroom-margins)))
    (cond ((consp darkroom-margins)
           darkroom-margins)
          ((and (floatp darkroom-margins)
                (< darkroom-margins 1))
           (let ((delta (darkroom--float-to-columns darkroom-margins)))
             (cons delta delta)))
          (t
           (error "Illegal value in `darkroom-margins'")))))

(defun darkroom--float-to-columns (f)
  (ceiling (* (let ((edges (window-edges)))
                (- (nth 2 edges) (nth 0 edges)))
              f)))

(defvar darkroom--margin-factor 1
  "Buffer local factor affecting `darkroom--set-margins'")

(defun darkroom--set-margins ()
  "Set darkroom margins for currently selected window"
  (let* ((window-configuration-change-hook nil)
         (window (selected-window))
         (margins (darkroom--compute-margins window)))
    ;; See description of
    ;; `fringes-outside-margins' for the reason
    ;; for this apparent noop
    (set-window-buffer window (current-buffer))
    (set-window-margins window
                        (round
                         (* darkroom--margin-factor
                            (car margins)))
                        (round
                         (* darkroom--margin-factor
                            (cdr margins))))))

(defun darkroom--reset-margins ()
  "Reset darkroom margins for currently selected window."
  (set-window-margins (selected-window) 0 0))

(defun darkroom-increase-margins (increment)
  "Increase darkroom margins by INCREMENT."
  (interactive (list darkroom-margin-increment))
  (set (make-local-variable 'darkroom--margin-factor)
       (* darkroom--margin-factor (+ 1 increment)))
  (mapc #'(lambda (w)
            (with-selected-window w
              (darkroom--set-margins)))
        (get-buffer-window-list (current-buffer))))

(defun darkroom-decrease-margins (decrement)
  "Decrease darkroom margins by DECREMENT."
  (interactive (list darkroom-margin-increment))
  (darkroom-increase-margins (- decrement)))

(defvar darkroom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-+") 'darkroom-increase-margins)
    (define-key map (kbd "C-M--") 'darkroom-decrease-margins)
    map))

(defconst darkroom--saved-variables
  '(mode-line-format
    header-line-format
    fringes-outside-margins)
  "Variables saved in `darkroom--saved-state'")

(defvar darkroom--saved-state nil
  "Saved state before `darkroom-mode' is turned on.
Alist of (VARIABLE . BEFORE-VALUE)")

;; (defvar darkroom--saved-text-scale-mode-amount nil
;;   "Text scale before `darkroom-mode' is turned on.")

(defun darkroom--enter (&optional just-margins)
  "Save current state and enter darkroom for the current buffer.
With optional JUST-MARGINS, just set the margins."
  (unless just-margins
    (setq darkroom--saved-state
          (mapcar #'(lambda (sym)
                      (cons sym (buffer-local-value sym (current-buffer))))
                  darkroom--saved-variables))
    (setq mode-line-format nil
          header-line-format nil
          fringes-outside-margins darkroom-fringes-outside-margins)
    (text-scale-increase darkroom-text-scale-increase))
  (mapc #'(lambda (w)
            (with-selected-window w
              (darkroom--set-margins)))
        (get-buffer-window-list (current-buffer))))

(defun darkroom--leave ()
  "Undo the effects of `darkroom--enter'."
  (mapc #'(lambda (pair)
            (set (make-local-variable (car pair)) (cdr pair)))
        darkroom--saved-state)
  (setq darkroom--saved-state nil)
  (text-scale-mode -1)
  (mapc #'(lambda (w)
            (with-selected-window w
              (darkroom--reset-margins)))
        (get-buffer-window-list (current-buffer))))

(defun darkroom--enter-or-leave ()
  "Enter or leave darkroom according to window configuration."
  (cond ((= (count-windows) 1)
         (darkroom--enter darkroom--saved-state))
        (darkroom--saved-state
         (darkroom--leave))
        (t
         ;; for clarity, don't do anything
         )))

(declare-function darkroom-tentative-mode "darkroom" t)

;;;###autoload
(define-minor-mode darkroom-mode
  "Remove visual distractions and focus on writing. When this
mode is active, everything but the buffer's text is elided from
view. The buffer margins are set so that text is centered on
screen. Text size is increased (display engine allowing) by
`darkroom-text-scale-increase'." nil nil nil
  (when darkroom-tentative-mode
    (display-warning
     'darkroom
     (concat "Turning off `darkroom-tentative-mode' first. "
             "It doesn't go with `darkroom-mode'.")
     (let ((darkroom-mode nil))
       (darkroom-tentative-mode -1))))
  (cond (darkroom-mode
         (darkroom--enter)
         (add-hook 'window-configuration-change-hook 'darkroom--set-margins
                   t t))
        (t
         (darkroom--leave)
         (remove-hook 'window-configuration-change-hook 'darkroom--set-margins
                      t))))

;;;###autoload
(define-minor-mode darkroom-tentative-mode
  "Enters `darkroom-mode' when all other windows are deleted."
  nil " Room" darkroom-mode-map
  ;; always begin by removing the hook
  ;; 
  (remove-hook 'window-configuration-change-hook
               'darkroom--enter-or-leave 'local)
  (when darkroom-mode
    (display-warning
     'darkroom
     (concat "Turning off `darkroom-mode' first. "
             "It doesn't go with `darkroom-tentative-mode'.")
     (let ((darkroom-tentative-mode nil))
       (darkroom-mode -1))))
  ;; turn darkroom on or off according to window state
  ;; 
  (cond (darkroom-tentative-mode
         ;; re-add the hook when we are turning ourselves on
         ;;
         (add-hook 'window-configuration-change-hook
                   'darkroom--enter-or-leave 'append 'local)
         ;; call this right away if we're supposed to turn darkroom on
         ;; immediately.
         ;; 
         (darkroom--enter-or-leave))
        (t
         (darkroom--leave))))


(provide 'darkroom)
;;; darkroom.el ends here
