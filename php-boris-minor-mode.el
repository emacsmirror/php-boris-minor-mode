;;; php-boris-minor-mode.el --- a minor mode to evaluate PHP code in the Boris PHP REPL"

;; Copyright (C) 2013 steckerhalter

;; Author: steckerhalter
;; Package-Requires: ((php-boris "0.0.1"))
;; Keywords: php repl eval

;;; Commentary:

;; Adds a few keyboard shortcuts to `php-mode' (e.g. C-c C-c) to send
;; code from a PHP buffer to the Boris PHP REPL and evaluate it there.

;;; Code:

(require 'php-boris)

(defun php-boris-eval-region (start end)
  "Evaluate the region.
The two arguments START and END are character positions;
they can be in either order."
  (interactive "r")
  (php-boris-interactive-eval (buffer-substring-no-properties start end)))

(defun php-boris-eval-buffer ()
  "Evaluate the current buffer."
  (interactive)
  (php-boris-eval-region (point-min) (point-max)))

(defun php-boris-eval-defun ()
  "Evaluate the current function."
  (interactive)
  (let ((start (save-excursion (php-beginning-of-defun) (point)))
        (end (save-excursion (php-end-of-defun) (point))))
    (php-boris-eval-region start end)))

(defun php-boris-expression-at-point ()
  "Return the text of the expr at point."
  (apply #'buffer-substring-no-properties
         (php-boris-region-for-expression-at-point)))

(defun php-boris-region-for-expression-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (c-end-of-statement)
      (let ((end (point)))
        (c-beginning-of-statement-1)
        (list (point) end)))))

(defun php-boris-eval-dwim ()
  "Print and evaluate the current statement in Boris PHP REPL.
With active region print and evaluate the text in the region."
  (interactive)
  (let ((form (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (php-boris-expression-at-point))))
    (php-boris-interactive-eval form)))

(defun php-boris-interactive-eval (form)
  "Evaluate the given FORM and print value in minibuffer."
  (let ((buffer (current-buffer))
        (repl (get-process php-boris-process-name)))
    (if repl
        (display-buffer (process-buffer repl))
      (php-boris)
      (setq repl (current-buffer))
      (sit-for 0.1 t)
      (pop-to-buffer buffer))
    (comint-send-string repl (php-boris-clean-php-code form))
    (comint-send-string repl "\n")))

(defun php-boris-clean-php-code (code)
  (if (string-prefix-p "<?php" code t)
      (substring code 5)
    code))

;;;###autoload
(define-minor-mode php-boris-minor-mode
  "PHP boris minor mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state.

     When the minor mode is enabled, it adds several commands to
     interact with the Boris PHP REPL."
  :group 'php-boris
  :lighter " brs"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'php-boris-eval-dwim)
            (define-key map (kbd "C-c C-k") 'php-boris-eval-buffer)
            (define-key map (kbd "C-c C-r") 'php-boris-eval-region)
            (define-key map (kbd "C-M-x") 'php-boris-eval-defun)
            map)
  )

;;;###autoload
(add-hook 'php-mode-hook 'php-boris-minor-mode)

(provide 'php-boris-minor-mode)
;;; php-boris-minor-mode.el ends here
