;;; ~/.config/doom/autoload/advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +robbert--company-box-fix-tng (&rest args)
  "Hides the `company-box' line if nothing is selected.
 `company-tng' uses `company-selection-changed' to emulate an
'unselected' state. Also makes sure the scroll bar is not too wide."
  (when-let* ((window (and company-candidates
                           (not company-selection-changed)
                           (get-buffer-window (company-box--get-buffer) t))))
    (with-selected-window window
      (move-overlay (company-box--get-ov) -1 -1)))

  ;; TODO: Fix scroll bar sizing
  )

;;;###autoload
(defun +robbert--indent-paste-advise (original-function &rest args)
  "Automatically indent pasted code. See
`+robbert/indentation-sensitive-modes'."
  (if (or (member major-mode +robbert/indentation-sensitive-modes))
      (apply original-function (cdr args))
    (let ((inhibit-message t)
          (transient-mark-mode nil))
      (evil-start-undo-step)
      ;; This would otherwise cause double highlighting
      (let ((evil-goggles-mode nil))
        (apply original-function args))
      ;; Only indent whent the region is not too large
      (when (<= (- (region-end) (region-beginning)) +robbert/indentation-max-length)
        (indent-region (region-beginning) (region-end) nil))
      ;; HACK: For some reason the `(evil-end-undo-step)' moves the point one
      ;;       unit to the left when in insert mode
      (when (evil-insert-state-p) (forward-char))
      (evil-end-undo-step))))
