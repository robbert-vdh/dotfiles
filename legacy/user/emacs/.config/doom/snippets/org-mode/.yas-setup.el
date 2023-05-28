(defun latex-create-matrix ()
  "Creates an NxM matrix for quick insertion in org-mode."
  (let* ((input (mapcar
                 'string-to-number
                 (split-string (read-string "Matrix size (NxM): " nil) "x")))
         (rows (car input))
         (columns (cadr input)))

    (if (or (zerop rows) (zerop columns))
        "Invalid input"
      (string-join
       (cl-loop for row to (1- rows) collect
                (concat
                 "  "
                 (string-join
                  (cl-loop for column to (1- columns) collect "X")
                  " & ")))
       " \\\\\n"))))
