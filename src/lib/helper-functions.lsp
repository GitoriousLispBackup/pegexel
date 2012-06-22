;
;
;
(defun add-grammar (grammar)
;  (in-package cl-user)
  (setf *grammar* (append *grammar* (symbol-value grammar))))
(export  'add-grammar)


(defun debug-symbol (value)
  (when *debug* (format t "~A -> ~A~%"  (symbol-name value) (symbol-value value))))