    ;; Pegexel is a little exercises generator, using common-lisp.
    ;; Copyright (C) 2012 Yves Combe <yves@ycombe.net>

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU Affero General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU Affero General Public License for more details.

    ;; You should have received a copy of the GNU Affero General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


; get symbol in cl-user package
(defun cl-u-sym (symbol)
  (let ((sym (find-symbol (symbol-name symbol) :cl-user)))
    sym))

;
; Keep generated values in some case
;
(defvar *generated-values* () "store the values generated")
(pushnew '(*generated-values* ()) *values-to-init*)

; Why macro? I don't want val be evaluated if not used
(defmacro real-store-result (name val &key (generate-function nil))
     `(if (member ,name  *generated-values*)
	  (getf *generated-values* ,name)
	  (setf (getf *generated-values* ,name) 
		(if ,generate-function (funcall ,generate-function ,val)
		    ,val))))

; macro cannot be called by funcall. This function can.
(defun store-result (&rest listval)
  (let ((generate-function (second (member :generate-function listval))))
    (if generate-function (real-store-result (first listval) (second listval) :generate-function generate-function)
	(real-store-result (first listval) (second listval)))))

(defun get-result (&rest listval)
  (apply #'store-result (append (list (first listval) nil) (rest listval))))

(defun multiple-store-result (symbols values &optional &key (generate-function nil gf-p))
  (let ((gen-values (if generate-function (funcall generate-function values)
			values)))
    (loop
       for sym in symbols
       for val in gen-values
       do  (store-result sym val :generate-function generate-function))
    (if generate-function (funcall generate-function (cl-u-sym 'nothing))
	(cl-u-sym 'nothing))))
			    

;
; get random value from list, storing choice to reuse it in linked choices
;
; Note: the intern function find a symbol from name or create one.
(defun random-link (&rest listval)
  (let* ((name (first listval))
	 (store-symbol (intern (concatenate 'string "link-" (symbol-name name))))
	 (generate-function (second (member :generate-function listval)))
	 (values (cl-user::delete-param-pair :generate-function (rest listval)))
	 (result (elt values (store-result store-symbol (random (length values))))))
    (cond (generate-function (funcall generate-function result))
	  (t result))))

(defun result-of-function (sexpr &key (generate-function nil))
  (if generate-function (funcall generate-function (eval sexpr))
      (eval sexpr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

