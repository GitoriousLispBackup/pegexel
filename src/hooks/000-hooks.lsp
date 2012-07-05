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


;; initialisation for hooks
;; hook cah simply add a list  (var value) here to reinit it each time
(defvar *values-to-init* nil "List of couples (var value) to init at generation start")
(defun init-hooks ()
  (loop for (var value) in *values-to-init*
       do (setf (symbol-value var) value)))
(export 'init-hooks)

;; Keep generated values in some case

(defvar *generated-values* () "store the values generated")
(pushnew '(*generated-values* ()) *values-to-init*)

(defmacro record-value (name  &optional (val nil))
  `(if (member ,name  *generated-values*)
      (getf *generated-values* ,name)
	  (setf (getf *generated-values* ,name) 
		,val)))

;; (defun record-value (name  &optional (val nil))
;;   (if (member name  *generated-values*)
;;       (getf *generated-values* name)
;; 	  (setf (getf *generated-values* name) 
;; 		val)))

(defun ?-! (name  &optional (val nil))
  (record-value name (when val (funcall *generate* val))))

;; get random value from list, storing choice to reuse it in linked choices

;; Note: the intern function find a symbol from name or create one.
(defun ?-% (name &rest values)
  (let* ((store-symbol (intern (concatenate 'string "link-" (symbol-name name)))))
    (funcall *generate* (elt values (record-value store-symbol (random (length values)))))))


;; Evaluation of sexpr has to be passed through generate after

;; evaluate sexpr
(defun ?-e (sexpr)
  (funcall *generate* (eval sexpr)))

;; evaluate and store sexpr
(defun ?-!e (name &optional (sexpr nil))
  (funcall *generate* (record-value name  (eval sexpr))))


