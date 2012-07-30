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
  "Initialisation functions for hooks."
  (loop for (var value) in *values-to-init*
       do (setf (symbol-value var) value)))
(export 'init-hooks)

;; Keep generated values in some case

(defvar *generated-values* () "store the values generated")
(pushnew '(*generated-values* ()) *values-to-init*)

(defmacro record-value (name  &optional (val nil))
  "Record name and value in a plist if they not exist in. Send value if they exist."
  `(if (member ,name  *generated-values*)
      (getf *generated-values* ,name)
      (setf (getf *generated-values* ,name) 
	    ,val)))

(defun §-! (name  &optional (val nil))
  "Record value (hook).$"
  (record-value name (when val (funcall *generate* val))))

;; get random value from list, storing choice to reuse it in linked choices
;; Note: the intern function find a symbol from name or create one.
(defun §-% (name &rest values)
  "Get random value storing position, or using stored position."
  (let* ((store-symbol (intern (concatenate 'string "link-" (symbol-name name)))))
    (funcall *generate* (elt values (record-value store-symbol (random (length values)))))))

;; force value to link 
(defun §-%< (name val)
  "Force position for §-% hook."
  (let ((value (eval val))
	(store-symbol (intern (concatenate 'string "link-" (symbol-name name)))))
    (when value   (record-value store-symbol value)))
  (funcall *generate* (template 'nothing)))

;; Evaluation of sexpr has to be passed through generate after

;; evaluate sexpr
;; MAYBE obsolete with variables?
(defun §-e (sexpr)
  "Evaluate (hook)."
  (funcall *generate* (eval sexpr)))

;; evaluate and store sexpr
;; MAYBE obsolete with variables?
(defun §-!e (name &optional (sexpr nil))
  "Evaluate and store (hook)."
  (funcall *generate* (record-value name  (eval sexpr))))

(defun evaluable-p (expr)
  (or (not (consp expr))
      (fboundp (first expr))))

(defun §-* (times &rest listval)
  (apply #'append 
	 (cond ((evaluable-p times)
		(loop repeat (eval times)
		   collect (funcall *generate* listval)))
;; TODO add an error if (first times) is in variables !!
	       (t (eval `(defvar  ,(template (first times)) nil))  ; I need dynamic scoping
		  (pushnew (template (first times)) *variables*)
		  (eval `(loop for ,(template (first times)) from 0 upto ,(1- (eval (second times)))
			    collect (funcall *generate* ',listval)))))))

(defun reinit-generated-value (key)
  "Suppress stored value in plist."
  (remf *generated-values* key))

(defun reinit-all-records ()
  "Suppress all stored value in plist."
  (setf  *generated-values* nil))

; default is reinit generated
(defun §-reinit (&rest keys-to-suppress)
  "Reinit hook: suppress rtoredplist values and reset variables."
  (if keys-to-suppress 
      (loop for k in keys-to-suppress
	 do (let ((link-name  (intern (concatenate 'string "link-" (symbol-name k)))))
	      (cond ((eq-template k 'variables) (reinit-all-variables))
		    ((eq-template k 'records) (reinit-all-records))
		    ((member k *generated-values*) (reinit-generated-value k))
		    ((member link-name *generated-values*) (reinit-generated-value link-name))
		    (t (reinit-variable k)))))
      (reinit-all-records))
  (funcall *generate* (template 'nothing)))

;; in 000-hooks.lsp
(defun §-> (name)
  "Next step walk-through variable (hook)."
  (real-next-walk name))

