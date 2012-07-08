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

(defvar *variables* ()  "List of variables.")

(defun variable-p (sym)
  (if (member sym *variables*) t nil))

(defun set-variable (sym val &key (var-index nil))
  (eval `(defvar  ,sym)) ; to suppress warnings udefined variable 
  (pushnew sym *variables*)
  (setf (symbol-value sym) (eval val))
  (when var-index (setf (get sym 'var-index) var-index)))

(defun set-multiples-variables (listsym listval &key (var-index nil))
  (loop for sym in listsym
       for val in  (eval listval)
       do (set-variable sym val :var-index var-index)))

(defun get-walk-through-value (name)
  (elt (get name 'values) (get name 'index)))
(export 'get-walk-through-value)

(defun set-walk-through-value (name values &key (var-index nil))
  (pushnew name *variables*)
  (setf (get name 'values) (if (and (symbolp (first values))
				    (fboundp (first values)))
			       (eval values)
			       values))
  (setf (get name 'index) -1)
  (when var-index (setf (get name 'var-index) var-index))
  (setf (symbol-value name) 'value-not-set-before-first-call-to-next-walk-!))

;; in 000-hooks.lsp
(defun real-next-walk (name)
  (when (< (get name 'index) (1- (length-walk name)))  
    (incf (get name 'index))
    (setf (symbol-value name) (get-walk-through-value name)))
  (funcall *generate* (template 'nothing)))
(export 'real-next-walk)

(defun eval-a-variable (var val &key (var-index nil))
  (cond ((symbolp var) (set-variable var val :var-index var-index))
	((eq-template (first var) '§-walk-through) (set-walk-through-value (second var) val :var-index var-index))
	(t (set-multiples-variables var val :var-index var-index))))
  
(defun eval-variables ()
  (loop for (var nil val) in *exo-variables*
       for index = 0 then (1+ index)
       do (eval `(eval-a-variable ',var ',val :var-index ,index))))

(defun length-walk (name)
  (length (get name 'values)))
(export 'length-walk)

(defun reinit-variable (key)
  (when (member key *variables*)
    (let* ((vardef (elt *exo-variables* (get key 'var-index)))
	   (var (first vardef))
	   (val (third vardef)))
      (eval-a-variable var val))))
(export 'reinit-variable)

(defun reinit-all-variables ()
  (loop for k in *variables*
       do (reinit-variable k)))
(export 'reinit-all-variables)
