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

(defun variable-p (sym)
  "Predicat check if symbol is template variable."
  (if (member sym *variables*) t nil))

(defun set-variable (sym val)
  "Set variable."
  (setf (symbol-value sym) (eval val)))

(defun set-multiples-variables (listsym listval)
  "Set list of variables with list of values."
  (let ((values (eval-if-needed listval)))
    (loop for sym in listsym
       for val in values
       do (set-variable sym val))))

(defun get-walk-through-value (name)
  "Get actual value of variable of type walk-through"
  (elt (get name 'values) (get name 'index)))
(export 'get-walk-through-value)

(defun eval-if-needed (values)
  (cond ((and (symbolp values) (boundp values)) (eval values))
	((and (consp values)
	      (symbolp (first values))
	      (fboundp (first values)))
	 (eval values))
	(t values)))

(defun set-walk-through-value (name values)
  "Set value of variable of type walk-through. Keep list of value and index in symbol's plist."
;  (script-debug "set-walk-through-value ~A ~A~%" name (eval-if-needed values))
  (setf (get name 'values) (eval-if-needed values))
  (setf (get name 'index) -1)
  (setf (symbol-value name) 'value-not-set-before-first-call-to-next-step-§->!))

(defun next-walk-in-multiple (name)
  (let ((symbols (get name 'symbols))
	(values (get name 'values))
	(index (get name 'index)))
    (when (< index (1- (length-walk name)))
      (loop for sym in symbols
	 for val in (elt values (1+ index))
	 do 
	   (incf (get  sym 'index))
	   (setf (symbol-value sym) val)))))

;; in 000-hooks.lsp
(defun real-next-walk (name)
  "Set value of variable of type walk-through to the next step."
  (if (get name 'symbols)
      (next-walk-in-multiple name)
      (when (< (get name 'index) (1- (length-walk name)))  
	(incf (get name 'index))
	(setf (symbol-value name) (get-walk-through-value name))))
  (funcall *generate* (template 'nothing)))
(export 'real-next-walk)

(defun set-walk-through-value-in-multiple (name  symbols values)
;  (script-debug "set-walk-through-value ~A ~A ~A~%" name  symbols values)
  (setf (get name 'values) values)
  (setf (get name 'symbols) symbols)
  (setf (get name 'index) -1)
  (setf (symbol-value name) 'value-not-set-before-first-call-to-next-step-§->!))

(defun set-multiple-walk-through-value (listsym listval)
  (let ((values (eval-if-needed listval)))
    (loop 
       for sym in listsym
       do (set-walk-through-value-in-multiple sym listsym values))))

(defun eval-a-variable (var val)
  "Parse variable definition and set it (or them)." 
  (cond ((symbolp var) (set-variable var val))
	((eq-template (first var) '§-walk-through) 
	 (if (consp (second var))
	     (set-multiple-walk-through-value (second var) val)
	     (set-walk-through-value (second var) val)))
	(t (set-multiples-variables var val))))
  
(defun eval-variables ()
  "Parse variable list to set them."
  (loop for (var nil val) in *exo-variables*
       for index = 0 then (1+ index)
       do (eval `(eval-a-variable ',var ',val :var-index ,index)))
  (setf *variables* (reverse *variables*)))

(defun length-walk (name)
  "length of the list of values from a walk-through variable."
  (length (get name 'values)))
(export 'length-walk)

(defun check-reinit (key)
  (cond ((get key 'no-reinit) nil)
	((not (get key 'reinit)) t)
	((equal (get key 'reinit) key) t)
	(t nil)))

(defun reinit-variable (key)
  "Reinit variable as it was not used yet. Usefull for walk-through or random ones."
  (when (and (member key *variables*) (check-reinit key))
    (let* ((vardef (elt *exo-variables* (get key 'var-index)))
	   (var (first vardef))
	   (val (third vardef)))
      (script-debug "Initialising ~A with  ~A ~%" key vardef)
      (eval-a-variable var val))))
(export 'reinit-variable)

(defun reinit-all-variables ()
  "Reinit all variables from template."
  (loop for k in *variables*
       do (reinit-variable k)))
(export 'reinit-all-variables)

(defun define-variable (name &key (var-index nil) (reinit-symbol nil))
  "Define a variable."
  (when (and *items* (eq-template name '?n-items)) (return-from define-variable))
  (eval `(defvar ,name nil))
  (pushnew name *variables*)
  (when reinit-symbol (setf (get name 'reinit) reinit-symbol))
  (when var-index (setf (get name 'var-index) var-index)))
  
(defun define-list-of-variables (varlist  &key (var-index nil))
  "Define all variable from list."
  (let ((reinit (first varlist)))
    (loop for var in varlist 
       do (define-variable var 
	      :var-index var-index
	      :reinit-symbol reinit))))

(defun defvar-all ()
  "Define all variables from template. To suppress 'undefined variable' from templates code."
    (loop 
       for (vardef nil) in *exo-variables*
       for index = 0 then (1+ index) 
       do
	 (cond ((symbolp vardef) (define-variable vardef :var-index index))
	       ((and (consp vardef) (eq-template (first vardef) '§-walk-through))
		(if (consp (second vardef))
		    (define-list-of-variables (second vardef) :var-index index)
		    (define-variable (second vardef) :var-index index)))
	       ((consp vardef) (define-list-of-variables vardef :var-index index))
	       (t (error "defvar-all should not be here !"))))
    (setf *variables* (reverse *variables*)))
