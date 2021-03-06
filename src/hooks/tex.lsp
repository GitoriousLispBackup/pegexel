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

(defun §-tex (&rest values)
  "Initialize tex output (hook)."
  (let* ((result nil))
    (load-grammar-file-and-eval-code "tex")    
    (setf result (append (list (template 'begintex)) 
			 (list (template 'upcase)) 
			 (list values) 
			 (list (template 'endtex))))
    (setf *output-type* "tex")
    (funcall *generate* result)))

;
; (§-tex-table "|l|ccc" 'hline '(Title first second third) 'hline
;
(defun get-first-list (listval)
  "Get the first list in listval elements."
  (cond ((null listval) nil)
	((consp (first listval)) (first listval))
	(t (get-first-list (rest listval)))))

(defun col-string (length)
  "Get default string for columns in tabular environment."
  (coerce (loop repeat length collect #\c) 'string))

(defun cols-table (listval)
  "Get string for columns in tabular environment."
  (list
   (concatenate 'string
		"{"
		(or (first listval)
		    (col-string (length (get-first-list listval))))
		"}")))

(defun get-line (line)
  "Get tabular line with '&' (or hline...)."
  (cond ((listp line)
	 (loop for (a b) on line collect a when b collect (template '&)))
	(t (list line))))

(defun get-content-table (content)
   "Get tabular content from lists."
  (apply #'append 
	 (loop for (a b) on content
	    collect (get-line a)
	    when (and b (not (eq-template a 'hline)))
	    collect (list (template 'tabnewline)))))

(defun §-tex-table (&rest values)
  "Output tex tabular from lists (hook)."
   (let* ((begintable (list (template 'begintabletex)))
	  (endtable (list (template 'endtabletex)))
	  (colstring (cols-table values))
	  (result (append begintable 
			  colstring 
			  (list (template 'newline)) 
			  (get-content-table 
			   (rest values)) 
			  endtable)))
     (funcall *generate* result)))

(defun set-tex-env (name &key (end nil))
  "Output generic tex environment begin or end."
  (unless (stringp name) (error "TeX environment name must be a string !"))
  (list (if end (template 'end-tex-env) 
	    (template 'begin-tex-env))
	name 
	(template 'close-brace)))


(defun §-tex-env (name &rest listval)
  "Output generic tex environment (hook)."
  (funcall *generate* (append (set-tex-env name) listval (set-tex-env name :end t))))

(defun §-tex-si (number unit)
  (funcall *generate* (list (template 'tex-si) number (template 'nospace) "}{" (template 'nospace) unit (template 'nospace) "}")))