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

(defun tex (&rest listval)
  (let* ((generate-function (second (member :generate-function listval)))
	 (values (delete-param-pair :generate-function listval))
	 (result nil))
    (cl-user::load-grammar-file-and-eval-code "tex")    
    (setf result (append (list (cl-u-sym 'begintex) (cl-u-sym 'upcase)) (list values) (list (cl-u-sym 'endtex))))
    (if generate-function (funcall generate-function result)
	result)))

;
; (tex-table "|l|ccc" 'hrule '(Title first second third) 'hrule
;
(defun get-first-list (listval)
  (cond ((null listval) nil)
	((consp (first listval)) (first listval))
	(t (get-first-list (rest listval)))))

(defun col-string (length)
  (coerce (loop repeat length collect #\c) 'string))

(defun cols-table (listval)
  (list
   (concatenate 'string
		"{"
		(or (first listval)
		    (col-string (length (get-first-list listval))))
		"}")
   (cl-u-sym 'NEWLINE)))

(defun get-line (line)
  (cond ((listp line)
	 (loop for (a b) on line collect a when b collect '&))
	(t (list line))))

(defun get-content-table (content)
  (apply #'append 
	 (loop for (a b) on content
	    collect (get-line a)
	    when (and b (not (equal a (cl-u-sym 'hline))))
	    collect (list (cl-u-sym 'tabnewline)))))

(defun tex-table (&rest listval)
   (let* ((generate-function (second (member :generate-function listval)))
	  (values (delete-param-pair :generate-function listval))
	  (begintable (list (cl-u-sym 'begintabletex)))
	  (endtable (list (cl-u-sym 'endtabletex)))
	  (colstring (cols-table listval))
	  (result (append begintable colstring (get-content-table (rest values)) endtable)))
     (if generate-function 
	 (funcall generate-function result)
	result)))

