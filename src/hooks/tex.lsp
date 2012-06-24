
    ;; Pegexel is a little exercises generator, using common-lisp.
    ;; Copyright (C) 2012 Yves Combe <yves@ycombe.net>

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

			    
(defun tex (&rest listval)
  (let* ((generate-function (second (member :generate-function listval)))
	 (values (delete-param-pair :generate-function listval))
	 (result nil))
    (cl-user::load-grammar-file-and-eval-code "tex")    
    (setf result (append (list (cl-u-sym 'begintex)) (list values) (list (cl-u-sym 'endtex))))
    (if generate-function (funcall generate-function result)
	result)))


(defun cl-u-sym (symbol)
  (let ((sym (find-symbol (symbol-name symbol) :cl-user)))
    sym))
