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

(defun set-variable (sym val)
  (when (variable-p sym) (error "Variable ~A already defined in ~A" (symbol-value sym) *variables* ))
  (eval `(defvar  ,sym)) ; to suppress warnings udefined variable 
  (pushnew sym *variables*)
  (setf (symbol-value sym) val))

(defun set-multiples-variables (listsym listval)
  (loop for sym in listsym
       for val in  listval
       do (set-variable sym val)))

(defun eval-variables ()
  (loop for (var nil val) in *exo-variables*
     when (symbolp var) do (set-variable var (eval val))
     when (consp var) do (set-multiples-variables var (eval val))))