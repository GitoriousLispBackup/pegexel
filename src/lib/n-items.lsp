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
(defun init-items ()
  "Initialise variable ?n-items."
  (unless *items* (return-from init-items))
  (let ((value (read-from-string *items*))
	(items-var (template '?n-items)))
    (unless (numberp value) (error "Can't read ~A as number !" *items*))
    (eval `(defvar ,items-var ,value))
    (pushnew items-var *variables*)
    (setf (get items-var 'no-reinit) t)))
