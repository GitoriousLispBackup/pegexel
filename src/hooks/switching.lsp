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

;; if hooks
(defun §-Y (test if-non-nil if-nil)
  (funcall *generate* (if (eval test) if-non-nil if-nil)))

;; switch case number hooks (warning: index from 1 to n).
(defun §-F (index &rest cases)
  (let* ((index-1 (1- (eval index)))
	 (real (max 0 (min (1- (length cases)) index-1))))
    (funcall *generate* (elt cases real))))
