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

;
;
;
(defun add-grammar (grammar)
;  (in-package cl-user)
  (setf *grammar* (append *grammar* (symbol-value grammar))))
(export  'add-grammar)


(defun debug-symbol (value)
  (script-debug "~A -> ~A~%"  (symbol-name value) (symbol-value value)))
(export 'debug-symbol)

(defun template (sym)
  (intern (symbol-name sym) :template))
(export  'template)

;; (defun script (sym)
;;   (intern (symbol-name sym) :script))
;; (export  'script)



(defun eq-template (cible sym)
  (equal cible (template sym)))
(export  'eq-template)
