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

;; round-to : round to part of unit (deicimal, third, etc... 
;; idea from http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Common_Lisp
(defun round-to (number unit-nth &optional (what #'round))
  "Round number with 1/unit-nth precision."
  (/ (funcall what (* number unit-nth)) unit-nth))
(export 'round-to)

(defun p-list-of-k (p k)
  "List of p elements k."
  (loop repeat p collect k))

; Box-Muller method: http://fr.wikipedia.org/wiki/Loi_normale#Cas_de_la_loi_normale_.C3.A0_une_dimension
; result rounded to 1/unit-th
; 0 <= result < k 
(defun random-normal (k &optional &key (mu nil) (sigma nil) (round-unit-th 1))
  "random with normalized distribution (Box-Muller)."
  (let ((result k)
	(rmu (or mu (/ k 2)))
	(rsigma (or sigma (/ k 2))))
    (loop until (and (<= 0 result) (< result k))
       do (setf result (round-to (+ rmu (* rsigma (sqrt (* -2 (log (random 1.0)))) (cos (* 2 pi (random 1.0))))) round-unit-th)))
    result))
(export 'random-normal)

;
; random partition of n in p numbers. uniform or normal.
; Can be float or fract, specifying part of 1 to use as unit (unit-th 5 -> 0.2 as unit element)
;
(defun partition (n p &optional &key (mu nil) (sigma nil) (min 0) (normal nil) (unit-th 1) (float nil))
  "Random partition of n in p numbers."
  (when (> (* min p) n) (error "Partition of ~A with ~A numbers >= ~A !~%" n p min))
  (let ((rlist (p-list-of-k p min)))
    (loop repeat (* (- n (* min p)) unit-th)
       do 
	 (let ((pos (if normal (random-normal p :mu mu :sigma sigma)
			(random p))))
	   (incf (nth pos rlist) (/ 1 unit-th))))
    (if float (mapcar #'float rlist)
	rlist)))
(export 'partition)

;;
;; Cartesian product fo lists
;; This version is inefficient in GNU CLISP. Below better solution
;;
;; (defun product (&rest lists)
;;   "Cartesian product of lists filtered by :test test function.
;;    Make sure test function accept one argument per list."
;;   (let ((test (cadr (member :test lists))))
;;     (when test (setf lists (delete test lists))
;; 	  (setf lists (delete :test lists)))
;;     (product_aux lists nil :test test)))
;; (export 'product)

;; (defun product_aux (lists elts &key (test nil))
;;   "Auxiliary version recursive"
;;   (cond ((null lists) (let ((rev-elts (reverse elts)))
;; 		       (if test 
;; 			   (when (apply test rev-elts) rev-elts)
;; 			   rev-elts)))
;; 	((cdr lists) (apply #'append 
;; 			    (loop for i in (first lists) collect
;; 				 (product_aux (cdr lists) (cons i elts) :test test))))
;; 	(t (loop for i in (first lists) 
;; 	      for result =  (product_aux (cdr lists) (cons i elts) :test test)
;; 		when result collect result))))


;; This version comes from 
;; http://objectmix.com/lisp/337028-learning-lisp-cartesian-product-critique-4.html#post1223834
(defun cartesian* (&rest sets)
  "Cartesian product of lists"
  (reduce
   (lambda (xs ys)
     (mapcan
      (lambda (x)
	(mapcar
	 (lambda (y)
	   (cons x y))
	 ys))
      xs))
   sets
   :initial-value '(()) :from-end t))

;;
;; Filter results of cartesian product
;; 
(defun cartesian*-test (&rest lists)
  "Cartesian product, results filtered by :test test"
  (let ((test (cadr (member :test lists))))
    (when test (setf lists (delete test lists))
	  (setf lists (delete :test lists)))
    (if test (mapcan #'(lambda (x) (and (apply test x) (list x))) (apply #'cartesian* lists))
	(apply #'cartesian* lists))))
(export '(cartesian*  cartesian*-test))


;;
;; Efficient if n/N is small (< 1/3 or something like that).
;; Use (random-n-elt n (cartesian*-test list1 list2 ... :test test))
;; in other case.
(defun random-n-elt-from-* (n &rest lists)
  "Choose ramdomly n elts from cartesian product
   conforming test."
  (let ((test (cadr (member :test lists)))
	(result nil))
    (when test (setf lists (delete test lists))
	  (setf lists (delete :test lists)))
    (loop while (< (length result) n) 
       do
	 (let ((candidat (mapcar #'random-elt lists)))
	   (when (or (null test) (apply test candidat)) 
	     (pushnew candidat result :test #'equal))))
    result))
(export 'random-n-elt-from-*)

(defun random-n-elt (n list)
  "Return n random elts from list.
   ramdomized list if n >= (length list)."
  (if (< n 1) ()
      (random-n-elt-aux n (copy-list list) (length list) nil)))
(export 'random-n-elt)

(defun random-n-elt-aux (n list length results)
  (cond ((zerop (* n length)) results)
	(t (let* ((indice (random length))
		  (place (nthcdr indice list))
		  (result (first place)))
	     (setf (first place) (first list))
	     (random-n-elt-aux (1- n) (cdr list) (1- length) (cons result results))))))
;(export 'random-n-elt-aux)

(defun randomize (list)
  "return a list with same elements
    as list in random order"
  (random-n-elt (length list) list))
(export 'randomize)

(defun range (&optional &key (min 0) (max 10))
  (loop for i from min to max collect i))
(export 'range)
