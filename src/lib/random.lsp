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
;	 (setf *random-state* (make-random-state t))
	 (let ((pos (if normal (random-normal p :mu mu :sigma sigma)
			(random p))))
	   (incf (nth pos rlist) (/ 1 unit-th))))
    (if float (mapcar #'float rlist)
	rlist)))
(export 'partition)
