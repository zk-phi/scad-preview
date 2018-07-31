;;; linal-util.el --- Linear algebra functions for scad-preview mode
;;; Commentary:

;; Copyright (C) 2013-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: hooger

;;; Code:

(defun vector_norm (vec)
  "Calculates Euclidian-norm of an arbitrary length vector VEC."
  (sqrt (apply '+ (mapcar (lambda (x) (* x x)) vec)))
  )
(defun vector_normal (vec)
  "Returnsed a normal vector, pointing to the same direction as VEC."
  (let (
	(vec_n (float (vector_norm vec)))
	)
    (mapcar (lambda (x) (/ x vec_n)) vec)
    )
  )
(defun rotation (ang vec &optional deg)
  "Rotation matrix definition with Rodrigues formula.
\(Murray et.  al,  A Mathematical Introduction to Robotic Manipulation pp.  29\)
Rotation vector with ANG around VEC.  ANG is in degree if DEG is non-nil."
  (when (= (length vec) 3)
    (let (
	  (ang
	   (if deg
	       (* pi (/ ang (float 180)))
	     ang
	     ))
	  )
      (let (
	    (o (vector_normal vec))
	    (vt (- 1 (cos ang)))
	    (st (sin ang))
	    (ct (cos ang))
)
      `(,(+ (* vt (* (nth 0 o) (nth 0 o))) (* 1 ct))
,(- (* vt (* (nth 0 o) (nth 1 o))) (* (nth 2 o) st))
,(+ (* vt (* (nth 0 o) (nth 2 o))) (* (nth 1 o) st))

,(+ (* vt (* (nth 1 o) (nth 0 o))) (* (nth 2 o) st))
,(+ (* vt (* (nth 1 o) (nth 1 o))) (* 1 ct))
,(- (* vt (* (nth 1 o) (nth 2 o))) (* (nth 0 o) st))

,(- (* vt (* (nth 2 o) (nth 0 o))) (* (nth 1 o) st))
,(+ (* vt (* (nth 2 o) (nth 1 o))) (* (nth 0 o) st))
,(+ (* vt (* (nth 2 o) (nth 2 o))) (* 1 ct))
)))))
(defun matrixmul3x3 (a b)
  "Multiplying two 3x3 matrices.
The two matrices are A and B"
  (when (and (= (length a) 9) (= (length b) 9) )
    `(
      ,(+ (* (nth 0 a) (nth 0 b)) (* (nth 1 a) (nth 3 b)) (* (nth 2 a) (nth 6 b)))
      ,(+ (* (nth 0 a) (nth 1 b)) (* (nth 1 a) (nth 4 b)) (* (nth 2 a) (nth 7 b)))
      ,(+ (* (nth 0 a) (nth 2 b)) (* (nth 1 a) (nth 5 b)) (* (nth 2 a) (nth 8 b)))
      ,(+ (* (nth 3 a) (nth 0 b)) (* (nth 4 a) (nth 3 b)) (* (nth 5 a) (nth 6 b)))
      ,(+ (* (nth 3 a) (nth 1 b)) (* (nth 4 a) (nth 4 b)) (* (nth 5 a) (nth 7 b)))
      ,(+ (* (nth 3 a) (nth 2 b)) (* (nth 4 a) (nth 5 b)) (* (nth 5 a) (nth 8 b)))
      ,(+ (* (nth 6 a) (nth 0 b)) (* (nth 7 a) (nth 3 b)) (* (nth 8 a) (nth 6 b)))
      ,(+ (* (nth 6 a) (nth 1 b)) (* (nth 7 a) (nth 4 b)) (* (nth 8 a) (nth 7 b)))
      ,(+ (* (nth 6 a) (nth 2 b)) (* (nth 7 a) (nth 5 b)) (* (nth 8 a) (nth 8 b)))
      )
    )
  )
(defun matrixvectormul3x1 (mx v)
  "Multiplying 3x3 matrix with 3x1 vector.
MX is the matrix, V is the vector"
  (when (and (= (length mx) 9) (= (length v) 3) )
    `(
      ,(+ (* (nth 0 mx) (nth 0 v)) (* (nth 1 mx) (nth 1 v)) (* (nth 2 mx) (nth 2 v)))
      ,(+ (* (nth 3 mx) (nth 0 v)) (* (nth 4 mx) (nth 1 v)) (* (nth 5 mx) (nth 2 v)))
      ,(+ (* (nth 6 mx) (nth 0 v)) (* (nth 7 mx) (nth 1 v)) (* (nth 8 mx) (nth 2 v)))
      )
    )
  )
(defun rot2euler (r &optional deg)
  "Calculate Euler angles from a rotation matrix by Gregory G. Slabaugh.
Rotation order is X, Y, Z
R is the rotation matrix
if non-nil DEG is result is converted to degree"
  (when (= (length r) 9)
    (let
	(
	 (x1 0)
	 (x2 0)
	 (y1 0)
	 (y2 0)
	 (z1 0)
	 (z2 0)
	 )
      (if (= (abs (nth 6 r)) 1)
	  (progn
	    (setq z1 0)
	    (setq z2 0)
	    (if (= (nth 6 r) -1)
		(progn
		  (setq y1 (/ pi 2.0))
		  (setq y2 (/ pi 2.0))
		  (setq x1 (+ y1 (atan (nth 1 r) (nth 2 r))))
		  (setq x2 (+ y2 (atan (nth 1 r) (nth 2 r))))
		  )
	      (progn
		(setq y1 (/ pi -2.0))
		(setq y2 (/ pi -2.0))
		(setq x1 (- (atan (- (nth 1 r)) (- (nth 2 r))) y1))
		(setq x2 (- (atan (- (nth 1 r)) (- (nth 2 r))) y2))
		)
	      )
	    )
	(progn
	  (setq y1 (- (asin (nth 6 r))))
	  (setq y2 (- pi y1))
	  (setq x1 (atan (/ (nth 7 r) (cos y1)) (/ (nth 8 r) (cos y1))))
	  (setq x2 (atan (/ (nth 7 r) (cos y2)) (/ (nth 8 r) (cos y2))))
	  (setq z1 (atan (/ (nth 3 r) (cos y1)) (/ (nth 0 r) (cos y1))))
	  (setq z2 (atan (/ (nth 3 r) (cos y2)) (/ (nth 0 r) (cos y2))))
	  )
	)
      (if deg
	  ((lambda (ls) (list (butlast ls 3) (nthcdr 3 ls))) (mapcar (lambda (ang) (* 180 (/ ang (float pi)))) (list x1 y1 z1 x2 y2 z2)))
	`(,(list x1 y1 z1) ,(list x2 y2 z2))
	)
      )
    )
  )
(defun euler2rot (eulerls &optional deg)
  "Calculate rotation matrix from Euler angles.
EULERLS is the list of Euler angles,
if non-nil DEG is result is converted to degree"
  (matrixmul3x3 (matrixmul3x3 (rotation (nth 2 eulerls) '(0 0 1) deg) (rotation (nth 1 eulerls) '(0 1 0) deg)) (rotation (nth 0 eulerls) '(1 0 0) deg))
  )

(provide 'linal-util)
;;; linal-util ends here

