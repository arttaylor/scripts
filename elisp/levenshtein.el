;;; levenshtein.el --- Calculate Levenshtein distance
;;
;; Copyright (c) 2003 Art Taylor 
;;
;; Filename: levenshtein.el
;; Author: Art Taylor <reeses@astrogoth.com>
;; Version: 1.0
;; Keywords: Levenshtein, string edit distance
;;
;; [Commentary]
;;
;; Calculate Levenshtein string edit distance between two strings.
;;
;; This is a string edit distance calculation returning the minimum
;; number of characters inserted, deleted, or replaced to transform
;; one string into another, and is performed by the following steps:
;; 
;; Given two strings, a and b:
;; if a == b, distance = 0
;;
;; m = length a, n = length b
;; if m == 0, distance = n
;; if n == 0, distance = m
;;
;; Construct a matrix w, containing m+1 x n+1 elements
;; Initialise first row to 0..m
;; Initialise first column to 0..n
;; 
;; For each letter r in m
;;   For each letter s in n
;;     if r == s, cost = 0
;;     else cost = 1
;;     
;;     w[r,s] = the minimum of * 1 + the cell above
;;                             * 1 + the cell to the left
;;                             * cost + the cell above and to the left
;;
;; Levenshtein distance = value of w[m,n]
;;
;; For a clearer explanation with better examples, use Google or <IYFISE>
;;
;; [License]
;;
;; This software is provided 'as-is', without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:
;;
;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software. If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.
;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.
;; 3. This notice may not be removed or altered from any source
;;    distribution.
;;
;; Note that this license is borrowed from zlib via nullsoft.
;;
;; Written 15-Mar-2003, Washington, DC
;;

(require 'cl)

(defun levenshtein-initialise-matrix (matrix)
  "Initialise a Levenshtein matrix, setting indexes in first row & column."
  (loop for i from 0 below (length matrix) do
    (setf (car (nth i matrix)) i))
  (let ((row (car matrix)))
    (loop for i from 0 below (length row) do
      (setf (nth i row) i)))
  matrix)

(defun levenshtein-make-matrix (m n)
  "Create an initialised Levenshtein matrix of size m x n."
  (setq matrix (make-list m nil))
  (loop for i from 0 below m do
    (setf (nth i matrix) (make-list n 0)))
  (levenshtein-initialise-matrix matrix))

(defun levenshtein-get-cell (matrix x y)
  "Retrieve the value from cell [x,y] in matrix."
  (nth y (nth x matrix)))

(defun levenshtein-distance (one-string another-string)
  "Calculate the levenshtein string edit distance."
  (let ((m (length one-string))
	(n (length another-string)))
    (cond ((zerop m) n)
	  ((zerop n) m)
	  (t (let ((matrix (levenshtein-make-matrix (1+ m) (1+ n))))
	       (loop for i-m from 1 to m do
		 (loop for i-n from 1 to n do
		   (setq cost (if (equal (aref one-string (1- i-m)) 
					 (aref another-string (1- i-n)))
				  0 1))
		   (setf (nth i-n (nth i-m matrix))
			 (min (1+ (levenshtein-get-cell matrix 
							(1- i-m) 
							i-n))
			      (1+ (levenshtein-get-cell matrix 
							i-m 
							(1- i-n)))
			      (+ (levenshtein-get-cell matrix 
						       (1- i-m) 
						       (1- i-n))
				 cost)))))
	       (levenshtein-get-cell matrix m n))))))

(provide 'levenshtein)

;;; levenshtein.el ends here
