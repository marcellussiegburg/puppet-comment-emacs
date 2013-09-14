;;; puppet-comment.el --- Write class and define comments in puppet faster

;; Copyright (C) 2013  Marcellus Siegburg

;; Author: Marcellus Siegburg <marcellus@Manzana.local>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; It activates skeleton mode, hopefully you have it installed.

;;; Code:

(defun take (n xs)
  "Takes n values of list xs"
  (if (< n 1) ()
    (cons (car xs) (take (- n 1) (cdr xs)))))

(defun truncate-string-to-fit (lines len start-chars)
  "Truncates the lines so that words in aline are not superseeding
width. It begins lines with start char"
  (defun find-len (a b max-len)
    "Tells how many are enough"
    (if (null a) 0
      (if (> (+ (length (car a)) b) max-len) 0
	(+ 1 (find-len (cdr a) (+ 1 b (length (car a))) max-len)))))
  (if (null lines) (concat start-chars lines '("\n"))
    (if (<= (+ (length lines) (length start-chars)) len) (concat start-chars lines "\n")
      (let* ((splitted (split-string lines " " t))
	     (num (find-len splitted (length start-chars) len)))
	(if (= 0 num)
	    (if (null splitted) (concat start-chars lines "\n")
	      (concat start-chars (car splitted) "\n" (truncate-string-to-fit (mapconcat 'identity (cdr splitted) " ") len start-chars)))
	  (concat start-chars (mapconcat 'identity (take num splitted) " ") "\n" (truncate-string-to-fit (mapconcat 'identity (nthcdr num splitted) " ") len start-chars)))))))

(define-skeleton puppet-variable-comment
  "Promts you continuing for variables and their descriptions and inserts them."
  nil
  ("Variable name: "
   "# [*" str "*]\n"
   "#\n"
   (truncate-string-to-fit (skeleton-read (concat "Description of " str ": ")) 80 "#   ")
   "#\n"))

(provide 'puppet-comment)
;;; puppet-comment.el ends here
