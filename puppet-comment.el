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

;; This file provides a minor-mode for puppet comments. By default it will be
;; loaded with .pp$ files and provides key bindings for the provided functions.
;; It requires skeletons installed, hopefully you have it. If you have the
;; latest emacs version installed.

;;; Code:

;; Variables
; Key Map (Key bindings)
(defvar puppet-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c C-c") 'puppet-class-comment)
    (define-key map (kbd "C-c c C-d") 'puppet-define-comment)
    (define-key map (kbd "C-c c C-p") 'puppet-parameter-comment)
    (define-key map (kbd "C-c c C-v") 'puppet-variable-comment)
    (define-key map (kbd "C-c c C-a") 'puppet-author-comment)
    map)
  "Keymap used in puppet-comment-mode.")

;; Functions
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

;; Skeletons
(define-skeleton puppet-class-comment
  "Promts you for values and automatically inserts the comments for your class comment."
  nil
  (concat "# == Class: " (file-name-base buffer-file-truename) "\n")
  (truncate-string-to-fit (skeleton-read (concat "Description of class " (file-name-base buffer-file-truename) ": ")) 80 "# ")
  "#\n"
  "# === Parameters\n"
  "#\n"
  ("Parameter name: "
   "# [*" str "*]\n"
   "#\n"
   (truncate-string-to-fit (skeleton-read (concat "Description of " str ": ")) 80 "#   ")
   "#\n");(puppet-parameter-comment)
  "# === Variables\n"
  "#\n"
  ("Variable name: "
   "# [*" str "*]\n"
   "#\n"
   (truncate-string-to-fit (skeleton-read (concat "Description of " str ": ")) 80 "#   ")
   "#\n");(puppet-variable-comment)
  "# === Examples\n"
  "#\n"
  (truncate-string-to-fit (skeleton-read (concat "Code for example usage of class " (file-name-base buffer-file-truename) ": ")) 80 "# ")
  "#\n"
  "# === Authors\n"
  "#\n"
  ("Author name: "
   "# " str
   (let ((email (skeleton-read "Author Email: "))) (if (string= "" email) "" (concat " <" email ">")))"\n");(puppet-author-comment)
  "#\n"
  "# === Copyright\n"
  "#\n"
  "# "
  (skeleton-read "\"Copyright year name\" or license name: ") "\n"
  "#")

(define-skeleton puppet-define-comment
  "Promts you for values and automatically inserts the comments for you define comment."
  nil
  (concat "# == Define: " (file-name-base buffer-file-truename) "\n")
  (truncate-string-to-fit (skeleton-read (concat "Description of define " (file-name-base buffer-file-truename) ": ")) 80 "# ")
  "#\n"
  "# === Parameters\n"
  "#\n"
  ("Parameter name: "
   "# [*" str "*]\n"
   "#\n"
   (truncate-string-to-fit (skeleton-read (concat "Description of " str ": ")) 80 "#   ")
   "#\n");(puppet-parameter-comment)
  "# === Examples\n"
  "#\n"
  (truncate-string-to-fit (skeleton-read (concat "Code for example usage of class " (file-name-base buffer-file-truename) ": ")) 80 "# ")
  "#\n"
  "# === Authors\n"
  "#\n"
  ("Author name: "
   "# " str
   (let ((email (skeleton-read "Author Email: "))) (if (string= "" email) "" (concat " <" email ">")))"\n");(puppet-author-comment)
  "#\n"
  "# === Copyright\n"
  "#\n"
  "# "
  (skeleton-read "\"Copyright year name\" or license name: ") "\n"
  "#")

(define-skeleton puppet-parameter-comment
  "Promts you continuously for variables and their descriptions and inserts them."
  nil
  ("Parameter name: "
   "# [*" str "*]\n"
   "#\n"
   (truncate-string-to-fit (skeleton-read (concat "Description of " str ": ")) 80 "#   ")
   "#\n")
  (delete-char -1 nil))

(define-skeleton puppet-variable-comment
  "Promts you continuously for variables and their descriptions and inserts them."
  nil
  ("Variable name: "
   "# [*" str "*]\n"
   "#\n"
   (truncate-string-to-fit (skeleton-read (concat "Description of " str ": ")) 80 "#   ")
   "#\n")
  (delete-char -1 nil))

(define-skeleton puppet-author-comment
  "Promts you continuously for author names and their email addresses and inserts them."
  nil
  ("Author name: "
   "# " str
   (let ((email (skeleton-read "Author Email: "))) (if (string= "" email) "" (concat " <" email ">")))"\n")
  (delete-char -1 nil))

(provide 'puppet-comment-mode)

;; Mode definition
(define-minor-mode puppet-comment-mode
  "Toggle puppet-comment-mode
This minor-mode provides skeletons for writing comments in puppet"
  :init-value nil
  :lighter "puppet-comment"
  :keymap puppet-comment-mode-map)

(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-comment-mode))

;;; puppet-comment.el ends here
