;;; pretty-pages.el --- create webpages with pretty urls            -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Haider Mirza

;; Author: Haider Mirza <paralle1epiped@outlook.com>
;; Keywords: lisp webpage
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

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
;;
;; Pretty-pages is a Emacs Package that helps you create your webpages
;; in a format that makes your URL pretty.
;;
;; e.g.
;; https://parallelepiped.srht.site/contact/
;; instead of:
;; https://parallelepiped.srht.site/contact.html

;;; Code:

(defvar pretty-pages-file-extention "org"
  "The file extention you want your webpage to have. (e.g. org, md, html)
Make sure you assign to this variable the file extention and not the name of the format.")

(defvar pretty-pages-root nil
  "The directory where pretty webpages are to be created")

;; (setq pretty-pages-root "~/test")

;; TODO: Add a list with subdirectories user defines
(defun pretty-pages-create-webpage ()
  "Create a pretty webpage"
  (interactive)
  (if (= (length pretty-pages-root) 0)
      (message "Please set the variable (pretty-pages-root)")
    (let* ((name (read-string "Name for webpage: " ))
	   (directory
	    (if (string-suffix-p "/" pretty-pages-root)
		(concat pretty-pages-root name "/")
	      (concat pretty-pages-root "/" name "/")))
	   (file (concat directory "index." pretty-pages-file-extention)))
      (if (file-directory-p directory)
	  (message (concat "There is already a directory in '" pretty-pages-root "' with that name"))
	(make-directory directory)
	(make-empty-file file)
	(find-file file)))))

(provide 'pretty-pages)
