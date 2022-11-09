;;; pretty-pages.el --- create webpages with pretty urls            -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Haider Mirza

;; Author: Haider Mirza <paralle1epiped@outlook.com>
;; Keywords: lisp webpage
;; Version: 1.1.0

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

;; TODO: Remove reliance of find command
(defun pretty-pages-open ()
  "Opens a webpage"
  (interactive)
  (when (= (length pretty-pages-root) 0)
    (user-error "Please set the variable 'pretty-pages-root'"))
  (when (not (executable-find "find"))
    (user-error "Make sure the command 'find' is installed"))
  (when (not (executable-find "grep"))
    (user-error "Make sure the command 'grep' is installed"))
  (let* ((index-files
	  (replace-regexp-in-string "\n$" ""
				    (shell-command-to-string
				     (concat "find " (expand-file-name pretty-pages-root) " -type f | grep index.org"))))
	 (simplified-files
	  (if (atom index-files)
	      (s-replace "/" "-"
			 (s-replace "/index.org" ""
				    (s-replace
				     (if (string-suffix-p "/" pretty-pages-root)
					 (expand-file-name pretty-pages-root)
				       (concat (expand-file-name pretty-pages-root) "/")) "" index-files)))
	    (mapcar #'iterate l)))
	 (selected-file
	  (completing-read "Select a page: " (split-string simplified-files "\n"))))

    (message index-files)
    (find-file (if (string-suffix-p "/" pretty-pages-root)
		   (concat pretty-pages-root (s-replace "-" "/" selected-file) "/" "index.org")
		 (concat pretty-pages-root "/" (s-replace "-" "/" selected-file) "/" "index.org")))))


(defun pretty-pages-create-in-subdirectory (name directory)
  "Create a pretty webpage in a subdirectory listed in the variable 'pretty-pages-subdirectories'"
  (interactive (list (read-string "Name for webpage: ")
		     (read-directory-name "What directory? " 
					  (when (not (string-suffix-p "/" pretty-pages-root))
					    (concat pretty-pages-root "/")))))
  (when (= (length pretty-pages-root) 0)
    (user-error "Please set the variable (pretty-pages-root)"))
  (let ((file (concat directory name "/" "index." pretty-pages-file-extention)))
    (when (not (file-directory-p directory))
      (if (yes-or-no-p (concat "The directory '" directory "' doesn't exist. Create? "))
	  (progn
	    (make-directory directory)
	    (message (concat "Created New directory" directory)))
	(user-error (concat "Directory '" directory "' doesn't exist"))))
    (make-directory (concat directory name "/"))
    (make-empty-file file)
    (find-file file)))

(defun pretty-pages-create (name)
  "Create a pretty webpage"
  (interactive "MName for webpage: ")
  (if (= (length pretty-pages-root) 0)
      (user-error "Please set the variable (pretty-pages-root)")
    (let* ((directory
	    (if (string-suffix-p "/" pretty-pages-root)
		(concat pretty-pages-root name "/")
	      (concat pretty-pages-root "/" name "/")))
	   (file (concat directory "index." pretty-pages-file-extention)))
      (if (file-directory-p directory)
	  (user-error (concat "There is already a directory in '" pretty-pages-root "' with that name"))
	(make-directory directory)
	(make-empty-file file)
	(find-file file)))))

(provide 'pretty-pages)
