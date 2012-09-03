;;; doctags.el --- Generator of tags documentation

;; Copyright (C) 2012  Matthias Meulien

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: convenience, languages

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

;;; Code:

(require 'custom)

(defgroup doctags nil
  "Generator of tags documentation."
  :group 'tools
  :version "24.0")

(defun doctags-c-generator-type (tag)
  "Return a C-like documentation string for TAG assuming its
class is 'type.")

(defun doctags-c-generator-function (tag)
  "Return a C-like documentation string for TAG assuming its
class is 'function."
  (let ((docstring)
	(template "\t/*!\n\t\\brief \n\n%s%s\t*/")
	(args (semantic-tag-function-arguments tag))
	(type (semantic-tag-get-attribute tag :type)))
    (progn
      (setq docstring
	    (format template
		    (apply 'concat
			   (mapcar (lambda (arg) 
				     (concat "\t\\param " (car arg) " \n"))
				   args))
		    (if (not (equal type "void")) "\t\\return \n" "")))
      docstring)))

;; (defun doctags-c-skeleton-function (tag)
;;   "Return a C-like documentation string for TAG assuming its
;; class is 'function."
;;   (let ((elements (list ">" "/*!\n" 
;; 			(doctags-c-generator-command "brief") " \n"))
;; 	(template "\t/*!\n\t\\brief \n\n%s%s\t*/")
;; 	(args (semantic-tag-function-arguments tag))
;; 	(type (semantic-tag-get-attribute tag :type)))
;;     (progn
;;       (setq docstring
;; 	    (format template
;; 		    (apply 'concat
;; 			   (mapcar (lambda (arg) 
;; 				     (concat "\t\\param " (car arg) " \n"))
;; 				   args))
;; 		    (if (not (equal type "void")) "\t\\return \n" "")))
;;       (append '(nil) elements))))

(defun doctags-c-generator-variable (tag)
  "Return a C-like documentation string for TAG assuming its
class is 'variable.")

(defun doctags-c-generator-package (tag)
  "Return a C-like documentation string for TAG assuming its
class is 'package.")

(defun doctags-c-generator-code (tag)
  "Return a C-like documentation string for TAG assuming its
class is 'code.")

(defun doctags-c-generator-command (name)
  "Return NAME prefixed by an escaped backslash or an at-sign
according to `doctags-c-generator-command-style'."
  (cond
   ((eq doctags-c-generator-command-style 'backslash) 
    (concat "\\" name))
   ((eq doctags-c-generator-command-style 'at-sign)
    (concat "@" name))))

(defun doctags-c-generator (class)
  "The default documentation generator, handling various Doxygen
styles for C-like languages."
  (cond
   ((eq class 'type) 'doctags-c-generator-type)
   ((eq class 'function) 'doctags-c-generator-function)
   ((eq class 'variable) 'doctags-c-generator-variable)
   ((eq class 'package) 'doctags-c-generator-package)
   ((eq class 'code) 'doctags-c-generator-code)))

(defcustom doctags-c-generator-command-style backslash
  "Style of commands."
  :type '(radio (const :tag "backslash" backslash)
		(const :tag "at-sign" at-sign))
  :group 'doctags)

;; (defcustom doctags-c-generator-command-style) ; backslash or at-sign
;; (defcustom doctags-c-generator-use-autobrief) ; true or false
;; (defcustom doctags-c-generator-block-style) ; JavaDoc, Qt
;; (defcustom doctags-c-generator-document-after) ; list of tags classes

(defvar doctags-generator 'doctags-c-generator)

(defun doctags-document-current-tag ()
  "Generate documentation for the current tag."
  (interactive)
  (let ((tag (semantic-current-tag))
	(generator doctags-generator))
    (when (and tag (symbolp generator))
      (let* ((class (semantic-tag-class tag))
	     (skel (funcall (funcall generator class) tag)))
	(goto-char (semantic-tag-start tag))
	(beginning-of-line)
	(insert skel)))))

(provide 'doctags)
;;; doctags.el ends here
