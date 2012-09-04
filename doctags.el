;;; doctags.el --- Generation of tags documentation

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

(defcustom doctags-c-generator-command-style 'backslash
  "Style of commands."
  :type '(radio (const :tag "backslash" backslash)
		(const :tag "at-sign" at-sign))
  :group 'doctags)

(defcustom doctags-c-generator-use-autobrief nil
  "Whether to use the autobrief style or not."
  :type '(boolean)
  :group 'doctags)

;; (defcustom doctags-c-generator-block-style) ; JavaDoc, Qt
;; (defcustom doctags-c-generator-document-after) ; list of tags classes

(defun doctags-c-generator-type (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'type tag.")

(defun doctags-c-generator-function (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'function tag."
  `(nil > "/*!" \n
	> ,(doctags-c-generator-command "brief") " " _ \n
	> \n
	(nil 
	 (,(mapcar (lambda (arg) (car arg)) (semantic-tag-function-arguments tag)) 
	  >  ,(doctags-c-generator-command "param") " " str \n))
	,(cond 
	  ((not (equal (semantic-tag-get-attribute tag :type) "void"))
	   (list 'nil '> (doctags-c-generator-command "return") " " '\n))
	  (t ""))
	> "*/"))

(defun doctags-c-generator-variable (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'type tag.")

(defun doctags-c-generator-package (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'package tag.")

(defun doctags-c-generator-code (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'code tag.")

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

(defvar doctags-generator 'doctags-c-generator)

(defun doctags-document-current-tag ()
  "Generate documentation for the current tag."
  (interactive)
  (let ((tag (semantic-current-tag))
	(generator doctags-generator))
    (when (and tag (symbolp generator))
      (let ((class (semantic-tag-class tag)))
	(goto-char (semantic-tag-start tag))
	(beginning-of-line)
	(skeleton-insert (funcall (funcall generator class) tag))))))

(provide 'doctags)
;;; doctags.el ends here
