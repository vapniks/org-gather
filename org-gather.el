;;; org-gather.el --- Gather data from various places and display in org table or tree

;; Filename: org-gather.el
;; Description: Gather data from various places and display in org table or tree
;; Author: Joe Bloggs <vapniks@yahoo.com>, Wojciech Gac <wojciech.s.gac@gmail.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>, Wojciech Gac <wojciech.s.gac@gmail.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs & Wojciech Gac, all rites reversed.
;; Created: 2015-02-03 23:30:53
;; Version: 0.1
;; Last-Updated: 2015-02-03 23:30:53
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/org-gather
;; Keywords: extensions
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((org "20150119"))
;;
;; Features that might be required by this library:
;;
;; org, gnus
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1FgnGFwRES9MGzoieRDWLmLkjqMT3AsjQF
;;
;; How to create documentation and distribute package:
;;
;;     1) Remember to add ;;;###autoload magic cookies if possible
;;     2) Generate a bitcoin address for donations with shell command: bitcoin getaccountaddress org-gather
;;        and place address after "Commentary:" above.
;;     3) Use org-readme-top-header-to-readme to create initial Readme.org file.
;;     4) Use M-x auto-document to insert descriptions of commands and documents
;;     5) Create documentation in the Readme.org file:
;;        - Use org-mode features for structuring the data.
;;        - Divide the commands into different categories and create headings
;;          containing org lists of the commands in each category.
;;        - Create headings with any other extra information if needed (e.g. customization).
;;     6) In this buffer use org-readme-to-commentary to fill Commentary section with
;;        documentation from Readme.org file.
;;     7) Make any necessary adjustments to the documentation in this file (e.g. remove the installation
;;        and customization sections added in previous step since these should already be present).
;;     8) Use org-readme-marmalade-post and org-readme-convert-to-emacswiki to post
;;        the library on Marmalade and EmacsWiki respectively.
;; 
;;;;


;;; Installation:
;;
;; Put org-gather.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'org-gather)

;;; Customize:
;;
;; To automatically insert descriptions of customizable variables defined in this buffer
;; place point at the beginning of the next line and do: M-x auto-document

;;
;; All of the above can customized by:
;;      M-x customize-group RET org-gather RET
;;

;;; Change log:
;;	
;; 2015/02/03
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require


;;; Code:
;; simple-call-tree-info: DONE  
(defgroup org-gather nil
  "org functions for gathering data"
  :group 'gnus-summary)

;; simple-call-tree-info: DONE  
(defcustom org-gather-saved-functions nil
  "A list of named functions that can be used with `org-gather-from' commands.
Each element has the form (NAME ARGLIST EXPRESSION [EXPRESSION ...]).

NAME is a symbol naming the function.

ARGLIST is a list whose elements have the form (ARGUMENT
DEFAULT-VALUE). These variables are available when evaluating
the expressions.

EXPRESSION are elisp forms. They are wrapped in a `progn' and
compose the body of the function."
  :group 'org-gather
  :type  '(repeat (cons (symbol :tag "Function name")
                        (cons
                         (repeat :tag "Argument list"
                                 (list (symbol :tag "Argument name")
                                       (sexp :tag "Default value")))
                         (repeat (sexp :tag "Expression"))))))


;; simple-call-tree-info: STARTED  
(defun org-gather-from-single-buffer (buf vals &optional repeat)
  "Gather data from buffer BUF.
VALS should be an alist whose key are names for each piece of data extracted, and whose values are sexp's which
when evaluated in the buffer will return a piece of data.
The sexp's may utilize any functions defined in `org-gather-saved-functions' or any of the following functions
during of the gathering process:

(regex (STR) ...) - search for a match to regular expression STR, and return the match.
                    If STR contains a subexpression, return the match to that instead.
(region (START END &optional SSTART EEND) ...) - return the region between START and END which can be either
                    buffer positions, or regular expressions. If START is a regular expression then the
                    end of the matching text in the buffer will be used for the start position, unless
                    the optional argument SSTART is non-nil in which case the beginning of the match
                    will be used. Similarly for END, except in this case the end of the match is the default
                    position and if EEND is non-nil the start will be used.
(rect (START END SSTART EEND) ...) - same as region but returns the rectangle defined by START and END instead."
  (eval
   ;; code to put some functions in scope
   ;; these functions can be used in the values of the VALS alist to gather data
   `(cl-flet* ((regex (str) (if (re-search-forward str nil t) ; simple function which gets data using
                                (if (matching-substring 1)    ; a regexp match
                                    (match-string 1)
                                  (match-string 0))))
               (startend (start end sstart eend) ; helper function for finding start & end positions
                         (let* ((startmatch (if sstart 'match-beginning 'match-end))
                                (endmatch (if eend 'match-end 'match-beginning))
                                (startpos (if (numberp start) start
                                            (and (re-search-forward start nil t)
                                                 (if (matching-substring 1)
                                                     (funcall startmatch 1)
                                                   (funcall startmatch 0)))))
                                (endpos (if (numberp end) end
                                          (and (re-search-forward end nil t)
                                               (if (matching-substring 1)
                                                   (funcall endmatch 1)
                                                 (funcall endmatch 0))))))
                           (list startpos endpos)))
               (rect (start end &optional sstart eend) ; get rectangle of data
                     (let ((startpos (startend start end sstart eend))
                           (endpos (startend start end sstart eend)))
                       (if (and startpos endpos)
                           (extract-rectangle startpos endpos))))
               (region (start end &optional sstart eend) ; get region of data
                       (let ((startpos (startend start end sstart eend))
                             (endpos (startend start end sstart eend)))
                         (if (and startpos endpos)
                             (buffer-substring startpos endpos))))
               ;; splice in user defined functions from org-gather-saved-functions
               ,@(cl-loop for (name . code) in org-gather-saved-functions
                          if (> (length code) 1)
                          collect `(,name (&optional ,@(car code)) ,@(cdr code))
                          else
                          collect (list name nil code)))
      (with-current-buffer buf
        ;; now collect the data from the buffer              
        ))))

;; simple-call-tree-info: TODO
(defun org-gather-from-buffers ()
  "Gather data from single buffer.")

;; simple-call-tree-info: TODO
(defun org-gather-from-comint-outputs ()
  "Gather data from coming outputs.")

;; simple-call-tree-info: TODO
(defun org-gather-from-gnus-articles ()
  "Gather data from gnus articles.")

;; simple-call-tree-info: TODO
(defun org-gather-from-list ()
  "Gather data from list.")

;; simple-call-tree-info: TODO
(defun org-gather-from-files ()
  "Gather data from files.")

;; simple-call-tree-info: TODO
(defun org-gather-from-urls ()
  "Gather data from urls.")

;; simple-call-tree-info: TODO
(defun org-gather-create-table ()

  )

;; simple-call-tree-info: TODO
(defun org-gather-create-tree ()

  )

(provide 'org-gather)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "org-gather.el" (buffer-name) (buffer-string) "update")

;;; org-gather.el ends here
