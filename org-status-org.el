;;; org-status-org.el --- Org mode status -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.10"))
;; Keywords: org
;; URL: https://github.com/akirak/org-status

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a module for displaying the status of Org
;; mode in org-status.el.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'seq)
(require 'dash)

(declare-function 'org-agenda-files "org")

(defgroup org-status-org
  nil
  "Display status of Org mode."
  :group 'org-status
  :group 'org)

(defcustom org-status-org-agenda-commands t
  "If non-nil, display `org-agenda-custom-commands'."
  :group 'org-status-org
  :type 'boolean)

(defcustom org-status-org-files t
  "If non-nil, display files."
  :group 'org-status-org
  :type 'boolean)

;;;###autoload
(defun org-status-org-content ()
  "Generate the status of Org."
  (string-join `("* Org "
                 ,@(when org-status-org-agenda-commands
                     `("** Org Agenda Commands"
                       ,(org-status-org--agenda-commands-content)))
                 ,@(when org-status-org-files
                     `("** Org Files"
                       ,(org-status-org--file-list-content))))
               "\n"))

(defun org-status-org--agenda-commands-content ()
  "Generate a list of the custom agenda commands."
  (mapconcat (lambda (entry)
               (let ((key (car entry))
                     (description (nth 1 entry)))
                 (concat "- "
                         (org-link-make-string
                          (format "elisp:(org-agenda nil \"%s\")" key)
                          (format "[%s] %s" key description)))))
             org-agenda-custom-commands
             "\n"))

(defun org-status-org--file-list-content ()
  "Generate information on Org files."
  (let ((agenda-files (org-agenda-files))
        (known-files (bound-and-true-p org-starter-known-files))
        (deprecated-files (bound-and-true-p org-starter-deprecated-files)))
    (cl-loop for (heading files) in `(("Agenda Files" ,agenda-files)
                                      ("Known Files" ,(seq-difference known-files agenda-files #'file-equal-p))
                                      ("Deprecated Files" ,deprecated-files))
             when files
             concat (concat "*** " heading "\n"
                            (org-status-org--grouped-file-list files)
                            "\n"))))

(defun org-status-org--grouped-file-list (files)
  "Format a list of FILES grouoped by directory."
  (--> files
       (-group-by #'file-name-directory it)
       (cl-sort it #'string< :key #'car)
       (mapconcat (lambda (group)
                    (let ((dir (car group))
                          (files (cdr group)))
                      (concat "- " (abbreviate-file-name dir) "\n"
                              (mapconcat (lambda (fpath)
                                           (concat "  - "
                                                   (org-link-make-string
                                                    (concat "file:" fpath)
                                                    (file-name-nondirectory fpath))))
                                         files "\n"))))
                  it "\n")))

(provide 'org-status-org)
;;; org-status-org.el ends here
