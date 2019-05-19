;;; org-status.el --- Dashboard in Org mode -*- lexical-binding: t -*-

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

;; This package provides a framework for building an Org-based
;; dashboard.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org)

(defgroup org-status nil
  "Display statuses in Org mode."
  :group 'org)

(defconst org-status-buffer "*org-status*"
  "Name of the status buffer.")

(defcustom org-status-header "#+title: Status\n"
  "Function to generate the initial part of the status buffer.

This can be either a literal string or a function that takes no
argument and returns a string.

The string is inserted into the beginning of the status buffer."
  :group 'org-status
  :type '(or (string :tag "Literal content")
             (function :tag "Function generating a header")))

(defcustom org-status-sections
  '(("Org" org-status-org-content))
  "List of sections in the status buffer.

Each item is a list that consists of the following data:

- A tag which is added to the heading of the inserted entry.
  This tag is used to identify the entry, and an existing entry with
  the tag is deleted when the status is updated.

- A function which takes no argument and returns a string."
  :group 'org-status
  :type '(repeat (list (string :tag "Org tag to identity the section")
                       (function :tag "Function generating the content"))))

(defcustom org-status-startup-idle 3
  "Idle time until after startup you use `org-status-startup-mode'."
  :group 'org-status
  :type 'integer)

(defcustom org-status-display 'multi-column
  "How to display the status buffer.

If you select \"multi-column\", the frame is splitted based on
the value of `org-status-column-width'.

The window may be recentered depending on the value of
`org-status-recenter'.
"
  :group 'org-status
  :type '(choice (const :tag "Switch to buffer in the window" switch)
                 (const :tag "Make it the only window in the frame" only-window)
                 (const :tag "Multiple columns" multi-column)))

(defcustom org-status-position 'first-headline
  "Position to jump to after updating the buffer."
  :group 'org-status
  :type '(choice (const "Make the first header" first-headline)
                 (const "Show the beginning" beginning)
                 (const "Noop" nil)))

(defcustom org-status-column-width 50
  "Width of each column when you set `org-status-display' to \"multi-column\"."
  :group 'org-status
  :type 'integer)

(defvar org-status-mode-map
  (let ((map (make-composed-keymap nil org-mode-map)))
    (define-key map "q" #'org-status-quit-window)
    map))

(defvar org-status-previous-window-configuration nil)

;;;###autoload
(defun org-status-get-buffer ()
  "Return the status buffer."
  (or (get-buffer org-status-buffer)
      (org-status--initialize-buffer)))

;;;###autoload
(defun org-status-as-initial-buffer ()
  "Return a possibly empty status buffer.

This is a function used as `initial-buffer-choice' when
`org-status-startup-mode' is on."
  (if after-init-time
      ;; Initial buffer after Emacs initialization, e.g. emacsclient
      (with-current-buffer (org-status-get-buffer)
        (org-status--update)
        (current-buffer))
    (org-status--initialize-buffer :empty t)))

(cl-defun org-status--initialize-buffer (&key empty)
  "Return a new buffer for displaying statuses."
  (with-current-buffer (get-buffer-create org-status-buffer)
    (org-mode)
    (use-local-map org-status-mode-map)
    (unless empty
      (org-status--insert-header))
    (when (eq org-status-display 'multi-column)
      (follow-mode 1))
    (read-only-mode t)
    (current-buffer)))

(defun org-status--insert-header ()
  "Insert the header into the current buffer.

The content is configured in `org-status-header'."
  (let ((inhibit-read-only t))
    (cl-etypecase org-status-header
      (string (insert org-status-header))
      (function (insert (condition-case-unless-debug err
                            (funcall org-status-header)
                          (err (format "Error while running `org-status-header': %s" err)))))
      (null nil))))

(defun org-status--startup ()
  "Initialize the content of the status buffer after a given idle time."
  (run-with-timer
   org-status-startup-idle
   nil
   (lambda ()
     (with-current-buffer org-status-buffer
       (org-status--insert-header)
       (org-status--update)))))

(defun org-status--heading-with-tag-regexp (tag)
  "Return a regular expression for an Org heading with TAG."
  (concat (rx bol (+ "*") space (+ nonl) ":")
          (regexp-quote tag) ":"))

(defun org-status-update ()
  (interactive)
  (unless (equal (buffer-name) org-status-buffer)
    (user-error "Not in the status buffer"))
  (org-status--update))

(defun org-status--update ()
  "Update the statuses in the buffer."
  (let ((inhibit-read-only t))
    (org-with-wide-buffer
     (cl-loop for (tag func) in org-status-sections
              do (progn
                   (goto-char (point-min))
                   ;; Find a heading with the given tag.
                   (if (and tag
                            (re-search-forward
                             (org-status--heading-with-tag-regexp tag)
                             nil t))
                       (let (beg end)
                         (beginning-of-line)
                         (setq beg (point))
                         (org-end-of-subtree)
                         (setq end (point))
                         (delete-region beg end))
                     (goto-char (point-max))
                     (unless (= 0 (car (posn-col-row (posn-at-point (point)))))
                       (insert "\n")))
                   (let ((beg (point)))
                     (insert (condition-case-unless-debug err
                                 (funcall func)
                               (err (format-message "* Error from %s\n%s"
                                                    func err)))
                             "\n")
                     (when (and tag (> (point) (1+ beg)))
                       (goto-char beg)
                       (org-toggle-tag tag))))))
    (org-status--reposition)))

(defun org-status--reposition ()
  "Move to a designated position after updating.

The position is specified by `org-status-recenter'."
  (cl-case org-status-position
    ('first-headline
     (goto-char (point-min))
     (re-search-forward (rx bol "*" space) nil t)
     (beginning-of-line))
    (t
     (goto-char (point-min)))))

;;;###autoload
(define-minor-mode org-status-startup-mode
  "Global minor mode which makes the status buffer the initial buffer of Emacs."
  nil nil nil
  :global t
  (when org-status-startup-mode
    (setq initial-buffer-choice #'org-status-as-initial-buffer)
    (add-hook 'emacs-startup-hook #'org-status--startup)))

;;;; Display

;;;###autoload
(defun org-status ()
  "Update the status buffer and display it."
  (interactive)
  (let ((buffer (org-status-get-buffer)))
    (with-current-buffer buffer
      (org-status--update))
    (org-status--display buffer)))

(defun org-status-redisplay ()
  "Display the status buffer without updating its contents."
  (interactive)
  (let ((existing (get-buffer org-status-buffer))
        (buffer (org-status-get-buffer)))
    (unless existing
      (with-current-buffer buffer
        (org-status--update)))
    (org-status--display buffer)))

(defun org-status--display (buffer)
  "Display BUFFER in the configured manner."
  (setq org-status-previous-window-configuration
        (current-window-configuration))
  (cl-ecase org-status-display
    (switch (switch-to-buffer buffer))
    (only-window (progn
                   (switch-to-buffer buffer)
                   (delete-other-windows)))
    (multi-column (progn
                    (switch-to-buffer buffer)
                    (delete-other-windows)
                    (dolist (_i (number-sequence 1 (1- (/ (frame-width) org-status-column-width))))
                      (split-window-horizontally))
                    (balance-windows)
                    (add-hook 'window-configuration-change-hook 'org-status--oneshot))))
  (recenter 0))

(defun org-status--oneshot (&rest _)
  "Delete the other windows and delete itself from
`window-configuration-change-hook'."
  (delete-other-windows)
  (remove-hook 'window-configuration-change-hook 'org-status--oneshot))

(defun org-status-quit-window (kill)
  "Quit the status window and restore the previous configuration.

With a universal argument KILL, the buffer is killed."
  (interactive "P")
  (quit-window kill)
  (set-window-configuration org-status-previous-window-configuration)
  (setq org-status-previous-window-configuration nil))

(provide 'org-status)
;;; org-status.el ends here
