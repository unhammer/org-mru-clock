;;; org-mru-clock.el --- load most recently used clocks into history, provide selectors -*- lexical-binding: t -*-

;; Copyright (C) 2016--2017 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: org, convenience, calendar

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Do you often clock in to many different little tasks? Are you
;;; annoyed that you can't just clock in to one of your most recent
;;; tasks after restarting Emacs? This package replaces functions like
;;; `org-clock-select-task' and `org-clock-in-last' with ones that
;;; first ensure that `org-clock-history' is filled with your
;;; `org-mru-clock-how-many' most recent tasks, and let you pick from
;;; a list before clocking in.

;;; To use, require and bind whatever keys you prefer to the
;;; interactive functions:
;;;
;;; (require 'org-mru-clocks)
;;; (global-set-key (kbd "C-c C-x i") #'org-mru-clock-in)
;;; (global-set-key (kbd "C-c C-x C-j") #'org-mru-clock-select-recent-task)
;;;
;;; Maybe trade some initial slowness for more tasks cached:
;;;
;;; (setq org-mru-clock-how-many 100)
;;;
;;; But don't set it higher than the actual number of tasks; then
;;; it'll always try (and fail) to fill up the history cache!

;;; If you prefer `use-package', that would be:
;;;
;;; (use-package org-mru-clock
;;;   :defer t
;;;   :bind* (("C-c C-x i" . org-mru-clock-in)
;;;           ("C-c C-x C-j" . org-mru-clock-select-recent-task))
;;;   :init
;;;   (setq org-mru-clock-how-many 100))

;;; You may also be interested in these general org-clock settings:
;;;
;;; (setq org-clock-persist t)
;;; (org-clock-persistence-insinuate)


;;; Code:

(require 'org-clock)
(require 'cl-lib)

(defgroup org-mru-clock nil
  "Options for org-mru-clock"
  :tag "org-mru-clock"
  :group 'org)

(defcustom org-mru-clock-how-many 20
  "Default number of clock entries to look up with `org-mru-clock'.
This can be a bit slow the first time due to deduplication, but
the interactive functions cache the clocks to
`org-clock-history', and don't look up clocks if that variable
has enough entries."
  :group 'org-mru-clock
  :type 'integer)

(defcustom org-mru-clock-completing-read completing-read-function
  "Like `completing-read-function', but only used in org-mru-clock functions."
  :group 'org-mru-clock
  :type 'function)

(defun org-mru-clock-take (n l)
  "Take N elements from list L."
  (let (ret)
    (while (and l (> n 0))
      (push (car l) ret)
      (cl-decf n)
      (setq l (cdr l)))
    (reverse ret)))

(defun org-mru-heading-marker (marker)
  "Turn MARKER into a marker of the heading at that spot.
Used for uniquifying `org-mru-clock'."
  (when (marker-buffer marker)
    (with-current-buffer (org-base-buffer (marker-buffer marker))
      (save-excursion
        (save-restriction
          (widen)
          (ignore-errors
            (goto-char marker)
            (org-back-to-heading t)
            (point-marker)))))))

(defun org-mru-clock-find-clocks (file)
  "Search through the given FILE and find all open clocks."
  (let ((buf (or (get-file-buffer file)
                 (find-file-noselect file)))
        (org-clock-re (concat org-clock-string " \\(\\[.*?\\]\\)"))
        clocks)
    (with-current-buffer buf
      (org-with-wide-buffer
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward org-clock-re nil t)
           (push (cons (copy-marker (match-end 1) t)
                       (org-time-string-to-time (match-string 1)))
                 clocks)))))
    clocks))

(defun org-mru-clock (&optional n)
  "Find N most recently used clocks in `org-files-list'.
N defaults to `org-mru-clock-how-many'."
  (unless org-clock-resolving-clocks
    (let* ((org-clock-resolving-clocks t)
           (n (or n org-mru-clock-how-many))
           (clocks (cl-mapcan #'org-mru-clock-find-clocks (org-files-list)))
           (sort-pred (lambda (a b) (time-less-p (cdr b)
                                            (cdr a))))
           (sorted (mapcar #'car (sort clocks sort-pred)))
           (uniq (cl-remove-duplicates
                  ;; TODO: a bit hacky, might end up with <n (but uniq-ing is so slow)
                  (org-mru-clock-take (* n 3) sorted)
                  :test #'equal
                  :key #'org-mru-heading-marker)))
      (org-mru-clock-take n uniq))))

;;;###autoload
(defun org-mru-clock-to-history (&optional n)
  "Ensure `org-clock-history' filled with agenda tasks.
Optional argument N as in `org-mru-clock'."
  (interactive "P")
  (require 'cl-lib)
  (let ((n (cond ((and n (listp n)) (car n))
                 ((numberp n) n)
                 (t org-mru-clock-how-many)))
        (history (cl-remove-if (lambda (m) (not (marker-buffer m)))
                               org-clock-history)))
    (setq org-clock-history (if (< (length history) n)
                                (org-mru-clock n)
                              history))))

;;;###autoload
(defun org-mru-clock-select-recent-task (&optional n)
  "Select a task that was recently associated with clocking.
Like `org-clock-select-task', but ensures `org-clock-history' is
filled first.  Optional argument N as in `org-mru-clock'."
  (interactive "P")
  (org-mru-clock-to-history n)
  (let ((m (org-clock-select-task "Select recent task: ")))
    (switch-to-buffer (marker-buffer m))
    (goto-char (marker-position m))
    (org-up-element)
    (org-show-subtree)))

(defun org-mru-format-entry ()
  "Return the parent heading of the current heading."
  (let* ((this (org-get-heading 'no-tags 'no-todo))
         (parent
          (save-excursion
            (org-up-heading-safe)
            (org-get-heading 'no-tags 'no-todo)))
         (parent-post (if parent
                          (format " (%s)" parent)
                        "")))
    (concat this parent-post)))

;;;###autoload
(defun org-mru-clock-in (&optional n)
  "Use ido to clock in to a task recently associated with clocking.
Optional argument N as in `org-mru-clock'."
  (interactive "P")
  (org-mru-clock-to-history n)
  (let (res
        (history org-clock-history))
    (dolist (i history)
      (with-current-buffer
          (org-base-buffer (marker-buffer i))
        (org-with-wide-buffer
         (ignore-errors
           (goto-char (marker-position i))
           (push (cons (org-mru-format-entry) i) res)))))
    (let* ((l (reverse
               (mapcar #'substring-no-properties
                       (mapcar #'car res))))
           (task (cdr (assoc (funcall org-mru-clock-completing-read
                                      "Recent clocks: "
                                      l)
                             res))))
      (when task
        (with-current-buffer
            (org-base-buffer (marker-buffer task))
          (org-with-wide-buffer
           (goto-char (marker-position task))
           (org-clock-in)))))))


(provide 'org-mru-clock)
;;; org-mru-clock ends here
