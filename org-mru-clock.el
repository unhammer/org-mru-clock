;;; org-mru-clock.el --- clock in/out of tasks with completion and persistent history -*- lexical-binding: t -*-

;; Copyright (C) 2016--2018 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience, calendar

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
;;; `org-clock-select-task' and `org-clock-in-last' with functions
;;; `org-mru-clock-select-recent-task' and `org-mru-clock-in', which
;;; first ensure that `org-clock-history' is filled with your
;;; `org-mru-clock-how-many' most recent tasks, and let you pick from
;;; a list before clocking in.

;;; It also uses `completing-read-function' (overridable with
;;; `org-mru-clock-completing-read') on `org-mru-clock-in' to make
;;; clocking in even faster.

;;; To use, require and bind whatever keys you prefer to the
;;; interactive functions:
;;;
;;; (require 'org-mru-clock)
;;; (global-set-key (kbd "C-c C-x i") #'org-mru-clock-in)
;;; (global-set-key (kbd "C-c C-x C-j") #'org-mru-clock-select-recent-task)
;;;
;;; Maybe trade some initial slowness for more tasks cached:
;;;
;;; (setq org-mru-clock-how-many 100)
;;;
;;; But don't set it higher than the actual number of tasks; then
;;; it'll always try (and fail) to fill up the history cache!

;;; If you want to use ivy for `org-mru-clock-in':
;;;
;;; (setq org-mru-clock-completing-read #'ivy-completing-read)
;;;

;;; If you prefer `use-package', the above settings would be:
;;;
;;; (use-package org-mru-clock
;;;   :ensure t
;;;   :bind* (("C-c C-x i" . org-mru-clock-in)
;;;           ("C-c C-x C-j" . org-mru-clock-select-recent-task))
;;;   :init
;;;   (setq org-mru-clock-how-many 100
;;;         org-mru-clock-completing-read #'ivy-completing-read))

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
  "Like `completing-read-function', but only used in `org-mru-clock' functions.
Popular choices include `ivy-completing-read' and `ido-completing-read'."
  :group 'org-mru-clock
  :type 'function)

(defcustom org-mru-clock-include-entry-at-point t
  "If point is at an org headline, include it as the top choice."
  :group 'org-mru-clock
  :type 'boolean)

(defun org-mru-clock-take (n l)
  "Take N elements from list L."
  (let (ret)
    (while (and l (> n 0))
      (push (car l) ret)
      (cl-decf n)
      (setq l (cdr l)))
    (reverse ret)))

(defun org-mru-clock-heading-marker (marker)
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
            (let ((m (point-marker)))
              ;; in hash maps at least, #'equal doesn't work for
              ;; markers, so extract only what's relevant:
              (cons (marker-position m)
                    (marker-buffer m)))))))))

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

(defun org-mru-clock-take-uniq (n l key test)
  "Take the N first elements from L, skipping duplicates.
Elements are duplicates if KEY of each element is equal under TEST."
  (let* ((seen (make-hash-table :test test :size n))
         ret
         (_was-trimmed (catch 'done
                         (dolist (e l)
                           (let ((k (funcall key e)))
                             (unless (gethash k seen)
                               (push e ret))
                             (puthash k e seen))
                           (when (>= (hash-table-count seen) n)
                             (throw 'done t))))))
    (reverse ret)))

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
           (uniq (org-mru-clock-take-uniq
                  n
                  sorted
                  #'org-mru-clock-heading-marker
                  #'equal)))
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

(defun org-mru-clock-format-entry ()
  "Return the parent heading string appended to the heading at point."
  (let* ((this (org-get-heading 'no-tags 'no-todo))
         (parent
          (save-excursion
            (org-up-heading-safe)
            (org-get-heading 'no-tags 'no-todo)))
         (parent-post (if parent
                          (format " (%s)" parent)
                        "")))
    (concat this parent-post)))

(defun org-mru-clock--clock-in (task)
  "Clock into the TASK (cons of description and marker)."
  (let ((m (cdr task)))
    (with-current-buffer
        (org-base-buffer (marker-buffer m))
      (org-with-wide-buffer
       (goto-char (marker-position m))
       (org-clock-in)))))

(defun org-mru-clock--goto (task)
  "Go to buffer and position of the TASK (cons of description and marker)."
  (let ((m (cdr task)))
    (switch-to-buffer (org-base-buffer (marker-buffer m)))
    (if (or (< m (point-min)) (> m (point-max))) (widen))
    (goto-char m)
    (org-show-entry)
    (org-back-to-heading t)
    (org-cycle-hide-drawers 'children)
    (org-reveal)))

(eval-after-load 'ivy
  '(ivy-set-actions 'org-mru-clock-in
                    '(("g" org-mru-clock--goto "goto"))))

(defun org-mru-clock--read (prompt collection action caller)
  "Completing-read helper `org-mru-clock-in'.
Support extra actions if we're using ivy.
PROMPT and COLLECTION as in `completing-read',
ACTION and CALLER as in `ivy-read'."
  (if (eq org-mru-clock-completing-read #'ivy-completing-read)
      (ivy-read prompt
                collection
                :action action
                :require-match t
                :caller caller)
    (funcall action
             (assoc (funcall org-mru-clock-completing-read
                             prompt
                             (mapcar #'car collection)
                             nil
                             'require-match)
                    collection))))

(defun org-mru-clock--collect-history (history)
  "Turn HISTORY into a collection usable for `completing-read'.
HISTORY is e.g. `org-clock-history'.  Outputs a list of pairs of
headline strings and markers."
  (let (res)
    (dolist (i history)
      (with-current-buffer
          (org-base-buffer (marker-buffer i))
        (org-with-wide-buffer
         (ignore-errors
           (goto-char (marker-position i))
           (push (cons (org-mru-clock-format-entry) i) res)))))
    (reverse res)))

(defun org-mru-clock--collect-entry-at-point ()
  "Make a \"collection\" of a single entry with the heading at point.
Return nil if we're not looking at an org heading. Works both for
regular org files and the agenda. Output format should be the
same as `org-mru-clock--collect-history'."
  (when org-mru-clock-include-entry-at-point
    (if (and (eq major-mode 'org-mode)
             (eq (car (org-element-at-point)) 'headline))
        (list (cons (org-mru-clock-format-entry) (point-marker)))
      ;; If in agenda, first follow link to org file:
      (when (eq major-mode 'org-agenda-mode)
        (let ((m (org-get-at-bol 'org-hd-marker)))
          (when m
            (with-current-buffer (org-base-buffer (marker-buffer m))
              (org-with-wide-buffer
               (goto-char (marker-position m))
               (org-mru-clock--collect-entry-at-point)))))))))

;;;###autoload
(defun org-mru-clock-in (&optional n)
  "Use completion to clock in to a task recently associated with clocking.
See `org-mru-clock-completing-read' for the completion function used.
Optional argument N as in `org-mru-clock'."
  (interactive "P")
  (org-mru-clock-to-history n)
  (let ((prompt "Recent clocks: ")
        ;; Remove string faces, possibly include entry-at-point:
        (collection (mapcar (lambda (kv)
                              (setf (car kv) (substring-no-properties (car kv)))
                              kv)
                            (append (org-mru-clock--collect-entry-at-point)
                                    (org-mru-clock--collect-history org-clock-history)))))
    (org-mru-clock--read prompt
                         collection
                         #'org-mru-clock--clock-in
                         #'org-mru-clock-in)))


(provide 'org-mru-clock)
;;; org-mru-clock.el ends here
