;;; b.el --- Collection of useful functions operating on buffers -*- lexical-binding: t -*-

;; Copyright (C) 2014 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 7th May 2014
;; Package-requires: ((dash "2.6.0") (s "1.9.0") (emacs "24"))
;; Keywords: buffer

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(defun b-put-marker-at (pos)
  "Create new marker and make it point at POS."
  (move-marker (make-marker) pos))

(defun b-delete-marker (marker)
  "Delete the marker."
  (move-marker marker nil))

(defun b-operate-on-region (beg-extract beg-after end-extract end-after extractor)
  "Operate on buffer region specified by \"motions\".

After this function returns, the point could have moved.  If you
want to preserve the point, wrap the call with `save-excursion'.

BEG-EXTRACT is a zero argument function which moves point to
the beginning of the region to be operated on.  If this function
returns nil, no further action is taken.

BEG-AFTER is a single argument function which is called after
BEG-EXTRACT.  The argument passed to this function is the return
value of BEG-EXTRACT.

END-EXTRACT is a zero argument function which moves point to
the end of the region to be operated on.  If this function
returns nil, no further action is taken.

END-AFTER is a single argument function which is called after
END-EXTRACT.  The argument passed to this function is the return
value of END-EXTRACT.

EXTRACTOR is a two argument function which is used to operate on
the content of the region.  It is called with the points
indicating start and end of the region."
  (-when-let (beg-arg (funcall beg-extract))
    (let* ((beg (prog1 (point)
                  (funcall beg-after beg-arg)))
           (end-arg (funcall end-extract))
           (re (funcall extractor beg (point))))
      (funcall end-after end-arg)
      re)))

(defun b-extract (beg-extract beg-after end-extract end-after)
  (b-operate-on-region beg-extract beg-after end-extract end-after 'buffer-substring-no-properties))

(defun b-extract-with-properties (beg-extract beg-after end-extract end-after)
  (b-operate-on-region beg-extract beg-after end-extract end-after 'buffer-substring))

(defun b-delete (beg-extract beg-after end-extract end-after)
  (b-operate-on-region beg-extract beg-after end-extract end-after 'delete-region))

(defun b-kill (beg-extract beg-after end-extract end-after)
  (b-operate-on-region beg-extract beg-after end-extract end-after 'kill-region))

(defun b-operate-on-region-by-match (end-or-beg end-or-inside inside? extractor)
  (cond
   ;; beg end t
   ((and end-or-beg
         (not (eq end-or-inside t))
         inside?)
    (b-operate-on-region
     (lambda () (search-forward end-or-beg nil t))
     'identity
     (lambda ()
       (-when-let (end (search-forward end-or-inside nil t))
         (goto-char (match-beginning 0))
         (b-put-marker-at end)))
     (lambda (x)
       (goto-char x)
       (b-delete-marker x))
     extractor))
   ;; beg end nil
   ((and end-or-beg
         (and (not (eq end-or-inside t))
              (not (eq end-or-inside nil)))
         (not inside?))
    (b-operate-on-region
     (lambda ()
       (-when-let (beg (search-forward end-or-beg nil t))
         (goto-char (match-beginning 0))
         (b-put-marker-at beg)))
     (lambda (x)
       (goto-char x)
       (b-delete-marker x))
     (lambda () (search-forward end-or-inside nil t))
     'identity
     extractor))
   ;; beg t
   ((and end-or-beg
         (eq end-or-inside t)
         (not inside?))
    (b-operate-on-region
     (lambda () t)
     'identity
     (lambda ()
       (-when-let (end (search-forward end-or-beg nil t))
         (goto-char (match-beginning 0))
         (b-put-marker-at end)))
     (lambda (x)
       (goto-char x)
       (b-delete-marker x))
     extractor))
   ;; beg nil
   ((and end-or-beg
         (not (eq end-or-inside t))
         (not inside?))
    (b-operate-on-region
     (lambda () t)
     'identity
     (lambda () (search-forward end-or-beg nil t))
     'identity
     extractor))))

(defun b-extract-by-match (end-or-beg &optional end-or-inside inside?)
  (b-operate-on-region-by-match end-or-beg end-or-inside inside? 'buffer-substring-no-properties))

(defun b-extract-by-match-with-properties (end-or-beg &optional end-or-inside inside?)
  (b-operate-on-region-by-match end-or-beg end-or-inside inside? 'buffer-substring))

(defun b-delete-by-match (end-or-beg &optional end-or-inside inside?)
  (b-operate-on-region-by-match end-or-beg end-or-inside inside? 'delete-region))

(defun b-kill-by-match (end-or-beg &optional end-or-inside inside?)
  (b-operate-on-region-by-match end-or-beg end-or-inside inside? 'kill-region))


(defun b-extract-by-regexp (end-or-beg &optional end-or-inside inside?))
(defun b-extract-by-regexp-with-properties (end-or-beg &optional end-or-inside inside?))
(defun b-delete-by-regexp (end-or-beg &optional end-or-inside inside?))
(defun b-kill-by-regexp (end-or-beg &optional end-or-inside inside?))

(provide 'b)
;;; b.el ends here
