;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of Haunt.
;;;
;;; Haunt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Haunt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Miscellaneous utility procedures.
;;
;;; Code:

(define-module (haunt utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (flatten
            flat-map
            string-split-at))

(define* (flatten lst #:optional depth)
  "Return a list that recursively concatenates the sub-lists of LST,
up to DEPTH levels deep.  When DEPTH is #f, the entire tree is
flattened."
  (if (and (number? depth) (zero? depth))
      lst
      (fold-right (match-lambda*
                   (((sub-list ...) memo)
                    (append (flatten sub-list (and depth (1- depth)))
                            memo))
                   ((elem memo)
                    (cons elem memo)))
                  '()
                  lst)))

(define (flat-map proc . lsts)
  (flatten (apply map proc lsts) 1))

(define (string-split-at str char-pred)
  (let ((i (string-index str char-pred)))
    (if i
        (list (string-take str i)
              (string-drop str (1+ i)))
        (list str))))

