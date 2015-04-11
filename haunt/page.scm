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
;; Page data type.
;;
;;; Code:

(define-module (haunt page)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (haunt build html)
  #:export (make-page
            page?
            page-file-name
            page-contents
            page-writer
            write-page))

(define-record-type <page>
  (make-page file-name contents writer)
  page?
  (file-name page-file-name)
  (contents page-contents)
  (writer page-writer))

(define (write-page page output-directory)
  "Write PAGE to OUTPUT-DIRECTORY."
  (match page
    (($ <page> file-name contents writer)
     (let ((output (string-append output-directory "/" file-name)))
       (call-with-output-file output (cut writer contents <>))))))
