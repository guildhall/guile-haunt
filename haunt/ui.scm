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
;; Haunt user interface.
;;
;;; Code:

(define-module (haunt ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (haunt-main))

(define (show-haunt-help)
  (format #t "Usage: haunt COMMAND ARGS...
Run COMMAND with ARGS.~%~%"))

(define (show-haunt-usage)
  (format #t "Try `haunt --help' for more information.~%")
  (exit 1))

(define (option? str)
  (string-prefix? "-" str))

(define* (haunt-main arg0 . args)
  (match args
    (()
     (show-haunt-usage))
    ((or ("-h") ("--help"))
     (show-haunt-help))
    (((? option? opt) _ ...)
     (format (current-error-port)
             "haunt: unrecognized option '~a'~%"
             opt)
     (show-haunt-usage))))
