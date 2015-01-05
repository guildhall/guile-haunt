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
;; Haunt serve sub-command.
;;
;;; Code:

(define-module (haunt ui serve)
  #:use-module (ice-9 match)
  #:use-module (haunt config)
  #:use-module (haunt ui)
  #:use-module (haunt serve web-server)
  #:export (haunt-serve))

(define (show-serve-help)
  (format #t "Usage: haunt serve [OPTION]
Start an HTTP server for the current site.~%")
  (display "
  -h, --help             display this help and exit")
  (newline))

(define haunt-serve
  (match-lambda*
   (() (serve (haunt-output-directory)))
   ((or ("-h") ("--help"))
    (show-serve-help))
   (("--version")
    (show-version-and-exit))
   (((? option? opt) _ ...)
    (haunt-error "invalid option: ~a" opt)
    (exit 1))
   ((arg _ ...)
    (haunt-error "invalid argument: ~a" arg)
    (exit 1))))
