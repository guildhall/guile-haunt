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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (haunt site)
  #:use-module (haunt config)
  #:use-module (haunt ui)
  #:use-module (haunt serve web-server)
  #:export (haunt-serve))

(define (show-help)
  (format #t "Usage: haunt serve [OPTION]
Start an HTTP server for the current site.~%")
  (display "
  -p, --port             port to listen on")
  (newline)
  (show-common-options-help)
  (newline)
  (display "
  -h, --help             display this help and exit")
  (display "
  -V, --version          display version and exit")
  (newline))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda _
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda _
                   (show-version-and-exit "haunt serve")))
         (option '(#\p "port") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'port (string->number* arg) result)))
         %common-options))

(define %default-options
  (cons '(port . 8080)
        %default-common-options))

(define (haunt-serve . args)
  (let* ((opts (simple-args-fold args %options %default-options))
         (port (assq-ref opts 'port))
         (site (load-config (assq-ref opts 'config)))
         (doc-root (site-build-directory site)))
    (format #t "serving ~a on port ~d~%" doc-root port)
    (serve doc-root)))
