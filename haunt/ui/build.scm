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
;; Haunt build sub-command.
;;
;;; Code:

(define-module (haunt ui build)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (haunt site)
  #:use-module (haunt config)
  #:use-module (haunt ui)
  #:export (haunt-build))

(define (show-help)
  (format #t "Usage: haunt build [OPTION]
Compile the site defined in the current directory.~%")
  (show-common-options-help)
  (newline)
  (display "
  -h, --help             display this help and exit")
  (display "
  --version              display version information and exit")
  (newline))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda _
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda _
                   (show-version-and-exit "haunt build")))
         %common-options))

(define %default-options %default-common-options)

(define (haunt-build . args)
  (let* ((opts (simple-args-fold args %options %default-options))
         (site (load-config (assq-ref opts 'config))))
    (format #t "building pages in '~a'...~%" (site-build-directory site))
    (build-site site)
    (display "build completed successfully\n")))
