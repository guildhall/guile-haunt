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
;; Haunt configuration.
;;
;;; Code:

(define-module (haunt config)
  #:export (%haunt-cwd
            haunt-file-name
            haunt-output-directory))

(define %haunt-cwd (getcwd))

(define (haunt-file-name rel)
  "Return an absolute file name to the file REL in the haunt current
working directory."
  (string-append %haunt-cwd "/" rel))

(define (haunt-output-directory)
  "Return the current haunt compiled page output directory."
  (haunt-file-name "output"))
