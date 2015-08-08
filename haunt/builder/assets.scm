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
;; Static asset builder.
;;
;;; Code:

(define-module (haunt builder assets)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (haunt asset)
  #:use-module (haunt site)
  #:export (static-directory))

(define* (static-directory directory #:optional (dest directory))
  "Return a builder procedure that recursively copies all of the files
in DIRECTORY, a file names relative to a site's source directory, and
copies them into DEST, a prefix relative to a site's target output
directory.  By default, DEST is DIRECTORY."
  (lambda (site posts)
    (directory-assets directory (site-file-filter site) dest)))
