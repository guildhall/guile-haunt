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
;; Site configuration data type.
;;
;;; Code:

(define-module (haunt site)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (haunt utils)
  #:use-module (haunt reader)
  #:use-module (haunt page)
  #:export (site
            site?
            site-title
            site-domain
            site-posts-directory
            site-build-directory
            site-default-metadata
            site-readers
            site-builders
            build-site))

(define-record-type <site>
  (make-site title domain posts-directory build-directory
             default-metadata readers builders)
  site?
  (title site-title)
  (domain site-domain)
  (posts-directory site-posts-directory)
  (build-directory site-build-directory)
  (default-metadata site-default-metadata)
  (readers site-readers)
  (builders site-builders))

(define* (site #:key
               (title "This Place is Haunted")
               (domain "example.com")
               (posts-directory "posts")
               (build-directory "site")
               (default-metadata '())
               (readers '())
               (builders '()))
  "Create a new site object.  All arguments are optional:

TITLE: The name of the site
POSTS-DIRECTORY: The directory where posts are found
BUILD-DIRECTORY: The directory that generated pages are stored in
DEFAULT-METADATA: An alist of arbitrary default metadata for posts
whose keys are symbols
READERS: A list of reader objects for processing posts
BUILDERS: A list of procedures for building pages from posts"
  (make-site title domain posts-directory build-directory
             default-metadata readers builders))

(define (build-site site)
  "Build SITE in the appropriate build directory."
  (let ((posts (read-posts (site-posts-directory site)
                           (site-readers site)
                           (site-default-metadata site)))
        (build-dir (absolute-file-name (site-build-directory site))))
    (delete-file-recursively build-dir)
    (for-each (lambda (page)
                (format #t "writing '~a'~%" (page-file-name page))
                (write-page page build-dir))
              (flat-map (cut <> site posts) (site-builders site)))))
