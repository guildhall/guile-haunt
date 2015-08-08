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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (haunt utils)
  #:use-module (haunt reader)
  #:use-module (haunt page)
  #:use-module (haunt post)
  #:use-module (haunt asset)
  #:export (site
            site?
            site-title
            site-domain
            site-posts-directory
            site-file-filter
            site-build-directory
            site-default-metadata
            site-make-slug
            site-readers
            site-builders
            site-post-slug
            build-site

            make-file-filter
            default-file-filter))

(define-record-type <site>
  (make-site title domain posts-directory file-filter build-directory
             default-metadata make-slug readers builders)
  site?
  (title site-title)
  (domain site-domain)
  (posts-directory site-posts-directory)
  (file-filter site-file-filter)
  (build-directory site-build-directory)
  (default-metadata site-default-metadata)
  (make-slug site-make-slug)
  (readers site-readers)
  (builders site-builders))

(define* (site #:key
               (title "This Place is Haunted")
               (domain "example.com")
               (posts-directory "posts")
               (file-filter default-file-filter)
               (build-directory "site")
               (default-metadata '())
               (make-slug post-slug)
               (readers '())
               (builders '()))
  "Create a new site object.  All arguments are optional:

TITLE: The name of the site
POSTS-DIRECTORY: The directory where posts are found
FILE-FILTER: A predicate procedure that returns #f when a post file
should be ignored, and #f otherwise.  Emacs temp files are ignored by
default.
BUILD-DIRECTORY: The directory that generated pages are stored in
DEFAULT-METADATA: An alist of arbitrary default metadata for posts
whose keys are symbols
MAKE-SLUG: A procedure generating a file name slug from a post
READERS: A list of reader objects for processing posts
BUILDERS: A list of procedures for building pages from posts"
  (make-site title domain posts-directory file-filter build-directory
             default-metadata make-slug readers builders))

(define (site-post-slug site post)
  "Return a slug string for POST using the slug generator for SITE."
  ((site-make-slug site) post))

(define (build-site site)
  "Build SITE in the appropriate build directory."
  (let ((posts (if (file-exists? (site-posts-directory site))
                   (read-posts (site-posts-directory site)
                               (site-file-filter site)
                               (site-readers site)
                               (site-default-metadata site))
                   '()))
        (build-dir (absolute-file-name (site-build-directory site))))
    (when (file-exists? build-dir)
      (delete-file-recursively build-dir)
      (mkdir build-dir))
    (for-each (match-lambda
               ((? page? page)
                (format #t "writing page '~a'~%" (page-file-name page))
                (write-page page build-dir))
               ((? asset? asset)
                (format #t "copying asset '~a' → '~a'~%"
                        (asset-source asset)
                        (asset-target asset))
                (install-asset asset build-dir))
               (obj
                (error "unrecognized site object: " obj)))
              (flat-map (cut <> site posts) (site-builders site)))))

(define (make-file-filter patterns)
  (let ((patterns (map make-regexp patterns)))
    (lambda (file-name)
      (not (any (lambda (regexp)
                  (regexp-match?
                   (regexp-exec regexp (basename file-name))))
                patterns)))))

;; Filter out Emacs temporary files by default.
(define default-file-filter
  (make-file-filter '("^\\." "^#")))
