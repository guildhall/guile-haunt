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
;; Post data type.
;;
;;; Code:

(define-module (haunt post)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-post
            post?
            post-file-name
            post-sxml
            post-metadata
            post-ref
            post-slug
            posts/reverse-chronological))

(define-record-type <post>
  (make-post file-name metadata sxml)
  post?
  (file-name post-file-name)
  (metadata post-metadata)
  (sxml post-sxml))

(define (post-ref post key)
  "Return the metadata corresponding to KEY within POST."
  (assq-ref (post-metadata post) key))

(define (post-slug post)
  "Transform the title of POST into a URL slug."
  (string-join (map (lambda (s)
                      (string-filter char-set:letter+digit s))
                    (string-split (string-downcase (post-ref post 'title))
                                  char-set:whitespace))
               "-"))

(define (post->time post)
  (date->time-utc (post-ref post 'date)))

(define (posts/reverse-chronological posts)
  "Returns POSTS sorted in reverse chronological order."
  (sort posts
        (lambda (a b)
          (time>? (post->time a) (post->time b)))))
