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
;; Page builders
;;
;;; Code:

(define-module (haunt builder blog)
  #:use-module (srfi srfi-19)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (haunt build html)
  #:export (blog))

(define (ugly-theme site post)
  "Render POST on SITE with an unstyled, barebones theme."
  `((doctype "html")
    (head
     (title ,(string-append (post-ref post 'title)
                            " — "
                            (site-title site))))
    (body
     (h1 ,(post-ref post 'title))
     (h3 ,(post-ref post 'author))
     (div ,(post-sxml post)))))

(define* (blog #:key (theme ugly-theme) prefix)
  "Return a procedure that transforms a list of posts into pages
decorated by THEME, whose URLs start with PREFIX."
  (define (make-file-name base-name)
    (if prefix
        (string-append prefix "/" base-name)
        base-name))

  (define (post-uri post)
    (string-append "/" (or prefix "") (post-slug post) ".html"))

  (define (post->recent-post-entry post)
    `(li
      (a (@ (href ,(post-uri post)))
         ,(post-ref post 'title))))

  (lambda (site posts)
    (define (post->page post)
      (let ((base-name (string-append (post-slug post) ".html")))
        (make-page (make-file-name base-name)
                   (theme site post)
                   sxml->html)))

    (define index-page
      (make-page (make-file-name "index.html")
                 `((doctype "html")
                   (head
                    (title ,(site-title site)))
                   (body
                    (h1 ,(site-title site))
                    (h3 "Recent Posts")
                    (ul ,@(map post->recent-post-entry
                               (posts/reverse-chronological posts)))))
                 sxml->html))

    (cons index-page (map post->page posts))))
