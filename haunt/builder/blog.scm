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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:export (theme
            theme?
            theme-name
            theme-layout
            theme-post-template
            theme-list-template

            blog))

(define-record-type <theme>
  (make-theme name layout post-template list-template)
  theme?
  (name theme-name)
  (layout theme-layout)
  (post-template theme-post-template)
  (list-template theme-list-template))

(define* (theme #:key
                (name "Untitled")
                layout
                post-template
                list-template)
  (make-theme name layout post-template list-template))

(define (with-layout theme site title body)
  ((theme-layout theme) site title body))

(define (render-post theme site post)
  (let ((title (post-ref post 'title))
        (body ((theme-post-template theme) post)))
    (with-layout theme site title body)))

(define (render-list theme site title posts prefix)
  (let ((body ((theme-list-template theme) site title posts prefix)))
    (with-layout theme site title body)))

(define (date->string* date)
  "Convert DATE to human readable string."
  (date->string date "~a ~d ~B ~Y"))

(define ugly-theme
  (theme #:name "Ugly"
         #:layout
         (lambda (site title body)
           `((doctype "html")
             (head
              (meta (@ (charset "utf-8")))
              (title ,(string-append title " — " (site-title site))))
             (body
              (h1 ,(site-title site))
              ,body)))
         #:post-template
         (lambda (post)
           `((h2 ,(post-ref post 'title))
             (h3 "by " ,(post-ref post 'author)
                 " — " ,(date->string* (post-date post)))
             (div ,(post-sxml post))))
         #:list-template
         (lambda (site title posts prefix)
           (define (post-uri post)
             (string-append "/" (or prefix "")
                            (site-post-slug site post) ".html"))

           `((h3 ,title)
             (ul
              ,@(map (lambda (post)
                       `(li
                         (a (@ (href ,(post-uri post)))
                            ,(post-ref post 'title)
                            " — "
                            ,(date->string* (post-date post)))))
                     posts))))))

(define* (blog #:key (theme ugly-theme) prefix)
  "Return a procedure that transforms a list of posts into pages
decorated by THEME, whose URLs start with PREFIX."
  (define (make-file-name base-name)
    (if prefix
        (string-append prefix "/" base-name)
        base-name))

  (lambda (site posts)
    (define (post->page post)
      (let ((base-name (string-append (site-post-slug site post)
                                      ".html")))
        (make-page (make-file-name base-name)
                   (render-post theme site post)
                   sxml->html)))

    (define index-page
      (make-page (make-file-name "index.html")
                 (render-list theme site "Recent Posts"
                              (posts/reverse-chronological posts)
                              prefix)
                 sxml->html))

    (cons index-page (map post->page posts))))
