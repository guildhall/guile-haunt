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
;; Atom feed builder.
;;
;;; Code:

(define-module (haunt builder atom)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (haunt build html)
  #:export (atom-feed
            atom-feeds-by-tag))

(define (sxml->xml* sxml port)
  "Write SXML to PORT, preceded by an <?xml> tag."
  (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>" port)
  (sxml->xml sxml port))

(define (date->string* date)
  "Convert date to ISO-8601 formatted string."
  (date->string date "~4"))

(define (post->atom-entry post)
  "Convert POST into an Atom <entry> XML node."
  `(entry
    (title ,(post-ref post 'title))
    (author
     (name ,(post-ref post 'author))
     ,(let ((email (post-ref post 'email)))
        (if email `(email ,email) '())))
    (updated ,(date->string* (post-ref post 'date)))
    (link (@ (href ,(string-append "/" (post-slug post) ".html"))
             (rel "alternate")))
    (summary (@ (type "html"))
             ,(sxml->html-string (post-sxml post)))))

(define* (atom-feed #:key
                    (file-name "feed.xml")
                    (subtitle "Recent Posts")
                    (filter posts/reverse-chronological)
                    (max-entries 20))
  "Return a builder procedure that renders a list of posts as an Atom
feed.  All arguments are optional:

FILE-NAME: The page file name
SUBTITLE: The feed subtitle
FILTER: The procedure called to manipulate the posts list before rendering
MAX-ENTRIES: The maximum number of posts to render in the feed"
  (lambda (site posts)
    (make-page file-name
               `(feed (@ (xmlns "http://www.w3.org/2005/Atom"))
                      (title ,(site-title site))
                      (subtitle ,subtitle)
                      (updated ,(date->string* (current-date)))
                      (link (@ (href ,(string-append "/" file-name))
                               (rel "self")))
                      (link (@ (href ,(site-domain site))))
                      ,@(map post->atom-entry
                             (take-up-to max-entries (filter posts))))
               sxml->xml*)))

(define* (atom-feeds-by-tag #:key
                            (prefix "feeds/tags")
                            (filter posts/reverse-chronological)
                            (max-entries 20))
  "Return a builder procedure that renders an atom feed for every tag
used in a post.  All arguments are optional:

PREFIX: The directory in which to write the feeds
FILTER: The procedure called to manipulate the posts list before rendering
MAX-ENTRIES: The maximum number of posts to render in each feed"
  (lambda (site posts)
    (let ((tag-groups (posts/group-by-tag posts)))
      (map (match-lambda
            ((tag . posts)
             ((atom-feed #:file-name (string-append prefix "/" tag ".xml")
                         #:subtitle (string-append "Tag: " tag)
                         #:filter filter
                         #:max-entries max-entries)
              site posts)))
           tag-groups))))
