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

(use-modules (haunt site)
             (haunt reader)
             (haunt asset)
             (haunt page)
             (haunt post)
             (haunt html)
             (haunt utils)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (srfi srfi-19)
             (ice-9 rdelim)
             (ice-9 match)
             (web uri))

(define %releases
  '(("0.1" "1f751f234e382ed0f13d79e4c106373e9ae55c23")))

(define (tarball-url version)
  (string-append "http://files.dthompson.us/haunt/haunt-"
                 version ".tar.gz"))

(define %download-button
  (match %releases
    (((version sha1) . _)
     `(a (@ (class "btn btn-primary btn-lg")
            (role "button")
            (href ,(tarball-url version)))
         "Download Haunt " ,version))))

(define (stylesheet name)
  `(link (@ (rel "stylesheet")
            (href ,(string-append "/css/" name ".css")))))

(define (anchor content uri)
  `(a (@ (href ,uri)) ,content))

(define (logo src)
  `(img (@ (class "logo") (src ,(string-append "/images/" src)))))

(define (jumbotron content)
  `(div (@ (class "jumbotron"))
        (div (@ (class "row"))
             (div (@ (class "column-logo"))
                  (img (@ (class "big-logo")
                          (src "/images/haunt.png"))))
             (div (@ (class "column-info")) ,content))))

(define %cc-by-sa-link
  '(a (@ (href "https://creativecommons.org/licenses/by-sa/4.0/"))
      "Creative Commons Attribution Share-Alike 4.0 International"))

(define haunt-theme
  (theme #:name "Haunt"
         #:layout
         (lambda (site title body)
           `((doctype "html")
             (head
              (meta (@ (charset "utf-8")))
              (title ,(string-append title " — " (site-title site)))
              ,(stylesheet "reset")
              ,(stylesheet "main"))
             (body
              (header (@ (class "navbar"))
                      (div (@ (class "container"))
                           (ul
                            (li ,(anchor "home" "/"))
                            (li ,(anchor "downloads" "/downloads.html"))
                            (li ,(anchor "git"
                                         "https://git.dthompson.us/haunt.git")))))
              (div (@ (class "container"))
                   ,body
                   (footer (@ (class "text-center"))
                    (p (small "Copyright © 2015 David Thompson"))
                    (p
                     (small "The text and images on this site are free
culture works available under the " ,%cc-by-sa-link " license.")))))))
         #:post-template
         (lambda (post)
           `((h2 ,(post-ref post 'title))
             (h3 "by " ,(post-ref post 'author)
                 " — " ,(date->string* (post-date post)))
             (div ,(post-sxml post))))
         #:collection-template
         (lambda (site title posts prefix)
           (define (post-uri post)
             (string-append "/" (or prefix "")
                            (site-post-slug site post) ".html"))

           `(,(jumbotron
               `((p "Haunt is a simple, functional, hackable static site
generator written in Guile Scheme that gives authors the ability to
treat websites as programs.")
                 ,%download-button))

             (p "Haunt isn't your average static site generator.  Its
mission is to give authors the full expressive power of Scheme to
define every aspect of their websites are generated.  Haunt uses a
simple, functional build system that allows any type of web page to be
built by writing procedures that return page objects.")
             (p "Haunt has no opinion about what markup language
authors should use to write posts.  Just write the relevant reader
procedure and Haunt will happily work with that format.  Likewise,
Haunt has no opinion about how authors structure their sites.  Haunt
ships with helpful builder procedures that generate simple blogs or
Atom feeds, but authors should feel empowered to tweak them, write
replacements, or add new builders to do things that the Haunt hackers
didn't think of.")
             (p "Here's what a simple Haunt configuration looks
like:")
             (pre
              ,(call-with-input-file "../example/haunt.scm" read-string))

             (p "With the above saved into a file named "
                (code "haunt.scm")
                " and a "
                (code "posts")
                " directory populated with the articles to publish,
the site can be built by running "
                (code "haunt build")
                ".  Once the site is built, running "
                (code "haunt serve")
                " and visiting "
                (code "localhost:8080")
                " in a web browser will show the results of the build
without needing to upload the generated files to a web server.")

             (h2 "News")
             (ul
              ,@(map (lambda (post)
                       `(li
                         (a (@ (href ,(post-uri post)))
                            ,(post-ref post 'title)
                            " — "
                            ,(date->string* (post-date post)))))
                     (posts/reverse-chronological posts)))

             (h2 "Contributing")
             (p "Patches to fix bugs or add new functionality are
highly encouraged.  In lieu of a mailing list, please send patches
to "
                (code "davet") " at " (code "gnu") " dot " (code "org")
                " for now.")
             (p "To get the latest version of the source code, clone
the official git repository:")
             (pre "git clone git://dthompson.us/haunt.git")))))

(define (downloads-page site posts)
  (define body
    `(,(jumbotron
        `(,%download-button
          (p (small "SHA1 checksum: "
                    ,(match %releases (((_ sha1) . _) sha1))))))
      (h2 "Downloads")
      (table (@ (class "table"))
       (thead
        (tr (th "Source") (th "SHA1")))
       (tbody
        ,(map (match-lambda
               ((version sha1)
                `(tr
                  (td (a (@ (href ,(tarball-url version)))
                         ,(string-append "haunt-" version ".tar.gz")))
                  (td ,sha1))))
              %releases)))))

  (make-page "downloads.html"
             (with-layout haunt-theme site "Downloads" body)
             sxml->html))

(define %collections
  `(("Home" "index.html" ,posts/reverse-chronological)))

(site #:title "Haunt"
      #:domain "dthompson.us"
      #:default-metadata
      '((author . "David Thompson")
        (email  . "davet@gnu.org"))
      #:readers (list sxml-reader html-reader)
      #:builders (list (blog #:theme haunt-theme #:collections %collections)
                       (atom-feed)
                       (atom-feeds-by-tag)
                       downloads-page
                       (static-directory "images")
                       (static-directory "css")))
