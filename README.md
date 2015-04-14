Haunt
=====

About
-----

Haunt is a static site generator written in Guile Scheme.  It's
simple, functional, and extensible.

Features
--------

* Easy blog and Atom feed generation
* Supports any markup language that can be parsed to SXML
* Simple development server
* Purely functional build process
* User extensible

Example Configuration
---------------------

```
(use-modules (haunt site)
             (haunt reader)
             (haunt builder blog)
             (haunt builder atom))

(site #:title "Built with Guile"
      #:domain "dthompson.us"
      #:default-metadata
      '((author . "David Thompson")
        (email  . "davet@gnu.org"))
      #:readers (list sxml-reader html-reader)
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)))
```

Usage
-----

Write a configuration file named `haunt.scm`.  Add your posts to a
directory named `posts`.  Then run `haunt build`!

To view your creation, run `haunt serve` and browse to
`localhost:8080`.

License
-------

GNU GPLv3 or later
