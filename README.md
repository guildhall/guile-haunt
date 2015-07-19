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
      #:domain "example.com"
      #:default-metadata
      '((author . "Eva Luator")
        (email  . "eva@example.com"))
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

Requirements
------------

GNU Guile >= 2.0.9

Building
--------

Haunt uses the familiar GNU build system.  GNU automake, autoconf, and
make are required to build from source.

```
./bootstrap && ./configure && make
```

License
-------

GNU GPLv3 or later
