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
             (haunt builder atom)
             (haunt builder assets))

(site #:title "Built with Guile"
      #:domain "example.com"
      #:default-metadata
      '((author . "Eva Luator")
        (email  . "eva@example.com"))
      #:readers (list sxml-reader html-reader)
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)
                       (static-directory "images"))
```

Usage
-----

Write a configuration file named `haunt.scm`.  Add your posts to a
directory named `posts`.  Then run `haunt build`!

To view your creation, run `haunt serve` and browse to
`localhost:8080`.

Requirements
------------

GNU Guile >= 2.0.11

Building from Git
-----------------

Haunt uses the familiar GNU build system.  GNU automake, autoconf, and
make are required to build from a git checkout.

```
./bootstrap && ./configure && make
```

Once Haunt is built, it can be run directly from the source tree (no
need to run `make install`) via the `pre-inst-env` script:

```
./pre-inst-env haunt --help
```

GNU Guix users can create a build environment with all necessary
dependencies using the handy `guix environment` tool:

```
guix environment -l package.scm
```

Examples
--------

An example Haunt site can be found in the `example` directory:

```
cd example
../pre-inst-env haunt build
```

License
-------

GNU GPLv3 or later
