(use-modules (haunt site)
             (haunt reader)
             (haunt builder blog)
             (haunt builder atom)
             (srfi srfi-19))

(site #:title "Built with Guile"
      #:domain "dthompson.us"
      #:default-metadata
      '((author . "David Thompson")
        (email  . "davet@gnu.org"))
      #:readers (list sxml-reader html-reader)
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)))
