(use-modules (haunt site)
             (haunt reader)
             (haunt asset)
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
                       (static-directory "images")))
