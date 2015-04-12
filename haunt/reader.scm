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
;; Post readers.
;;
;;; Code:

(define-module (haunt reader)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:export (make-reader
            reader?
            reader-matcher
            reader-proc
            reader-match?
            read-post
            read-posts

            make-file-extension-matcher
            sxml-reader
            html-reader))

(define-record-type <reader>
  (make-reader matcher proc)
  reader?
  (matcher reader-matcher)
  (proc reader-proc))

(define (reader-match? reader file-name)
  "Return #t if FILE-NAME is a file supported by READER."
  ((reader-matcher reader) file-name))

(define* (read-post reader file-name #:optional (default-metadata '()))
  "Read a post object from FILE-NAME using READER, merging its
metadata with DEFAULT-METADATA."
  (let-values (((metadata sxml) ((reader-proc reader) file-name)))
    (make-post file-name
               (append metadata default-metadata)
               sxml)))

(define* (read-posts directory readers #:optional (default-metadata '()))
  "Read all of the files in DIRECTORY as post objects.  The READERS
list must contain a matching reader for every post."
  (define enter? (const #t))

  (define (leaf file-name stat memo)
    (let ((reader (find (cut reader-match? <> file-name) readers)))
      (if reader
          (cons (read-post reader file-name default-metadata) memo)
          (error "no reader available for post: " file-name))))

  (define (noop file-name stat result)
    result)

  (define (err file-name stat errno result)
    (error "file processing failed with errno: " file-name errno))

  (file-system-fold enter? leaf noop noop noop err '() directory))

;;;
;;; Simple readers
;;;

(define (make-file-extension-matcher ext)
  "Return a procedure that returns #t when a file name ends with
'.EXT'."
  (let ((regexp (make-regexp (string-append "\\." ext "$"))))
    (lambda (file-name)
      (regexp-match? (regexp-exec regexp file-name)))))

(define sxml-reader
  (make-reader (make-file-extension-matcher "sxml")
               (lambda (file-name)
                 (let ((contents (load (absolute-file-name file-name))))
                   (values (alist-delete 'content contents eq?)
                           (assq-ref contents 'content))))))

(define (read-html-post port)
  (let loop ((metadata '()))
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)
        (error "end of file while reading metadata: " (port-filename port)))
       ((string=? line "---")
        (values metadata `(raw ,(read-string port))))
       (else
        (match (map string-trim-both (string-split-at line #\:))
          (((= string->symbol key) value)
           (loop (alist-cons key (parse-metadata key value) metadata)))
          (_ (error "invalid metadata format: " line))))))))

(define html-reader
  (make-reader (make-file-extension-matcher "html")
               (cut call-with-input-file <> read-html-post)))
