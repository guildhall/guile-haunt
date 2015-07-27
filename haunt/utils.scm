;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
;; Miscellaneous utility procedures.
;;
;;; Code:

(define-module (haunt utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:export (flatten
            flat-map
            string-split-at
            file-name-components
            join-file-name-components
            absolute-file-name
            delete-file-recursively
            mkdir-p
            string->date*
            take-up-to))

(define* (flatten lst #:optional depth)
  "Return a list that recursively concatenates the sub-lists of LST,
up to DEPTH levels deep.  When DEPTH is #f, the entire tree is
flattened."
  (if (and (number? depth) (zero? depth))
      lst
      (fold-right (match-lambda*
                   (((sub-list ...) memo)
                    (append (flatten sub-list (and depth (1- depth)))
                            memo))
                   ((elem memo)
                    (cons elem memo)))
                  '()
                  lst)))

(define (flat-map proc . lsts)
  (flatten (apply map proc lsts) 1))

(define (string-split-at str char-pred)
  (let ((i (string-index str char-pred)))
    (if i
        (list (string-take str i)
              (string-drop str (1+ i)))
        (list str))))

(define (file-name-components file-name)
  "Split FILE-NAME into the components delimited by '/'."
  (if (string-null? file-name)
      '()
      (string-split file-name #\/)))

(define (join-file-name-components components)
  "Join COMPONENTS into a file name string."
  (string-join components "/"))

(define (absolute-file-name file-name)
  (if (absolute-file-name? file-name)
      file-name
      (string-append (getcwd) "/" file-name)))

;; Written by Ludovic Courtès for GNU Guix.
(define* (delete-file-recursively dir
                                  #:key follow-mounts?)
  "Delete DIR recursively, like `rm -rf', without following symlinks.  Don't
follow mount points either, unless FOLLOW-MOUNTS? is true.  Report but ignore
errors."
  (let ((dev (stat:dev (lstat dir))))
    (file-system-fold (lambda (dir stat result)    ; enter?
                        (or follow-mounts?
                            (= dev (stat:dev stat))))
                      (lambda (file stat result)   ; leaf
                        (delete-file file))
                      (const #t)                   ; down
                      (lambda (dir stat result)    ; up
                        (rmdir dir))
                      (const #t)                   ; skip
                      (lambda (file stat errno result)
                        (format (current-error-port)
                                "warning: failed to delete ~a: ~a~%"
                                file (strerror errno)))
                      #t
                      dir

                      ;; Don't follow symlinks.
                      lstat)))

;; Written by Ludovic Courtès for GNU Guix.
(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define (string->date* str)
  "Convert STR, a string in '~Y~m~d ~H:~M' format, into a SRFI-19 date
object."
  (string->date str "~Y~m~d ~H:~M"))

(define (take-up-to n lst)
  "Return the first N elements of LST or an equivalent list if there
are fewer than N elements."
  (if (zero? n)
      '()
      (match lst
        (() '())
        ((head . tail)
         (cons head (take-up-to (1- n) tail))))))
