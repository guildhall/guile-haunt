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
;; Static asset data type.
;;
;;; Code:

(define-module (haunt asset)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (haunt utils)
  #:export (make-asset
            asset?
            asset-source
            asset-target
            install-asset
            directory-assets))

(define-record-type <asset>
  (make-asset source target)
  asset?
  (source asset-source)
  (target asset-target))

(define (install-asset asset prefix)
  "Install ASSET source file into destination directory within
PREFIX."
  (match asset
    (($ <asset> source target)
     (let ((target* (string-append prefix "/" target)))
       (mkdir-p (dirname target*))
       (copy-file source target*)))))

(define (directory-assets directory dest)
  "Create a list of asset objects to be stored within DEST for all
files in DIRECTORY, recursively."
  (define enter? (const #t))

  ;; In order to do accurate file name manipulation, every file name
  ;; is converted into a list of components, manipulated, then
  ;; converted back into a string.
  (define leaf
    (let ((base-length (length (file-name-components directory)))
          (dest* (file-name-components dest)))
      (lambda (file-name stat memo)
        (let* ((file-name* (file-name-components file-name))
               (target (join-file-name-components
                        (append dest* (drop file-name* base-length)))))
          (cons (make-asset file-name target) memo)))))

  (define (noop file-name stat memo) memo)

  (define (err file-name stat errno memo)
    (error "asset processing failed with errno: " file-name errno))

  (file-system-fold enter? leaf noop noop noop err '() directory))
