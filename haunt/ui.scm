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
;;; WITnnnHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Haunt user interface.
;;
;;; Code:

(define-module (haunt ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (haunt config)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:export (program-name
            show-version-and-exit
            simple-args-fold
            %common-options
            %default-common-options
            show-common-options-help
            leave
            string->number*
            load-config
            option?
            haunt-main))

(define commands
  '("build" "serve"))

(define program-name (make-parameter 'haunt))

(define (show-haunt-help)
  (format #t "Usage: haunt COMMAND ARGS...
Run COMMAND with ARGS.~%~%")
  (format #t "COMMAND must be one of the sub-commands listed below:~%~%")
  (format #t "~{   ~a~%~}" (sort commands string<?)))

(define (show-haunt-usage)
  (format #t "Try `haunt --help' for more information.~%")
  (exit 1))

(define (show-version-and-exit name)
  (format #t "~a ~a
Copyright (C) 2015 the Haunt authors
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.~%"
          name %haunt-version)
  (exit 0))

(define (leave format-string . args)
  "Display error message and exist."
  (apply format (current-error-port) format-string args)
  (newline)
  (exit 1))

(define (string->number* str)
  "Like `string->number', but error out with an error message on failure."
  (or (string->number str)
      (leave "~a: invalid number" str)))

(define (simple-args-fold args options default-options)
  (args-fold args options
             (lambda (opt name arg result)
               (leave "~A: unrecognized option" name))
             (lambda (arg result)
               (leave "~A: extraneuous argument" arg))
             default-options))

(define %common-options
  (list (option '(#\c "config") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'config arg result)))))

(define %default-common-options
  '((config . "haunt.scm")))

(define (show-common-options-help)
  (display "
  -c, --config           configuration file to load"))

(define (option? str)
  (string-prefix? "-" str))

(define* (load-config file-name)
  "Load configuration from FILE-NAME."
  (if (file-exists? file-name)
      (let ((obj (load (absolute-file-name file-name))))
        (if (site? obj)
            obj
            (leave "configuration object must be a site, got: ~a" obj)))
      (leave "configuration file not found: ~a" file-name)))

(define (run-haunt-command command . args)
  (let* ((module
          (catch 'misc-error
            (lambda ()
              (resolve-interface `(haunt ui ,command)))
            (lambda -
              (format (current-error-port) "~a: invalid subcommand~%" command)
              (show-haunt-usage))))
         (command-main (module-ref module (symbol-append 'haunt- command))))
    (parameterize ((program-name command))
      (apply command-main args))))

(define* (haunt-main arg0 . args)
  (setlocale LC_ALL "")
  (match args
    (()
     (show-haunt-usage))
    ((or ("-h") ("--help"))
     (show-haunt-help))
    (("--version")
     (show-version-and-exit "haunt"))
    (((? option? opt) _ ...)
     (format (current-error-port)
             "haunt: unrecognized option '~a'~%"
             opt)
     (show-haunt-usage))
    ((command args ...)
     (apply run-haunt-command (string->symbol command) args))))
