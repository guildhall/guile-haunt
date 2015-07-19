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
;; Development environment for GNU Guix.
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile))

(package
  (name "haunt")
  (version "0.0")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)))
  (inputs
   `(("guile" ,guile-2.0)))
  (synopsis "Functional, composable static site generator")
  (description "Haunt is a static site generator written in Guile
Scheme.  Haunt features a functional build system and an extensible
interface for reading site content in any format.")
  (home-page "https://git.dthompson.us/haunt.git")
  (license gpl3+))
