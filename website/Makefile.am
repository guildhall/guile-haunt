## Haunt --- Static site generator for GNU Guile
## Copyright © 2015 David Thompson <davet@gnu.org>
##
## This file is part of Haunt.
##
## Haunt is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Haunt is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

dist_noinst_DATA =			        \
  haunt.scm					\
  css/main.css					\
  css/reset.css					\
  images/haunt.png				\
  js/piwik.js					\
  posts/0.1-release.sxml

publish:
	rsync -P -rvz --delete site/ blog@dthompson.us:/var/www/haunt --cvs-exclude
