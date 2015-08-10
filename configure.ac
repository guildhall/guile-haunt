dnl -*- Autoconf -*-

AC_INIT(Haunt, 0.1)
AC_CONFIG_SRCDIR(haunt)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile example/Makefile website/Makefile haunt/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/haunt], [chmod +x scripts/haunt])

GUILE_PROGS([2.0.11])

AC_OUTPUT
