#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.13])
AC_INIT([guile-bytestructure-class], [0.2.0], [z572@z572.online])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([Makefile])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AM_SILENT_RULES([yes])
# Checks for programs.

# Checks for libraries.
GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR

PKG_CHECK_MODULES([GUILE], [guile-3.0])
GUILE_MODULE_REQUIRED([bytestructures guile])
# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
