dnl Process this file with autoconf to produce a configure script.
AC_INIT([chez-a-sync], [0.1])

dnl Require autoconf 2.59c or greater for docdir and htmldir
AC_PREREQ(2.59c)

dnl Set so_version info for libtool (set with -version-info in lib/Makefile.am)
dnl CURRENT : REVISION : AGE where
dnl CURRENT =  The most recent interface number that this library implements.
dnl REVISION = The implementation number of the current interface.
dnl AGE =      The difference between the newest and oldest interface numbers
dnl            that this library implements (ie a range so that AGE must be
dnl            less than or equal to CURRENT)
dnl
dnl +1 : ? : +1  == new interface that does not break old one
dnl +1 : ? : 0   == new interface that breaks old one
dnl  ? : ? : 0   == no new interfaces, but breaks apps
dnl  ? :+1 : ?   == just some internal changes, nothing breaks (eg bump with
dnl                 minor/micro library version number changes changing
dnl                 compiled code but no new interface)
CHEZ_A_SYNC_SO_VERSION=0:0:0
AC_SUBST(CHEZ_A_SYNC_SO_VERSION)

AM_INIT_AUTOMAKE([1.11])

AC_CONFIG_HEADERS([config.h])

dnl use '--disable-silent-rules' or 'make V=1 ...' to override this
AM_SILENT_RULES([yes])

dnl check for working c compiler
AC_CHECK_COMPILER

dnl set up libtool
AC_PROG_LIBTOOL
AC_CONFIG_MACRO_DIR([m4])

dnl define MAKE for subdirectories
AC_PROG_MAKE_SET

AC_CONFIG_FILES([ Makefile lib/Makefile a-sync/Makefile a-sync/helper/Makefile \
		  docs/Makefile tests/Makefile chez-a-sync.pc ])
AC_OUTPUT
