dnl @synopsis AC_CHECK_PACKAGE(PACKAGE, FUNCTION, LIBRARY, HEADERFILE [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl
dnl Provides --with-PACKAGE, --with-PACKAGE-include and --with-PACKAGE-libdir
dnl options to configure. Supports the now standard --with-PACKAGE=DIR
dnl approach where the package's include dir and lib dir are underneath DIR,
dnl but also allows the include and lib directories to be specified seperately
dnl
dnl adds the extra -Ipath to CPPFLAGS if needed
dnl adds extra -Lpath and -Rpath to LD_FLAGS if needed
dnl searches for the FUNCTION in the LIBRARY with
dnl AC_CHECK_LIBRARY and thus adds the lib to LIBS
dnl
dnl defines HAVE_PKG_PACKAGE if it is found, (where PACKAGE in the
dnl HAVE_PKG_PACKAGE is replaced with the actual first parameter passed)
dnl note that autoheader will complain of not having the HAVE_PKG_PACKAGE and you
dnl will have to add it to acconfig.h manually
dnl
dnl @version $Id: ac_check_package.m4,v 3.5 2003/05/11 18:54:32 pfkeb Exp $
dnl @author Paul_Kunz@slac.stanford.edu modified ac_caolan_check_package.m4 for all languages
dnl
AC_DEFUN([AC_CHECK_PACKAGE],
[

AC_ARG_WITH($1,
[  --with-$1[=DIR]      root directory of $1 installation],
with_$1=$withval
if test "${with_$1}" != yes; then
        $1_include="$withval/include"
        $1_libdir="$withval/lib"
fi,
$1_include="/usr/local/cfitsio/include"
$1_libdir="/usr/local/cfitsio/lib"
)

AC_ARG_WITH($1-include,
[  --with-$1-include=DIR        specify exact include dir for $1 headers],
$1_include="$withval")

AC_ARG_WITH($1-libdir,
[  --with-$1-libdir=DIR        specify exact library dir for $1 library
  --without-$1        disables $1 usage completely],
$1_libdir="$withval")

if test "${with_$1}" != no ; then
        OLD_LIBS=$LIBS
        OLD_LDFLAGS=$LDFLAGS
        OLD_CPPFLAGS=$CPPFLAGS

        if test "${$1_libdir}" ; then
                LDFLAGS="$LDFLAGS -L${$1_libdir}"
                RDFLAGS="${$1_libdir}"
        fi
        if test "${$1_include}" ; then
                CPPFLAGS="$CPPFLAGS -I${$1_include}"
        fi

        AC_CHECK_LIB($3,$2,,no_good=yes)
        AC_CHECK_HEADER($4,,no_good=yes)
        if test "$no_good" = yes; then
dnl     broken
                ifelse([$6], , , [$6])

                LIBS=$OLD_LIBS
                LDFLAGS=$OLD_LDFLAGS
                CPPFLAGS=$OLD_CPPFLAGS
        else
dnl     fixed
                ifelse([$5], , , [$5])

                AC_DEFINE(HAVE_PKG_$1, 1, Define if you have the package )
                AC_SUBST(RDFLAGS)
        fi

fi

])
