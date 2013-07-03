# check for one particular file of the sources
AC_INIT([higher-order-tools],[0.0.1],[jakobro@informatik.uni-freiburg.de])

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

AC_BASE_CHECKS()

# Include a config.h
AC_CONFIG_HEADERS([config.h:config.h.in])
# Include it
CFLAGS="$CFLAGS -I.. -DHAVE_CONFIG_H"

OCAMLDOCFLAGS="$OCAMLDOCFLAGS -v"

AC_CHECK_HEADERS([errno.h])

# Check for libfaad
##FAAC_LIBS="-lfaad -lm"
##FAAC_CFLAGS=""
##FAAC_CPPFLAGS=""
##ac_save_CPPFLAGS="$CPPFLAGS"
##ac_save_CFLAGS="$CFLAGS"
##ac_save_LIBS="$LIBS"
##CPPFLAGS="$CPPFLAGS $FAAC_CPPFLAGS"
##CFLAGS="$CFLAGS $FAAC_CFLAGS"
##LIBS="$FAAC_LIBS $LIBS"
##AC_CHECK_HEADERS([neaacdec.h], [
##                  AC_CHECK_FUNC([NeAACDecInit], [ac_have_faad="yes"])
##])
##CPPFLAGS="$ac_save_CPPFLAGS"
##CFLAGS="$ac_save_CFLAGS"
##LIBS="$ac_save_LIBS"
##if test "$ac_have_faad" != "yes"; then
##  AC_MSG_ERROR([Cannot find libfaad.])
##fi
##CPPFLAGS="$CPPFLAGS $FAAC_CPPFLAGS"
##CFLAGS="$CFLAGS $FAAC_CFLAGS"
##LIBS="$LIBS $FAAC_LIBS"


# substitutions to perform
AC_SUBST(VERSION)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([VERSION],[chmod a-w VERSION])
AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([test/Makefile],[chmod a-w test/Makefile])
AC_OUTPUT
