AC_PREREQ(2.50)

AC_INIT(src/Makevars.in)

## Tcl/Tk.
AC_ARG_WITH([tcltk],
[AC_HELP_STRING([--with-tcltk],
                [use Tcl/Tk, or specify its library dir @<:@yes@:>@])],
[if test "${withval}" = no; then
  want_tcltk=no
elif test "${withval}" = yes; then
  want_tcltk=yes
else
  want_tcltk=yes
  LDFLAGS="${LDFLAGS} -L${withval}"
  tcltk_prefix="${withval}"
fi],
[want_tcltk=yes])
AC_ARG_WITH([tcl-config],
[AC_HELP_STRING([--with-tcl-config=TCL_CONFIG],
                [specify location of tclConfig.sh @<:@@:>@])],
[TCL_CONFIG="${withval}"],
[TCL_CONFIG=""])
AC_ARG_WITH([tk-config],
[AC_HELP_STRING([--with-tk-config=TK_CONFIG],
                [specify location of tkConfig.sh @<:@@:>@])],
[TK_CONFIG="${withval}"],
[TK_CONFIG=""])

AC_ARG_VAR([TCLTK_LIBS],
           [flags needed for linking against the Tcl and Tk libraries])
AC_ARG_VAR([TCLTK_CPPFLAGS],
           [flags needed for finding the tcl.h and tk.h headers])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
TCLTK_CPPFLAGS=`${R_HOME}/bin/R CMD config TCLTK_CPPFLAGS`
TCLTK_LIBS=`${R_HOME}/bin/R CMD config TCLTK_LIBS`	   

if test -z "${TCLTK_LIBS}"; then
  R_TCLTK
fi

### Output.

AC_OUTPUT(src/Makevars)

dnl Local Variables: ***
dnl mode: sh ***
dnl sh-indentation: 2 ***
dnl End: ***
