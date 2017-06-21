AC_DEFUN([AC_CHECK_COMPILER],
[
  AC_ARG_ENABLE(debug,[  --enable-debug           creates debugging code [default=no]],
  [ 
   if test $enableval = "no"; dnl 
     then ac_use_debug_code="no"
     else ac_use_debug_code="yes"
   fi
  ], [ac_use_debug_code="no"])

  dnl this prevents stupid AC_PROG_CXX from adding "-g" to the default CXXFLAGS
  cxx_flags_in=$CXXFLAGS

  AC_PROG_CXX
  AC_PROG_CXXCPP

  CXXFLAGS=$cxx_flags_in

  if test -z "$CXXFLAGS"; then 
    if test "$GXX" = "yes"; then
      if test "$ac_use_debug_code" = "yes"; then
        CXXFLAGS="-g $CXXFLAGS"
      else
        CXXFLAGS="-O2 $CXXFLAGS"
        if test -z "$LDFLAGS"; then 
          LDFLAGS="-s"
        fi
      fi
    else 
      if test "$ac_use_debug_code" = "yes"; then
        AC_CHECK_COMPILER_FLAG(g,
          [
            CXXFLAGS="-g $CXXFLAGS"
        ])
      else 
        AC_CHECK_COMPILER_FLAG(O2,
          [
            CXXFLAGS="-O2 $CXXFLAGS"
        ])
      fi
    fi

    AC_CHECK_COMPILER_FLAG(fexceptions,
      [
        CXXFLAGS="$CXXFLAGS -fexceptions"
    ])

    if test "$GXX" = "yes"; then
       CXXFLAGS="$CXXFLAGS -Wall"
    fi
  fi

  AC_CHECK_COMPILER_FLAG(std=c++11,
    [
      CXXFLAGS="$CXXFLAGS -std=c++11"
    ],
    [
    AC_CHECK_COMPILER_FLAG(std=c++0x,
      [
        CXXFLAGS="$CXXFLAGS -std=c++0x"
    ])
  ])
])

AC_DEFUN([AC_CHECK_COMPILER_FLAG],
[
AC_MSG_CHECKING(whether $CXX supports -$1)
flag_cache=`echo $1 | sed 'y%.=/+-%___p_%'`
AC_CACHE_VAL(ac_cv_prog_cxx_$flag_cache,
[
echo 'int main() { return 0; }' >conftest.cc
eval "ac_cv_prog_cxx_$flag_cache=no"
if test -z "`$CXX -$1 -c conftest.cc 2>&1`"; then
  if test -z "`$CXX -$1 -o conftest conftest.o 2>&1`"; then
    eval "ac_cv_prog_cxx_$flag_cache=yes"
  fi
fi
rm -f conftest*
])
if eval "test \"`echo '$ac_cv_prog_cxx_'$flag_cache`\" = yes"; then
 AC_MSG_RESULT(yes)
 :
 $2
else
 AC_MSG_RESULT(no)
 :
 $3
fi
])

AC_DEFUN([AC_CHECK_COMPILE_SCHEME],
[
  AC_MSG_CHECKING([whether library is to compile scheme files to bytecode])

  AC_ARG_ENABLE(compile-to-bytecode,
  [  --enable-compile-to-bytecode  compile scheme files to bytecode [[default=no]]],
  [
     if test "x$enableval" = "xyes";  then
       compile_scheme=true
       AC_MSG_RESULT([yes])
     elif test "x$enableval" = "xno";  then
       compile_scheme=false
       AC_MSG_RESULT([no])
     else
       AC_MSG_ERROR([incorrect bytecode option specified - should be yes or no])
     fi
  ],
  [
    compile_scheme=false
    AC_MSG_RESULT([no])
  ])
  AM_CONDITIONAL([COMPILE_TO_BYTECODE], [test x$compile_scheme = xtrue])
])
