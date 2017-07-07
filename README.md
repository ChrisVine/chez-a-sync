Overview
--------

This package is a port of guile-a-sync for chez scheme.  It provides a
thread safe event loop (event-loop.ss) with support for watches on
ports/file descriptors and timeouts, and permits events to be posted
by other tasks.  This includes tasks running on other threads.

It also provides a coroutines interface (coroutines.ss) which
provides await semantics on such events, so as to avoid inversion of
control (aka "callback hell").

The package has been tested with chez-scheme-9.4.

See the documentation mentioned below for further details, and the
docs/example.ss file.

Installation
------------

When first run from git, or a tarball obtained from github, it is
necessary to set up autotools.  This can be done with:

  ./autogen.sh --prefix=/usr

or on a 64-bit system, probably:

  /autogen.sh --prefix=/usr --libdir=/usr/lib64

This generates a configure script and installs libtool.  Subsequent
configuration can be done just with

  ./configure --prefix=/usr

and

  ./configure --prefix=/usr --libdir=/usr/lib64

respectively.

On a 64 bit system you may also need to include -fPIC in your CFLAGS
options if libtool doesn't do that for you (libtool normally does when
necessary).

If using gcc to compile, gcc-4.6 or later is required.  If using
clang, clang-3.3 or later is required.

After compiling, install with 'make install' as root.  If /usr is the
prefix, this will install chez-a-sync in /usr/share/chez-a-sync, and
so on for other prefixes.  To enable user code to find the package,
either the CHEZSCHEMELIBDIRS environmental variable should be set to
include that directory, or that directory should be passed to the
scheme binary with the --libdirs option.  Also, run ldconfig after
installation because a shared support library (libchez-a-sync-0) is
installed in libdir.

From version 0.17, 'autogen.sh' and 'configure' can be passed the
configuration option --enable-compile-to-bytecode.  If that option is
chosen, then 'make' will compile the library's scheme source (\*.ss)
files to scheme object (\*.so) files, and 'make install' will install
those object files to the same directory as the source files.  By
default this pre-compilation will not be carried out, mainly because
it has not been tested with Mac OS, but as this default might change
in future, in writing build scripts it is worth considering calling up
the --disable-compile-to-bytecode or --enable-compile-to-bytecode
option explicitly.  If pre-compilation is not carried out, this can be
done by the user subsequently in the scheme REPL using the
'compile-library' procedure, but note that order of compilation is
important.  The ordering is: try.ss, coroutines.ss and event-loop.ss
first, in that order, followed by compose.ss, meeting.ss and
thread-pool.ss in any order.  Note also that when invoking the
'compile-library' procedure, the CHEZSCHEMELIBDIRS environmental
variable or the --libdirs command-line option must be correctly set to
cover those source and object files.  Note also that any instance of a
compiled program is tied to the particular compiled library object
files against which it was compiled.  If, on running a compiled
program, you get the message "Exception: compiled program requires
different compilation instance of ([xxx] [xxx]) from one found in
[xxx]", then dependency ordering was wrong or libdirs was wrong during
compilation, or the program is being run against a different set of
library object files.

Supported operating systems
---------------------------

This package uses chez scheme's FFI to call up the pipe() and poll()
system calls.  It therefore requires a UNIX-like operating system to
function correctly.  It has been tested with linux and should work on
the BSDs and on Mac OS X.  If using OS X, the glibtool package will be
required.

This package also requires chez scheme to have been compiled with the
--threads option.

Documentation
-------------

Html documentation is available after installation in the default html
directory for the target installation (normally at
$(prefix)/share/doc/chez-a-sync/html/index.html).

In addition, the documentation can be viewed at github at:
https://github.com/ChrisVine/chez-a-sync/wiki
