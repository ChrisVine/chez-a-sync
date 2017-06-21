## Process this file with automake to produce Makefile.in

SUBDIRS = helper

SOURCES = try.ss coroutines.ss event-loop.ss compose.ss meeting.ss thread-pool.ss

chezschemedir = $(datadir)/chez-a-sync/a-sync
chezscheme_DATA = $(SOURCES)

if COMPILE_TO_BYTECODE
SOBJECTS = $(SOURCES:%.ss=%.so)

chezscheme_DATA += $(SOBJECTS)

CLEANFILES = $(SOBJECTS)

# unfortunately we cannot compile with a generalized SUFFIXES target
# because we must state dependences to prevent parallel compilation
# busting chez scheme's dependency requirements.  Bummer.
try.so: try.ss
	$(top_builddir)/a-sync/a-sync-compile.sh try.ss "$@"
coroutines.so: coroutines.ss try.so
	$(top_builddir)/a-sync/a-sync-compile.sh coroutines.ss "$@"
event-loop.so: event-loop.ss try.so coroutines.so
	$(top_builddir)/a-sync/a-sync-compile.sh event-loop.ss "$@"
compose.so: compose.ss coroutines.so
	$(top_builddir)/a-sync/a-sync-compile.sh compose.ss "$@"
meeting.so: meeting.ss event-loop.so
	$(top_builddir)/a-sync/a-sync-compile.sh meeting.ss "$@"
thread-pool.so: thread-pool.ss try.so coroutines.so event-loop.so
	$(top_builddir)/a-sync/a-sync-compile.sh thread-pool.ss "$@"

# make sure that timestamps on object files are after that of source
# files
install-data-hook:
	for files in $(SOBJECTS); do touch -c $(chezschemedir)/$$files; done
endif

EXTRA_DIST = $(SOURCES) a-sync-compile.sh
