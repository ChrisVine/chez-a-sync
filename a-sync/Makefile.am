## Process this file with automake to produce Makefile.in

SUBDIRS = helper

SOURCES = try.ss coroutines.ss event-loop.ss compose.ss meeting.ss thread-pool.ss

chezschemedir = $(datadir)/chez-a-sync/a-sync
chezscheme_DATA = $(SOURCES)

if COMPILE_TO_BYTECODE
SOBJECTS = $(SOURCES:%.ss=%.so)

chezscheme_DATA += $(SOBJECTS)

CLEANFILES = $(SOBJECTS)

SUFFIXES: .ss .so
.ss.so:
	$(top_builddir)/a-sync/a-sync-compile.sh "$<" "$@"

# state dependences to prevent parallel compilation busting chez
# scheme's dependency requirements.
try.so: try.ss
coroutines.so: coroutines.ss try.so
event-loop.so: event-loop.ss try.so coroutines.so
compose.so: compose.ss coroutines.so
meeting.so: meeting.ss event-loop.so
thread-pool.so: thread-pool.ss try.so coroutines.so event-loop.so

# make sure that timestamps on object files are after that of source
# files
install-data-hook:
	for files in $(SOBJECTS); do touch -c $(chezschemedir)/$$files; done
endif

EXTRA_DIST = $(SOURCES) a-sync-compile.sh
