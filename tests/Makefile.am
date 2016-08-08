## Process this file with automake to produce Makefile.in

.PHONY: test
test:
	for files in `ls *.ss`; do $(top_builddir)/tests/run-test.sh $$files; done

EXTRA_DIST = test-event-loop.ss test-coroutines.ss \
	     test-helpers-await.ss test-try.ss run-test.sh
