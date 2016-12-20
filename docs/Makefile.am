## Process this file with automake to produce Makefile.in

.PHONY: docs
docs:
	makeinfo chez-a-sync.texi
	makeinfo --html chez-a-sync.texi
	rm -rf html
	mv chez-a-sync html

doc_DATA = example.ss example-client.ss example-server.ss

infofiledir = $(infodir)
infofile_DATA = chez-a-sync.info

install-data-hook:
	install -d $(DESTDIR)$(htmldir)/html
	install -m a+r,u+w html/* $(DESTDIR)$(htmldir)/html/

uninstall-hook:
	for files in `ls html`; do rm -f $(DESTDIR)$(htmldir)/html/$$files; done

EXTRA_DIST = html/*.html *.texi chez-a-sync.info $(doc_DATA)
