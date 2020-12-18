# From http://www.greghendershott.com/2014/06/does-your-racket-project-need-a-makefile.html
SCRBL=scribblings/sauron.scrbl

.PHONY: test docs publish clean

test:
	@raco test ./pos-range.rkt ./panel/repl/history.rkt

clean:
	find . -name compiled -type d | xargs rm -rf

docs: $(SCRBL)
	@mkdir -p docs/
	raco scribble \
		--html \
		--dest docs \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		$(SCRBL)

publish: docs
	(cd docs; git add -A)
	-(cd docs; git commit -m "update docs $$(date '+%Y/%m/%d %H:%M:%S')")
	git push origin
