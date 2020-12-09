# From http://www.greghendershott.com/2014/06/does-your-racket-project-need-a-makefile.html
COLLECTS=sauron
SCRBL=scribblings/sauron.scrbl

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf

setup:
	raco setup $(COLLECTS)

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
	-(cd docs; git commit -m "Update $$(date +%Y%m%d%H%M%S)")
	git push origin
