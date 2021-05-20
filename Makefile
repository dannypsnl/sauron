.PHONY: ci-test test
ci-test:
	@raco test ./pos-range.rkt \
				./repl/history.rkt
test:
	@raco test .

clean:
	find . -name compiled -type d | xargs rm -rf
