.PHONY: test
test:
	@raco test ./pos-range.rkt \
				./env/env.rkt \
				./panel/repl/history.rkt

clean:
	find . -name compiled -type d | xargs rm -rf
