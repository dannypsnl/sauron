.PHONY: test
test:
	@raco test .

clean:
	find . -name compiled -type d | xargs rm -rf
