.PHONY: test cover
test:
	@raco test .
cover:
	@raco cover -b .

clean:
	find . -name compiled -type d | xargs rm -rf
