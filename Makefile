.PHONY: test cover
test:
	@raco test .
cover:
	@raco cover -n tool -b .

clean:
	find . -name compiled -type d | xargs rm -rf
