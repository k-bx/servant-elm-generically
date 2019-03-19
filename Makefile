all:
	stack run -- generate-elm > frontend/src/Api.elm
	mkdir -p out
	(cd frontend && elm make src/Main.elm --debug --output=../out/index.html)
.PHONY: all

run:
	stack run
.PHONY: run
