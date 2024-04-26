.PHONY: run html

run:
	dune build
	gamelle hotreload _build/default/src/gamelle_example.cmxs &
	dune build --watch

html:
	dune build --profile=release
	xdg-open _build/default/gamelle_example.html
