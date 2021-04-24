
.PHONY: build
build:
	cabal build all

.PHONY: vim
vim:
	echo ":vsp\n:term" | nvim -s -

.PHONY: format
format:
	find src -name '*.hs' -exec echo "Formatting '{}'" \; -exec ormolu --mode=inplace '{}' \;
	find cli -name '*.hs' -exec echo "Formatting '{}'" \; -exec ormolu --mode=inplace '{}' \;
