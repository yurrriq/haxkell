HADDOCK_OPTIONS := "-hU -o ../doc/$(notdir $(CURDIR))"

.PHONY: doc
doc:
	@cabal haddock --haddock-options=$(HADDOCK_OPTIONS)

# Local Variables:
# eval: (standard-display-ascii ?\t "  ")
# End:
