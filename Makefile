WEB_NAME := toktok.github.io

ifneq ($(GITHUB_TOKEN),)
WEB_REPO := https://$(GITHUB_TOKEN)@github.com/TokTok/$(WEB_NAME)
else
WEB_REPO := git@github.com:TokTok/$(WEB_NAME)
endif

toktok-site: dist/build/yst/yst $(shell find toktok -type f)
	rm -rf $@
	cd toktok && ../$< && mv site ../$@
	! which linkchecker || linkchecker --ignore-url "https://travis-ci.org.*" --ignore-url "irc://.*" $@

dist/build/yst/yst: dist/setup-config $(wildcard Yst/*)
	cabal build --ghc-options=-dynamic

dist/setup-config: yst.cabal
	cabal configure

upload: toktok-site
	test -d $(WEB_NAME) || git clone --depth=1 $(WEB_REPO)
	rm -rf $(WEB_NAME)/*
	mv toktok-site/* $(WEB_NAME)/
	rmdir toktok-site
	cd $(WEB_NAME) && git add .
	cd $(WEB_NAME) && git commit -a --amend -m'Updated website'
	cd $(WEB_NAME) && git push --force
