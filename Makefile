REPOURL=http://joyful.com/repos/rss2irc
MAIN=rss2irc.hs
SOURCEFILES=*hs
PKG=rss2irc
EXE=$(PKG)

BUILDFLAGS=-rtsopts -Wall -fno-warn-orphans -fno-warn-unused-do-bind -threaded -O2

#HEAPPROFFLAGS=-hc
HEAPPROFFLAGS=-ht
#HEAPPROFFLAGS=-hT

# command to run during make auto or auto2
AUTOCMD=--help
AUTO2CMD='http://losangeles.craigslist.org/cpg/index.rss' 'rss2irctestbot@irc.freenode.net/smtest' -i1 --author --link --replace '(&.*?;|<.*?>)/' -r2

# command to run during make prof, heap, coverage
REPORTCMD=-n4 -i0 http://losangeles.craigslist.org/cpg/index.rss

# viewers
VIEWHTMLCMD=open
VIEWPSCMD=open


########################################
# GENERIC - reuse across projects

TIME:=$(shell date +"%Y%m%d%H%M")

install:
	cabal install

build:
	cabal configure && cabal build

# auto-recompile and run whenever a module changes.
# sp is from searchpath.org, you might need the http://joyful.com/repos/searchpath version.
auto:
	sp --no-exts --no-default-map -o $(EXE) ghc $(BUILDFLAGS) --make $(MAIN) --run $(AUTOCMD)

auto2:
	sp --no-exts --no-default-map -o $(EXE) ghc $(BUILDFLAGS) --make $(MAIN) --run $(AUTO2CMD)

# build the -p binary used for performance and heap profiles.
# You may need to cabal install --reinstall -p some libs.
# Keep these .o files separate.
$(EXE)p: $(SOURCEFILES)
	ghc --make $(MAIN) -prof -auto-all $(BUILDFLAGS) -o $@

# build the -hpc binary used for coverage reports.
# Keep these .o files separate.
$(EXE)hpc: $(SOURCEFILES)
	ghc --make $(MAIN) -fhpc -outputdir .hpcobjs $(BUILDFLAGS) -o $@

ghci:
	ghci $(MAIN)

profs:
	mkdir -p profs

tag: emacstags

emacstags:
	@rm -f TAGS; hasktags -e $(SOURCEFILES) *.cabal

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*"`

Clean: clean
	rm -f TAGS tags $(EXE) $(EXE)p $(EXE)hpc
	cabal clean

reports: profile heap coverage
	@echo Reports are at $(REPOURL)/profs

# generate, save and display a standard profile
profile: $(EXE)p profs
	@echo "Profiling $(REPORTCMD)"
	@./$(EXE)p +RTS -p -RTS $(REPORTCMD) #>/dev/null
	@mv $(EXE)p.prof profs/$(TIME).prof #-orig.prof #	tools/simplifyprof.hs profs/$(TIME)-orig.prof >profs/$(TIME).prof
	@(cd profs; rm -f latestprof*.txt; ln -s $(TIME).prof latestprof.txt) # ; ln -s $(TIME)-orig.prof latestprof-orig.prof
	@echo; cat profs/latestprof.txt

# generate, save and display a graphical heap profile
heap: $(EXE)p profs
	@echo "Profiling heap with $(REPORTCMD)"
	./$(EXE)p +RTS $(HEAPPROFFLAGS) -RTS $(REPORTCMD) #>/dev/null
	mv $(EXE)p.hp profs/$(TIME).hp
	(cd profs; hp2ps $(TIME).hp; rm -f latestheap.ps; ln -s $(TIME).ps latestheap.ps; rm -f *.aux)
# 	$(VIEWPSCMD) profs/latestheap.ps

# generate and display a code coverage report
coverage: $(EXE)hpc resettix profs
	@echo "Generating coverage report with $(REPORTCMD)"
	./$(EXE)hpc $(REPORTCMD) #>/dev/null
	hpc markup --destdir=profs $(EXE)hpc
	cd profs; rm -f latestcoverage.html; ln -s hpc_index.html latestcoverage.html
# 	cd profs; rm -f index.html; ln -s hpc_index.html index.html
# 	$(VIEWHTMLCMD) profs/coverage/index.html

# clear the old tick counts from a prior coverage run
resettix:
	rm -f $(EXE)hpc.tix

# misc stats reports

unreleasedcodechanges unreleased:
	@echo "code changes since last release:"
	@darcs changes --from-tag . --matches "not (name docs: or name doc: or name site: or name tools:)" | grep '*'
	@echo

codechanges:
	@echo "code changes:"
	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
	@echo

########################################
# THIS PROJECT

deploy: buildonly /usr/local/bin/$(EXE)

buildonly:
	cabal build

/usr/local/bin/$(EXE): dist/build/$(PKG)/$(EXE)
	rm -f $@
	cp $< $@

listbinaries:
	ls -lt rss2irc rss2irc-*

########################################
# THIS REPO

-include Makefile.local
