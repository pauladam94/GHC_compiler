DIFF=diff --ignore-all-space

SRC=$(wildcard *.f)
SRCJ=$(wildcard *.fj)

BAD=$(wildcard bad/*.f)
BADJ=$(wildcard bad/*.fj)

ECHO=$(SRC:.f=.echo)

TYPECHECK=$(SRC:.f=.typecheck)
TYPECHECKJ=$(SRCJ:.fj=.typecheckj)

TYPEFAIL=$(BAD:.f=.typefail)
TYPEFAILJ=$(BADJ:.fj=.typefailj)

OPTIMIZE2=$(SRC:.f=.opt2)
OPTIMIZE3=$(SRCJ:.fj=.opt3) $(SRC:.f=.opt3)

JOUJOU ?= src/joujou

################################################################
test: test1 test2 test3

################################################################
# For debugging purposes
# eg., `ARGS=--optimize make caseofcase.echo`

%.echo: %.f force
	$(JOUJOU) --echo $(ARGS) $<

echo: $(ECHO)

################################################################
# Test Task1

%.typecheck: %.f force
	$(JOUJOU) $< > /dev/null

%.typefail: %.f force
	! $(JOUJOU) $< &> /dev/null

test1: $(TYPECHECK) $(TYPEFAIL)

################################################################
# Test Task2

%.opt2: %.f force
	$(JOUJOU) --optimize $< > $*.opt2
	@$(DIFF) $*.opt2 $*.spec2; true

test2: $(OPTIMIZE2)

################################################################
# Test Task3

%.typecheckj: %.fj force
	$(JOUJOU) $< > /dev/null

%.typefailj: %.fj force
	! $(JOUJOU) $< &> /dev/null

%.opt3: %.fj force
	$(JOUJOU) --optimize --case-of-case $< > $*.opt3
	@$(DIFF) $*.opt3 $*.spec3; true

%.opt3: %.f force
	$(JOUJOU) --optimize --case-of-case $< > $*.opt3
	@$(DIFF) $*.opt3 $*.spec3; true

test3a: $(TYPECHECKJ) $(TYPEFAILJ) $(TYPECHECK) $(TYPECHECK)

test3b: $(OPTIMIZE3)

test3: test3a test3b

################################################################
# Global tests

typecheck: test1 test3a

optimize: test2 test3b

################################################################
# Misc.

force:
	@true

clean:
	rm -f *.opt2 *.opt3

################################################################

# ADD YOUR TEST HANDLERS HERE


