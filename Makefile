# config #############################################################

PROGS = dot neato twopi circo fdp

config :
	echo module Autolib.Local where > dot/Autolib/Local.hs
	for prog in $(PROGS); do \
		echo $$prog :: FilePath >> dot/Autolib/Local.hs ; \
		echo $$prog = \"$$(which $$prog)\" >> dot/Autolib/Local.hs ; \
	done

# DrIFT ############################################################

AUTODRIFT = /usr/local/bin/AutoDrIFT

DRIFT = $(basename $(shell find . -name "*.hs.drift" -print))

drift : $(DRIFT)

%.hs : %.hs.drift
	$(AUTODRIFT) $< | grep -v "^{- Generated" > $@


drift-clean :
	rm -f $(shell find . -name "*.hs.drift" -print | sed s/\.drift//)

