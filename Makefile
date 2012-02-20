ERLC=`which erlc`
BEAMDIR=./ebin
ERLCFLAGS=+debug_info -pa $(BEAMDIR)
SRCDIR=src

.PHONY=all clean

all:
	@echo "Erlware Commons is maintained with Sinan, its much better to use "
	@echo "sinan to build than this makefile. This is here just to get "
	@echo "get you started."
	$(ERLC) $(ERLCFLAGS) -o $(BEAMDIR) $(SRCDIR)/ec_dictionary.erl;
	$(ERLC) $(ERLCFLAGS) -o $(BEAMDIR) $(SRCDIR)/*.erl ;

clean:
	rm $(BEAMDIR)/*.beam
	rm -rf erl_crush.dump