REBAR=~/bin/rebar

compile: rebar
	$(REBAR) compile

rebar: ~/bin/rebar

~/bin/rebar:
	git clone https://github.com/rebar/rebar.git
	cd rebar && $(MAKE)
	mkdir -p ~/bin
	cp rebar/rebar ~/bin/
	rm -fr rebar

run: compile
	erl -pa ebin -sname tile_serv -eval 'application:start(tile_serv)'

debug: compile
    # ,dbg:tracer(),dbg:p(all, [c, timestamp]),dbg:tp(acceptor_sup, add_children, cx)
	erl -pa ebin -sname tile_serv -eval 'observer:start(),application:start(tile_serv)'
