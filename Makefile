REBAR=./rebar

compile: $(REBAR)
	$(REBAR) compile

rebar:
	git clone https://github.com/rebar/rebar.git rebar-git
	cd rebar-git && $(MAKE)
	cp rebar-git/rebar ./
	rm -fr rebar-git

run: compile
	erl -pa ebin -sname tile_serv -eval 'application:start(tile_serv)'

debug: compile
    # ,dbg:tracer(),dbg:p(all, [c, timestamp]),dbg:tp(acceptor_sup, add_children, cx)
	erl -pa ebin -sname tile_serv -eval 'observer:start(),application:start(tile_serv)'
