rebar = ./rebar

compile:
	$(rebar) compile

clean:
	$(rebar) clean

shell:
	erl -pa ebin -s tsdb_reloader
