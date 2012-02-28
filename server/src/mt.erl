%%% make tools
-module(mt).

-export([make/0, up/0]).

make() ->
    DirAction = case up() of
		    ok -> dir_up;
		    _ -> dir_ok
		end,
    {DirAction, make:all(), distel:reload_modules()}.

up() ->
    {ok, Dir} = file:get_cwd(),
    case filename:basename(Dir) of
	"src" ->
	    ok = file:set_cwd("..");	    
	_ ->
	    not_src
    end.
