-module(eep_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

prop_sliding_max() ->
    ?FORALL({List, WindowSize}, {nonempty_int_list(),window_size()},
            begin
%%                io:format("{List,WindowSize}=~p~n",[{List, WindowSize}]),
                Res = execute_sliding(eep_stats_max, List, WindowSize),
%%                io:format("done execute_sliding~n"),
                Expected = lists:max(slice(List, WindowSize)),
                Res == Expected
            end).


%% for now we only consider positive integers until we understand the
%% spec of max better.
%% The problem is that for the empty stream max is initialised to 0 -
%% it might be better to use undefined.
nonempty_int_list() ->
    ?SUCHTHAT(L, list(pos_int()), L /= []).

pos_int() ->
    ?SUCHTHAT(N, int(), N>0).

window_size() ->
    ?SUCHTHAT(N, int(), N>0).

execute_sliding(Mod, List, WindowSize) ->
    Pid = eep_window_sliding:start(Mod, WindowSize),
    [ Pid ! {push, Elem} || Elem <- List ],
    Pid ! {debug, self()},
    Res =
        receive
            {debug, {eep_stats_max, _Size, _Count, State}} ->
                State
        end,
    Pid ! stop,
    Res.

slice(List, WindowSize) when length(List) =< WindowSize ->
    List;
slice(List, WindowSize) ->
%%    io:format("{List,WindowSize}=~p~n",[{List, WindowSize}]),
    lists:sublist(List, length(List)-WindowSize+1,WindowSize).


    

    