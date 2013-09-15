-module(ecli).

-export([start/2]).
-export([binding/2]).
-export([opt/2]).

-define(PRINT(Fmt, Args), io:format(Fmt, Args)).

-define(HALT(Fmt), ?HALT(Fmt, [])).
-define(HALT(Fmt, Args), ?HALT(Fmt, Args, 1)).
-define(HALT(Fmt, Args, Code), halt_with(Fmt, Args, Code)).

-record(args, {
          bindings = [] :: list({atom(), string()}),
          opts = [] :: list({atom(), string()})
         }).

%% ===================================================================
%% Public
%% ===================================================================

start(Args, Spec) ->
    {Targets, Args1} = targets(Args, []),
    case match_cmd(Spec, Targets) of
        {ok, Fun, CmdSpec, Bindings} ->
            Opts = getopt(Bindings, CmdSpec, Args1),
            run(Fun, Opts);
        nomatch ->
            ?HALT("fuck ~n")
    end.

binding(Key, #args{bindings = Bindings}) ->
    proplists:get_value(Key, Bindings).

opt(Key, #args{opts = Opts}) ->
    proplists:get_value(Key, Opts).

%% ===================================================================
%% Public
%% ===================================================================

targets([], Cmds) ->
    {lists:reverse(Cmds), []};
targets(["-" ++ _ | _] = Args, Cmds) ->
    {lists:reverse(Cmds), Args};
targets([Cmd | Rest], Cmds) ->
    targets(Rest, [Cmd | Cmds]).

match_cmd([], _) ->
    nomatch;
match_cmd([{Route, Fun, CmdSpec} | Rest], Targets) ->
    case match(Targets, Route, []) of
        {ok, Bindings} ->
            {ok, Fun, CmdSpec, Bindings};
        nomatch ->
            match_cmd(Rest, Targets)
    end.

match([], [], Bs) ->
    {ok, Bs};
match([], _, _) ->
    nomatch;
match(_, [], _) ->
    nomatch;
match([Targ | Ts], [Cmd | Cs], Bs) when is_atom(Cmd) ->
    match(Ts, Cs, [{Cmd, Targ} | Bs]);
match([Targ | Ts], [Targ | Cs], Bs) ->
    match(Ts, Cs, Bs);
match(_, _, _) ->
    nomatch.

getopt(Bindings, Spec, Args) ->
    case getopt:parse(Spec, Args) of
        {ok, {Opts, _}} ->
            #args{bindings = Bindings, opts = Opts};
        {error, Error} ->
            ?HALT("Invalid option sequence given: ~w~n", [Error])
    end.

run({Mod, Fun}, Opts) ->
    Mod:Fun(Opts).

halt_with(String, Args, Code) ->
    ?PRINT(String, Args),
    halt(Code).
