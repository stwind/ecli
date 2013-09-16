-module(ecli).

-export([start/2]).
-export([binding/2]).
-export([opt/2]).

-define(PRINT(Fmt), io:format(Fmt)).
-define(PRINT(Fmt, Args), io:format(Fmt, Args)).

-define(HALT(Fmt), ?HALT(Fmt, [])).
-define(HALT(Fmt, Args), ?HALT(Fmt, Args, 1)).
-define(HALT(Fmt, Args, Code), halt_with(Fmt, Args, Code)).

-define(LINE_LENGTH, 75).

-record(args, {
          bindings = [] :: list({atom(), string()}),
          opts = [] :: list({atom(), string()})
         }).

%% ===================================================================
%% Public
%% ===================================================================

start(Args, Spec) ->
    {Targets, Args1} = targets(Args, []),
    case match_cmd(val(commands, Spec), Targets, []) of
        {ok, Fun, CmdSpec, Bindings} ->
            Opts = parse_args(CmdSpec, Args1),
            run(Fun, #args{bindings = Bindings, opts = Opts});
        {nomatch, Cmd, Acc} ->
            cmd_usage(Spec, Cmd, Acc);
        {nomatch, []} ->
            usage(Spec, [spec_help() | spec_default()], []);
        {nomatch, Acc} ->
            usage(Spec, [], Acc)
    end.

binding(Key, #args{bindings = Bindings}) ->
    val(Key, Bindings).

opt(Key, #args{opts = Opts}) ->
    val(Key, Opts).

%% ===================================================================
%% Public
%% ===================================================================

targets([], Cmds) ->
    {lists:reverse(Cmds), []};
targets(["-" ++ _ | _] = Args, Cmds) ->
    {lists:reverse(Cmds), Args};
targets([Cmd | Rest], Cmds) ->
    targets(Rest, [Cmd | Cmds]).

match_cmd([], _, Acc) ->
    {nomatch, lists:reverse(Acc)};
match_cmd([{Sub, Binds, Fun, Spec} = CmdSpec | _], [Sub | Rest], Acc) ->
    case match_bind(Binds, Rest, []) of
        {ok, Bindings} ->
            {ok, Fun, Spec, Bindings};
        nomatch ->
            {nomatch, CmdSpec, lists:reverse(Acc)}
    end;
match_cmd([{Sub, SubCmds} | _], [Sub | Rest], Acc) ->
    match_cmd(SubCmds, Rest, [Sub | Acc]);
match_cmd([_ | Rest], Targets, Acc) ->
    match_cmd(Rest, Targets, Acc).

match_bind([], [], Bs) ->
    {ok, Bs};
match_bind([], _, _) ->
    nomatch;
match_bind(_, [], _) ->
    nomatch;
match_bind([Name | Ns], [Target | Ts], Bs) ->
    match_bind(Ns, Ts, [{Name, Target} | Bs]).

parse_args(Spec, Args) ->
    case getopt:parse(Spec, Args) of
        {ok, {Opts, _}} ->
            Opts;
        {error, Error} ->
            ?HALT("Invalid option sequence given: ~w~n", [Error])
    end.

run({Mod, Fun}, Opts) ->
    Mod:Fun(Opts).

halt_with(String, Args, Code) ->
    ?PRINT(String, Args),
    halt(Code).

val(Key, Vals) ->
    proplists:get_value(Key, Vals).

val(Key, Vals, Def) ->
    proplists:get_value(Key, Vals, Def).

spec_default() ->
    [{version, $v, "version", undefined, "Print the version and exit."}].

spec_help() ->
    {help, $h, "help", undefined, "Print this help."}.

usage(Spec, CmdSpec, Acc) ->
    usage_cmd_line(Spec, [Acc, " <command> [<arg>]"]),
    usage_spec(CmdSpec),
    usage_subcmds(Spec, Acc),
    usage_help(Spec).

usage_cmd_line(Spec, Args) ->
    Script = val(script, Spec, "undef_script_name"),
    ?PRINT("Usage: ~s ~s [options]~n~n", [Script, lists:flatten(Args)]).

cmd_usage(Spec, {Sub, Binds, _, CmdSpec}, Acc) ->
    Binds1 = [["<",to_string(B), ">"] || B <- Binds],
    usage_cmd_line(Spec, [string:join(Acc ++ [Sub] ++ Binds1, " ")]),
    usage_spec(CmdSpec),
    halt(0).

usage_spec([]) ->
    ok;
usage_spec(Spec) ->
    {MaxLen, UsageLines} = usage_spec_lines(Spec, 0, []),
    MaxLineLen = line_len(),
    [print(fmt_spec_line(MaxLen + 1, MaxLineLen, L)) || L <- UsageLines],
    ?PRINT("~n").

usage_subcmds(Spec, Depth) ->
    Commands = get_subcmds(val(commands, Spec), Depth),
    usage_subcmds_1(Commands).

usage_subcmds_1([]) ->
    ok;
usage_subcmds_1(Commands) ->
    ?PRINT("Available subcommands: ~n~n"),
    [print(fmt_sub_cmd_line(L)) || L <- Commands],
    ?PRINT("~n").

usage_spec_lines([Opt | Rest], PrevMax, Acc) ->
    OptionText = usage_opt_text(Opt),
    HelpText = usage_help_text(Opt),
    {Max, ColWidth} = max_opt_len({OptionText, HelpText}, PrevMax),
    usage_spec_lines(Rest, Max, [ColWidth | Acc]);
usage_spec_lines([], Max, Acc) ->
    {Max, Acc}.

max_opt_len({OptText, HelpText}, PrevMax) ->
    OptLen = length(OptText),
    {erlang:max(OptLen, PrevMax), {OptLen, OptText, HelpText}}.

usage_opt_text({_Name, Short, undefined, _ArgSpec, _Help}) ->
    [$-, Short];
usage_opt_text({_Name, undefined, Long, _ArgSpec, _Help}) ->
    [$-, $- | Long];
usage_opt_text({_Name, Short, Long, _ArgSpec, _Help}) ->
    [$-, Short, $,, $\s, $-, $- | Long].

usage_help_text({_Name, _Short, _Long, {_ArgType, ArgValue}, [_ | _] = Help}) ->
    Help ++ " [default: " ++ to_string(ArgValue) ++ "]";
usage_help_text({_Name, _Short, _Long, _ArgSpec, Help}) ->
    Help.

%usage_ver(Spec) ->
    %Script = val(script, Spec, "undef_script_name"),
    %Ver = val(vsn, Spec, "undef_script_ver"),
    %?PRINT("~s ~s~n", [Script, Ver]).

get_subcmds(Cmds, Acc) ->
    case g_subcmds(Cmds, Acc) of
        nomatch ->
            proplists:get_keys(Cmds);
        SubCmds ->
            SubCmds   
    end.

g_subcmds([], _) ->
    nomatch;
g_subcmds([{Sub, SubCmds} | _], [Sub | _]) ->
    proplists:get_keys(SubCmds);
g_subcmds([_ | Rest], Acc) ->
    g_subcmds(Rest, Acc).

fmt_spec_line(MaxOptLen, MaxLineLen, {OptLen, OptText, [_ | _] = HelpText})
  when MaxOptLen < (MaxLineLen div 2) ->
    [Head | Tail] = wrap_line(MaxLineLen - MaxOptLen - 3, HelpText),
    FirstLineIndent = lists:duplicate(MaxOptLen - OptLen + 1, $\s),
    Indent = [$\n | lists:duplicate(MaxOptLen + 3, $\s)],
    ["  ", OptText, FirstLineIndent, Head,
     [[Indent, Line] || Line <- Tail], $\n];
fmt_spec_line(_, MaxLineLen, {_, OptText, [_ | _] = HelpText}) ->
    HelpLines = wrap_line(MaxLineLen - 6, HelpText),
    ["  ", OptText, [["\n      ", Line] || Line <- HelpLines], $\n];
fmt_spec_line(_, _, {_, OptText, _}) ->
    ["  ", OptText, $\n].

fmt_sub_cmd_line(Cmd) ->
    ["  ", Cmd, $\n].

wrap_line(Length, Text) ->
    wrap_line(Length, Text, [], 0, []).

wrap_line(Length, [Char | Tail], Acc, Count, CurrentLineAcc) when Count < Length ->
    wrap_line(Length, Tail, Acc, Count + 1, [Char | CurrentLineAcc]);
wrap_line(Length, [_ | _] = Help, Acc, Count, CurrentLineAcc) ->
    {NextLineAcc, WrappedLine} = case string:cspan(CurrentLineAcc, " \t") of
        WhitespacePos when WhitespacePos < Count ->
            lists:split(WhitespacePos, CurrentLineAcc);
        _ ->
            {[], CurrentLineAcc}
    end,
    wrap_line(Length, Help, [lists:reverse(WrappedLine) | Acc], 
              length(NextLineAcc), NextLineAcc);
wrap_line(_Length, [], Acc, _Count, [_ | _] = CurrentLineAcc) ->
    lists:reverse([lists:reverse(CurrentLineAcc) | Acc]);
wrap_line(_Length, [], Acc, _Count, _CurrentLineAcc) ->
    lists:reverse(Acc).

print(Line) ->
    ?PRINT("~s",[lists:flatten(Line)]).

line_len() ->
    case io:columns() of
        {ok, Columns} when Columns < ?LINE_LENGTH ->
            Columns - 1;
        _ ->
            ?LINE_LENGTH
    end.

to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string(Value) when is_float(Value) ->
    lists:flatten(io_lib:format("~w", [Value]));
to_string(Value) ->
    Value.

usage_help(Spec) ->
    Script = val(script, Spec, "undef_script_name"),
    ?PRINT("For help on any individual command run `~s COMMAND -h`~n", [Script]).
