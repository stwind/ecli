-module(ecli_tbl).

-export([print/2]).

-include("ecli.hrl").

-define(CHARS, 
        [
         {"top", [9472]}, %% "─"
         {"top-mid", [9516]}, %% "┬"
         {"top-left", [9484]}, %% "┌"
         {"top-right", [9488]}, %% "┐"
         {"bottom", [9472]}, %% "─"
         {"bottom-mid", [9524]}, %% "┴"
         {"bottom-left", [9492]}, %% "└"
         {"bottom-right", [9496]}, %% "┘"
         {"left", [9474]}, %% "│"
         {"left-mid", [9500]}, %%"├" 
         {"mid", [9472]}, %% "─"
         {"mid-mid", [9532]}, %% "┼"
         {"right", [9474]}, %% "│"
         {"right-mid", [9508]}, %% "┤"
         {"middle", [9474]} %% "│"
        ]).

-define(nth(I, L), lists:nth(I, L)).
-define(dup(N, C), lists:duplicate(N, C)).
-define(pad(N), ?dup(N, $\s)).

-type head() :: atom() | binary().

-type column() :: width() | 
                  {width(), align()} | 
                  {width(), align(), pad_left(), pad_right()}.
-type align() :: left | right | center.
-type width() :: integer().
-type pad_left() :: integer().
-type pad_right() :: integer().

-record(table, {
          chars = ?CHARS :: [{string(), string()}],
          width = 0 :: integer(),
          columns = [] :: [column()],
          heads = [] :: [head()],
          truncate = "..." :: string(),
          rows = []
         }).

%% ===================================================================
%% Public
%% ===================================================================

print(Rows, Opts) ->
    Table = init_opts(Opts, #table{rows = Rows}),
    Table1 = init_rows(Rows, Table),
    Table2 = init_heads(Table1),
    Table3 = align_rows(Table2),
    Table4 = init_columns(Table3),
    Table5 = calc_width(Table4),
    %print_info(Table5),
    do_print(Table5).

%% ===================================================================
%% Private
%% ===================================================================

init_opts([{chars, Chars} | Rest], Table) ->
    init_opts(Rest, Table#table{chars = Chars});
init_opts([{columns, Columns} | Rest], Table) ->
    init_opts(Rest, Table#table{columns = Columns});
init_opts([{heads, Heads} | Rest], Table) ->
    init_opts(Rest, Table#table{heads = Heads});
init_opts([_ | Rest], Table) ->
    init_opts(Rest, Table);
init_opts([], Table) ->
    Table.

init_rows(Rows, Table) ->
    Rows1 = lists:map(fun(Row) -> [{to_b(K), V} || {K, V} <- Row] end, Rows),
    Table#table{rows = Rows1}.

init_heads(#table{heads = [_|_] = Heads} = Table) ->
    Table#table{heads = lists:usort([to_b(H) || H <- Heads])};
init_heads(#table{rows = Rows} = Table) ->
    Keys = lists:foldl(
             fun(Row, Acc) -> 
                     proplists:get_keys(Row) ++ Acc
             end, [], Rows),
    Table#table{heads = lists:usort([to_b(H) || H <- Keys])}.

align_rows(#table{heads = Heads, rows = Rows} = Table) ->
    Rows1 = lists:map(fun(Row) -> align_row(Row, Heads) end, Rows),
    Table#table{rows = [[to_l(H) || H <- Heads] | Rows1]}.

align_row(Row, Heads) ->
    [proplists:get_value(Key, Row, "") || Key <- Heads].

init_columns(#table{columns = Columns, heads = Heads} = Table) 
  when length(Columns) == length(Heads) ->
    Table#table{columns = [column(C) || C <- Columns]};
init_columns(#table{rows = Rows} = Table) ->
    Columns = lists:map(fun(Idx) ->  init_column(Idx, Rows) end, 
                          lists:seq(1, length(hd(Rows)))),
    Table#table{columns = [column(C) || C <- Columns]}.

init_column(Idx, Rows) ->
    column(calc_colwidth(Idx, Rows)).

column(Width) when is_integer(Width) ->
    {Width, left, 0, 0};
column({Width, Align}) ->
    {Width, Align, 0, 0};
column({_, _, _, _} = Col) ->
    Col.

calc_colwidth(Idx, Rows) ->
    Vals = [lists:nth(Idx, Row) || Row <- Rows],
    lists:max([get_width(V) || V <- Vals]).

%% TODO: handle unicode
get_width(Val) ->
    length(to_l(Val)).

calc_width(#table{columns = Columns} = Table) ->
    Width = lists:foldl(
              fun({W, _, _, _}, Acc) -> 
                      W + Acc 
              end, length(Columns) + 1, Columns),
    Table#table{width = Width}.

%% ===================================================================
%% Drawing functions
%% ===================================================================

%print_info(#table{rows = Rows, columns = Columns, width = Width} = T) ->
    %?PRINT("columns: ~p~n", [Columns]),
    %?PRINT("checkpoints: ~p~n", [checkpoints(T)]),
    %?PRINT("width: ~p~n", [Width]),
    %[?PRINT("r: ~p ~n", [Row]) || Row <- Rows].

do_print(#table{rows = Rows} = T) ->
    print_line_top(T),
    print_line_break(),
    print_rows(Rows, T),
    print_line_break().

print_rows([Cells], #table{columns = Cols} = T) ->
    lists:foreach(
      fun({Cell, Col}) -> 
              print_cell(to_l(Cell), Col, T)
      end, lists:zip(Cells, Cols)),
    ?PRINT("~ts",[c("right", T)]),
    print_line_break(),
    print_line_bottom(T);
print_rows([Cells | Rs], #table{columns = Cols} = T) ->
    lists:foreach(
      fun({Cell, Col}) -> 
              print_cell(to_l(Cell), Col, T)
      end, lists:zip(Cells, Cols)),
    ?PRINT("~ts",[c("right", T)]),
    print_line_break(),
    print_line_mid(T),
    print_line_break(),
    print_rows(Rs, T).

print_line_top(#table{width = Width} = T) ->
    line({c("top",T),c("top-left",T),c("top-right",T),c("top-mid",T)},
         {1, Width}, checkpoints(T)).

print_line_mid(#table{width = Width} = T) ->
    line({c("mid",T),c("left-mid",T),c("right-mid",T),c("mid-mid",T)},
         {1, Width}, checkpoints(T)).

print_line_bottom(#table{width = Width} = T) ->
    line({c("bottom",T),c("bottom-left",T),c("bottom-right",T),c("bottom-mid",T)},
         {1, Width}, checkpoints(T)).

print_cell(Cell, Col, T) ->
    ?PRINT("~ts~ts", [c("left",T), fmt_cell(Cell, Col, T)]).

fmt_cell(Cell, {Width, Align, PL, PR}, _) when length(Cell) =< Width  ->
    {PadL, PadR} = calc_cell_pad(Cell, Width, Align),
    ?pad(PadL + PL) ++ Cell ++ ?pad(PadR + PR);
fmt_cell(Cell, {Width, _, PL, PR}, T) ->
    ?pad(PL) ++ truncate(Cell, Width, T) ++ ?pad(PR).

print_line_break() ->
    ?PRINT("~n").

checkpoints(#table{columns = Columns}) ->
    {CheckPoints, _} = lists:foldl(
      fun({W, _, _, _}, {CW, Acc}) -> 
              P = W + Acc + 1,
              {[P | CW], P}
      end, {[], 1}, Columns),
    lists:reverse(CheckPoints).

line({_, Left, _, _} = Chars, {1, Width}, CheckPoints) ->
    ?PRINT("~ts", [Left]),
    line(Chars, {2, Width}, CheckPoints);
line({_, _, Right, _}, {Width, Width}, _)  ->
    ?PRINT("~ts", [Right]);
line({Line, _, _, Inter} = Chars, {Now, Width}, CheckPoints) ->
    case lists:member(Now, CheckPoints) of
        true -> ?PRINT("~ts", [Inter]);
        false -> ?PRINT("~ts", [Line])
    end,
    line(Chars, {Now + 1, Width}, CheckPoints).

to_b(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_b(V) when is_list(V) ->
    list_to_binary(V);
to_b(V) when is_binary(V) ->
    V.

to_l(V) when is_float(V); is_integer(V) ->
    mochinum:digits(V);
to_l(V) when is_binary(V) ->
    binary_to_list(V);
to_l(V) when is_atom(V) ->
    atom_to_list(V);
to_l(V) when is_list(V) ->
    V;
to_l(_) ->
    exit(badarg).

c(C, #table{chars = Chars}) ->
    proplists:get_value(C, Chars).

calc_cell_pad(Cell, Width, left) ->
    {0, Width - length(Cell)};
calc_cell_pad(Cell, Width, right) ->
    {Width - length(Cell), 0};
calc_cell_pad(Cell, Width, center) ->
    Diff = (Width - Cell) / 2,
    {mochinum:int_ceil(Diff), trunc(Diff)}.

truncate(Cell, Width, #table{truncate = Trunc}) ->
    do_trunc(Cell, {1, Width - length(Trunc)}, []) ++ Trunc.

do_trunc(_, {Width, Width}, Acc) ->
    lists:reverse(Acc);
do_trunc([C | Cs], {Now, Width}, Acc) ->
    do_trunc(Cs, {Now + 1, Width}, [C | Acc]).
