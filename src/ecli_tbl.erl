-module(ecli_tbl).

-export([print/2]).

-include("ecli.hrl").

-define(CHARS, 
        [
         {"top", "─"}, %% 9472
         {"top-mid", "┬"}, %% 9516
         {"top-left", "┌"}, %% 9484
         {"top-right", "┐"}, %% 9488
         {"bottom", "─"}, %% 9472
         {"bottom-mid", "┴"}, %% 9524
         {"bottom-left", "└"}, %% 9492
         {"bottom-right", "┘"}, %% 9496
         {"left", "│"}, %% 9474
         {"left-mid", "├"}, %% 9500
         {"mid", "─"}, %% 9572
         {"mid-mid", "┼"}, %% 9532
         {"right", "│"}, %% 9474
         {"right-mid", "┤"}, %% 9508
         {"middle", "│"} %% 9474
        ]).

-define(nth(I, L), lists:nth(I, L)).
-define(dup(N, C), lists:duplicate(N, C)).
-define(pad(N), ?dup(N, $\s)).

-define(VALID_ALIGN(A), A == left orelse A == right orelse A == center).

-define(VALID_COL(Col), 
        is_tuple(Col) andalso
        tuple_size(Col) == 4 andalso
        (?VALID_ALIGN(element(1, Col))) andalso
        is_integer(element(2, Col)) andalso
        element(2, Col) >= 2 andalso %% minimum column width: 2 
        is_integer(element(3, Col)) andalso
        is_integer(element(4, Col))).

-type head() :: atom() | binary().

-type column() :: align() | 
                  {align(), width()} | 
                  {align(), width(), pad_left(), pad_right()}.
-type align() :: left | right | center.
-type width() :: integer() | auto.
-type pad_left() :: integer().
-type pad_right() :: integer().

-record(table, {
          chars = ?CHARS :: [{string(), string()}],
          width = 0 :: integer(),
          columns = [] :: [column()],
          heads = [] :: [head()],
          truncate = [8230] :: string(), %% "…"
          rows = [],
          compact = false :: boolean(),
          debug = false :: boolean()
         }).

%% ===================================================================
%% Public
%% ===================================================================

print(Rows, Opts) ->
    do_print(init_opts(Opts, #table{rows = Rows})).

%% ===================================================================
%% Private
%% ===================================================================

init_opts([{chars, Chars} | Rest], T) ->
    init_opts(Rest, T#table{chars = Chars});
init_opts([{columns, Columns} | Rest], T) ->
    init_opts(Rest, T#table{columns = Columns});
init_opts([{heads, Heads} | Rest], T) ->
    init_opts(Rest, T#table{heads = Heads});
init_opts([debug | Rest], T) ->
    init_opts(Rest, T#table{debug = true});
init_opts([compact | Rest], T) ->
    init_opts(Rest, T#table{compact = true});
init_opts([_ | Rest], T) ->
    init_opts(Rest, T);
init_opts([], T) ->
    init_rows(T).

init_rows(#table{rows = [R | _] = Rows} = T) when is_list(R) -> 
    Rows1 = [[{to_b(K), V} || {K, V} <- Row] || Row <- Rows],
    init_heads(T#table{rows = Rows1});
init_rows(#table{rows = [{_, _} | _] = Rows} = T) ->
    Rows1 = [[K, V] || {K, V} <- Rows],
    init_columns(T#table{rows = Rows1, 
                         columns = [left,left], 
                         heads = [dummy,dumy]}).

init_heads(#table{heads = [_|_] = Heads} = T) ->
    align_rows(T#table{heads = [to_b(H) || H <- Heads]});
init_heads(#table{rows = Rows} = T) ->
    Keys = lists:foldl(
             fun(Row, Acc) -> 
                     proplists:get_keys(Row) ++ Acc
             end, [], Rows),
    align_rows(T#table{heads = lists:usort([to_b(H) || H <- Keys])}).

align_rows(#table{heads = Heads, rows = Rows} = T) ->
    Rows1 = [align_row(Row, Heads) || Row <- Rows],
    init_columns(T#table{rows = [Heads | Rows1]}).

align_row(Row, Heads) ->
    [proplists:get_value(Key, Row, "") || Key <- Heads].

init_columns(#table{columns = [], heads = Heads} = T) ->
    init_columns(T#table{columns = [left || _ <- Heads]});
init_columns(#table{columns = Columns, heads = Heads} = T) 
  when length(Columns) == length(Heads) ->
    Columns1 = [init_column(I, T) || I <- lists:seq(1, num_cols(T))],
    calc_width(T#table{columns = Columns1});
init_columns(_) ->
    throw({error, invalid_columns}).

init_column(I, #table{columns = Cols, rows = Rows}) ->
    init_col(?nth(I, Cols), [lists:nth(I, Row) || Row <- Rows]).

init_col(Align, Vals) when is_atom(Align) ->
    valid_col({Align, calc_vals_width(Vals), 1, 1});
init_col({Align, auto}, Vals) ->
    valid_col({Align, calc_vals_width(Vals), 1, 1});
init_col({Align, Width}, _) ->
    valid_col({Align, Width, 1, 1});
init_col({Align, auto, PL, PR}, Vals) ->
    valid_col({Align, calc_vals_width(Vals), PL, PR});
init_col(Col, _) ->
    valid_col(Col).

valid_col(Col) when ?VALID_COL(Col) ->
    Col;
valid_col(Col) ->
    throw({error, {invalid_column, Col}}).

col_width({_, W, PL, PR}) -> W + PL + PR.

calc_vals_width(Vals) ->
    lists:max([get_width(V) || V <- Vals]).

%% TODO: handle unicode
get_width(Val) ->
    length(to_l(Val)).

calc_width(#table{columns = Columns} = T) ->
    Width = lists:foldl(
              fun(Col, Acc) -> 
                      col_width(Col) + Acc 
              end, length(Columns) + 1, Columns),
    T#table{width = Width}.

%% ===================================================================
%% Drawing functions
%% ===================================================================

print_info(#table{rows = Rows, columns = Columns, debug = true,
                  width = Width, heads = Heads} = T) ->
    ?PRINT("heads: ~p~n", [Heads]),
    ?PRINT("columns: ~p~n", [Columns]),
    ?PRINT("checkpoints: ~p~n", [checkpoints(T)]),
    ?PRINT("width: ~p~n", [Width]),
    [?PRINT("r: ~p ~n", [Row]) || Row <- Rows];
print_info(_) ->
    nop.

do_print(#table{rows = Rows} = T) ->
    print_info(T),
    print_line_top(T),
    print_rows(Rows, T),
    print_line_bottom(T).

print_rows([Cells], T) ->
    print_cells(Cells, T);
print_rows([Cells | Rs], T) ->
    print_cells(Cells, T),
    print_line_mid(T),
    print_rows(Rs, T).

print_cells(Cells, #table{columns = Cols} = T) ->
    [print_cell(to_l(Cell), Col, T) || {Cell, Col} <- lists:zip(Cells, Cols)],
    ?PRINT("~ts~n",[c("right", T)]).

print_line_top(T) -> 
    print_line(line_chars(top, T), T).

print_line_mid(T) -> 
    print_line(line_chars(middle, T), T).

print_line_bottom(T) -> 
    print_line(line_chars(bottom, T), T).

line_chars(top, T) ->
    {c("top",T),c("top-left",T),c("top-right",T),c("top-mid",T)};
line_chars(middle, T) ->
    {c("mid",T),c("left-mid",T),c("right-mid",T),c("mid-mid",T)};
line_chars(bottom, T) ->
    {c("bottom",T),c("bottom-left",T),c("bottom-right",T),c("bottom-mid",T)}.

print_line(_, #table{compact = true}) ->
    nop;
print_line(Chars, #table{width = Width} = T) ->
    draw_line(Chars, {1, Width}, checkpoints(T)).

draw_line({_, Left, _, _} = Chars, {1, Width}, CheckPoints) ->
    ?PRINT("~ts", [Left]),
    draw_line(Chars, {2, Width}, CheckPoints);
draw_line({_, _, Right, _}, {Width, Width}, _)  ->
    ?PRINT("~ts~n", [Right]);
draw_line({_, _, _, Inter} = Chars, {Now, Width}, [Now | CheckPoints]) ->
    ?PRINT("~ts", [Inter]),
    draw_line(Chars, {Now + 1, Width}, CheckPoints);
draw_line({Line, _, _, _} = Chars, {Now, Width}, CheckPoints) ->
    ?PRINT("~ts", [Line]),
    draw_line(Chars, {Now + 1, Width}, CheckPoints).

print_cell(Cell, Col, T) ->
    ?PRINT("~ts~ts", [c("left",T), fmt_cell(Cell, Col, T)]).

fmt_cell(Cell, {Align, Width, PL, PR}, _) when length(Cell) =< Width ->
    {PadL, PadR} = calc_cell_pad(Cell, Width, Align),
    ?pad(PadL + PL) ++ Cell ++ ?pad(PadR + PR);
fmt_cell(Cell, {_, Width, PL, PR}, T) ->
    ?pad(PL) ++ truncate(Cell, Width, T) ++ ?pad(PR).

checkpoints(#table{columns = Columns}) ->
    {CheckPoints, _} = lists:foldl(
      fun(Col, {CW, Acc}) -> 
              P = col_width(Col) + Acc + 1,
              {[P | CW], P}
      end, {[], 1}, Columns),
    lists:reverse(CheckPoints).

to_b(V) when is_atom(V) -> list_to_binary(atom_to_list(V));
to_b(V) when is_list(V) -> list_to_binary(V);
to_b(V) when is_binary(V) -> V;
to_b(_) -> exit(badarg).

to_l(V) when is_float(V); is_integer(V) -> mochinum:digits(V);
to_l(V) when is_binary(V) -> binary_to_list(V);
to_l(V) when is_atom(V) -> atom_to_list(V);
to_l(V) when is_list(V) -> V;
to_l(_) -> exit(badarg).

c(_, #table{compact = true}) -> 
    "";
c(C, #table{chars = Chars}) -> 
    proplists:get_value(C, Chars).

num_cols(#table{heads = Heads}) -> length(Heads).

calc_cell_pad(Cell, Width, left) ->
    {0, Width - length(Cell)};
calc_cell_pad(Cell, Width, right) ->
    {Width - length(Cell), 0};
calc_cell_pad(Cell, Width, center) ->
    Diff = (Width - length(Cell)) / 2,
    {mochinum:int_ceil(Diff), trunc(Diff)}.

truncate(Cell, Width, #table{truncate = Trunc}) ->
    do_trunc(Cell, {1, Width - length(Trunc) + 1}, []) ++ Trunc.

do_trunc(_, {Width, Width}, Acc) ->
    lists:reverse(Acc);
do_trunc([C | Cs], {Now, Width}, Acc) ->
    do_trunc(Cs, {Now + 1, Width}, [C | Acc]).
