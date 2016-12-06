%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% Simple & efficient operations on CSV binaries.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_csv).

%% Public API
-export([count_rows/1
        ,fold/3
        ,take_row/1
        ,split_row/1
        ,pad_row_to/2
        ,associator/3
        ,row_to_iolist/1
        ,json_to_iolist/1
        ]).

-include_lib("kazoo/include/kz_types.hrl").

-ifndef(ZILCH).
-define(ZILCH, 'undefined').
-endif.

-type cell() :: ne_binary() | ?ZILCH.
-type row() :: [cell()].

-export_type([cell/0
             ,row/0
             ,folder/1
             ,fassoc/0
             ,verifier/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return count of rows minus the first one.
%% Returns 0 if a row is longer/smaller than the header row.
%% @end
%%--------------------------------------------------------------------
-spec count_rows(binary()) -> non_neg_integer().
count_rows(<<>>) -> 0;
count_rows(CSV) when is_binary(CSV) ->
    try fold(CSV, fun throw_bad/2, {-1,0}) of
        {_, TotalRows} -> TotalRows
    catch
        'throw':'bad_csv' -> 0
    end.

-spec throw_bad(row(), {integer(), non_neg_integer()}) -> {integer(), non_neg_integer()}.
throw_bad(Header, {-1,0}) ->
    case lists:all(fun is_binary/1, Header) of
        %% Strip header line from total rows count
        'true' ->
            {length(Header), 0};
        'false' ->
            io:format("bad header: ~p~n", [Header]),
            throw('bad_csv')
    end;
throw_bad(Row, {MaxRow,RowsCounted}) ->
    case length(Row) of
        MaxRow -> {MaxRow, RowsCounted+1};
        _ ->
            io:format("bad row: ~p~n", [Row]),
            throw('bad_csv')
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type folder(T) :: fun((row(), T) -> T).
-spec fold(binary(), folder(T), T) -> T when
      T :: any().
fold(CSV, Fun, Acc)
  when is_binary(CSV), is_function(Fun, 2) ->
    case take_row(CSV) of
        'eof' -> Acc;
        {Row, CSVRest} ->
            NewAcc = Fun(Row, Acc),
            fold(CSVRest, Fun, NewAcc)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec take_row(binary()) -> {row(), binary()} |
                            'eof'.
take_row(<<>>) -> 'eof';
take_row(CSV=?NE_BINARY) ->
    case split_row(CSV) of
        {[], <<>>} -> 'eof';
        {Row, Rest} -> {Row, Rest}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec split_row(ne_binary()) -> {row(), binary()}.
split_row(Row=?NE_BINARY) ->
    split_fields(Row, []).

-spec split_fields(binary(), [binary()]) -> {row(), binary()}.
split_fields(<<>>, Fields) ->
    {lists:reverse(Fields), <<>>};

split_fields(<<"\n\r", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};
split_fields(<<"\r\n", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};
split_fields(<<"\r\r", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};

split_fields(<<"\n", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};
split_fields(<<"\r", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};

split_fields(<<$", Row/binary>>, Fields) ->
    split_field(Row, $", Fields);
split_fields(<<$', Row/binary>>, Fields) ->
    split_field(Row, $', Fields);
split_fields(Row, Fields) ->
    split_field(Row, $,, Fields).

-type field_terminator() :: 34 | 39 | 44. %% $" | $' | $,

-spec split_field(binary(), field_terminator(), row()) ->
                         {row(), binary()}.
-spec split_field(binary(), field_terminator(), row(), [byte()]) ->
                         {row(), binary()}.
split_field(Row, EndChar, Fields) ->
    split_field(Row, EndChar, Fields, []).

split_field(<<$,>>, $,, Fields, FieldSoFar) ->
    split_fields(<<>>, [<<>>, iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<$,, Row/binary>>, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\n\r", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\r\n", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\r\r", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\n", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\r", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);

split_field(<<EndChar, EndChar, Row/binary>>, EndChar, Fields, FieldSoFar) ->
    split_field(Row, EndChar, Fields, [EndChar, EndChar | FieldSoFar]);
split_field(<<EndChar, $,, Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(Row, [Field | Fields]);

split_field(<<EndChar, "\r\n", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r\n", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\n", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\n", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\r\r", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r\r", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\r\n", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r\n", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\r", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r", Row/binary>>, [Field | Fields]);

split_field(<<>>, $,, Fields, FieldSoFar) ->
    split_fields(<<>>, [<<>>, iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<EndChar>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<>>, [Field | Fields]);

split_field(<<Char:1/binary>>, _EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse([Char | FieldSoFar])),
    split_fields(<<>>, [Field | Fields]);
split_field(<<Char:8, Row/binary>>, EndChar, Fields, FieldSoFar) ->
    split_field(Row, EndChar, Fields, [Char | FieldSoFar]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec pad_row_to(non_neg_integer(), row()) -> row().
pad_row_to(N, Row)
  when N > length(Row) ->
    Row ++ lists:duplicate(N - length(Row), ?ZILCH);
pad_row_to(_, Row) ->
    Row.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type fassoc_ret() :: {'true', row()} | 'false'.
-type fassoc() :: fun((row()) -> fassoc_ret()).
-type verifier() :: fun((atom(), cell()) -> boolean()).
-spec associator(row(), row(), verifier()) -> fassoc().
associator(CSVHeader, OrderedFields, Verifier) ->
    Max = length(OrderedFields),
    MappedHeaders = map_header_to_ordered_fields(CSVHeader, OrderedFields),
    fun(InputRow0) ->
            InputRow = pad_row_to(Max, InputRow0),
            ReOrdered =
                [maybe_include_cell(CellPosition, OrderedFields, Verifier, InputRow, MappedHeaders)
                 || CellPosition <- lists:seq(1, Max)
                ],
            case lists:any(fun is_boolean/1, ReOrdered) of
                'false' -> {'true', ReOrdered};
                'true' -> 'false'
            end
    end.

-spec maybe_include_cell(pos_integer(), row(), verifier(), row(), map()) ->
                                'false' | row().
maybe_include_cell(CellPosition, OrderedFields, Verifier, InputRow, MappedHeaders) ->
    Cell = find_cell_in_row(CellPosition, InputRow, MappedHeaders),
    Verifier(lists:nth(CellPosition, OrderedFields), Cell)
        andalso Cell.

find_cell_in_row(OrderedPosition, InputRow, MappedHeaders) ->
    case maps:get(OrderedPosition, MappedHeaders, 'undefined') of
        'undefined' -> ?ZILCH;
        CSVHeaderIndex -> lists:nth(CSVHeaderIndex, InputRow)
    end.

-spec map_header_to_ordered_fields(ne_binaries(), ne_binaries()) ->
                                          map().
map_header_to_ordered_fields(CSVHeader, OrderedFields) ->
    maps:from_list(
      [{find_ordered_position(Header, OrderedFields, 1), CSVHeaderIndex}
       || {CSVHeaderIndex,Header} <- lists:zip(lists:seq(1, length(CSVHeader)), CSVHeader)
      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec row_to_iolist(row()) -> iodata().
row_to_iolist([Cell]) -> cell_to_binary(Cell);
row_to_iolist(Row=[_|_]) ->
    kz_util:iolist_join($,, [cell_to_binary(Cell) || Cell <- Row]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Converts JSON-represented CSV data to binary.
%% We assume fields for first record are defined in all other records.
%% @end
%%--------------------------------------------------------------------
-spec json_to_iolist(nonempty_list(kz_json:object())) -> iodata().
json_to_iolist(Records)
  when is_list(Records) ->
    Tmp = <<"/tmp/json_", (kz_util:rand_hex_binary(11))/binary, ".csv">>,
    Fields = kz_json:get_keys(hd(Records)),
    'ok' = file:write_file(Tmp, [kz_util:iolist_join($,, Fields), $\n]),
    lists:foreach(fun (Record) ->
                          Row = [kz_json:get_ne_binary_value(Field, Record, ?ZILCH) || Field <- Fields],
                          _ = file:write_file(Tmp, [row_to_iolist(Row),$\n], ['append'])
                  end
                 ,Records
                 ),
    {'ok', IOData} = file:read_file(Tmp),
    kz_util:delete_file(Tmp),
    IOData.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec find_ordered_position(ne_binary(), ne_binaries(), non_neg_integer()) ->
                                   non_neg_integer().
find_ordered_position(_Item, [], _Pos) -> 0;
find_ordered_position(Item, [Item|_], Pos) -> Pos;
find_ordered_position(Item, [_|Items], N) ->
    find_ordered_position(Item, Items, N+1).

%% @private
-spec cell_to_binary(cell()) -> binary().
cell_to_binary(?ZILCH) -> <<>>;
cell_to_binary(Cell=?NE_BINARY) ->
    binary:replace(Cell, <<$,>>, <<$;>>, ['global']).


%%% End of Module.
