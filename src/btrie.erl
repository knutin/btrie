-module(btrie).
-compile([export_all]).

-define(MAGIC, "btrie").
-define(VALUE, "v").


-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").



new() ->
    <<?MAGIC>>.

size_mb(B) ->
    size(B) / 1024 / 1024.

node(K, Value, Children) ->
    <<K:8,
      (size(Value)):32/integer, Value/binary,
      (size(Children)):32/integer, Children/binary>>.


find(<<?MAGIC, B/binary>>, Key) ->
    case find(B, Key) of
        <<>> ->
            not_found;
        Value ->
            Value
    end;


find(<<K:8,
       ValueSize:32/integer, Value:ValueSize/binary,
       ChildrenSize:32/integer, _:ChildrenSize/binary, _/binary>>, <<K:8>>) ->
    Value;

find(<<>>, _Key) ->
    <<>>;

find(<<K,
       ValueSize:32/integer, _:ValueSize/binary,
       ChildrenSize:32/integer, Children:ChildrenSize/binary, _/binary>>,
     <<K, KeyRest/binary>>) ->
    find(Children, KeyRest);


find(<<_:8,
       ValueSize:32/integer, _:ValueSize/binary,
       ChildrenSize:32/integer, _:ChildrenSize/binary, Siblings/binary>>,
     Key) ->
    find(Siblings, Key).


insert(<<?MAGIC, B/binary>>, Key, Value) when is_binary(Key) andalso is_binary(Value) ->
    <<?MAGIC, (insert(B, Key, Value))/binary>>;

insert(<<>>, <<Key>>, Value) ->
    node(Key, Value, <<>>);


insert(<<>>, <<K:8, KeyRest/binary>>, Value) ->
    node(K, <<>>, insert(<<>>, KeyRest, Value));


insert(<<K,
         ValueSize:32/integer, _OldValue:ValueSize/binary,
         ChildrenSize:32/integer, Children:ChildrenSize/binary, Siblings/binary>>,
       <<K>>, Value) ->
    <<K,
      (size(Value)):32/integer, Value/binary,
      ChildrenSize:32/integer, Children/binary, Siblings/binary>>;

insert(<<NodeK,
         ValueSize:32/integer, NodeValue:ValueSize/binary,
         ChildrenSize:32/integer, Children:ChildrenSize/binary, Siblings/binary>>,
       <<K, KeyRest/binary>> = Key, Value) ->

    if
        NodeK < K ->
            <<NodeK, ValueSize:32, NodeValue/binary,
              ChildrenSize:32, Children/binary,
              (insert(Siblings, Key, Value))/binary>>;
        NodeK > K ->
            <<(insert(<<>>, Key, Value))/binary,
              NodeK, ValueSize:32, NodeValue/binary,
              ChildrenSize:32, Children/binary, Siblings/binary>>;
        NodeK =:= K ->
            NewChildren = insert(Children, KeyRest, Value),
            <<NodeK, ValueSize:32, NodeValue/binary,
              (size(NewChildren)):32/integer, NewChildren/binary, Siblings/binary>>
    end.




delete(<<?MAGIC, B/binary>>, Key) when is_binary(Key) ->
    <<?MAGIC, (delete(B, Key))/binary>>;

delete(<<K,
         ValueSize:32/integer, _:ValueSize/binary,
         ChildrenSize:32/integer, Children:ChildrenSize/binary, Siblings/binary>>,
       <<K>>) ->
    <<K, 0:32, ChildrenSize:32, Children/binary, Siblings/binary>>;

delete(<<K,
         ValueSize:32/integer, Value:ValueSize/binary,
         ChildrenSize:32/integer, Children:ChildrenSize/binary, Siblings/binary>>,
       <<K, KeyRest/binary>>) ->
    NewChildren = delete(Children, KeyRest),
    <<K,
      ValueSize:32/integer, Value/binary,
      (size(NewChildren)):32/integer, NewChildren/binary, Siblings/binary>>;

delete(<<K,
         ValueSize:32/integer, Value:ValueSize/binary,
         ChildrenSize:32/integer, Children:ChildrenSize/binary, Siblings/binary>>,
       Key) ->
    NewSiblings = delete(Siblings, Key),
    <<K,
      ValueSize:32, Value/binary,
      ChildrenSize:32, Children/binary, NewSiblings/binary>>.




print(<<?MAGIC, B/binary>>) ->
    print(B);

print(<<>>) ->
    [];

print(<<K,
        ValueSize:32/integer, Value:ValueSize/binary,
        ChildrenSize:32/integer, Children:ChildrenSize/binary,
        Siblings/binary>>) ->

    [{<<K>>, Value, print(Children)} | print(Siblings)].


from_list(L) ->
    lists:foldl(fun ({K, V}, T) -> insert(T, K, V) end,
                new(), L).



%%
%% TESTS
%%


insert_test() ->
    L = [{<<"ab">>, <<"b">>}, {<<"aa">>, <<"a">>}, {<<"ac">>, <<"c">>},
         {<<"abc">>, <<"abc">>}, {<<"ac">>, <<"ac">>}],

    %%error_logger:info_msg("~p~n", [print(from_list(L))]),
    ?assertEqual(<<"abc">>, find(from_list(L), <<"abc">>)),
    ?assertEqual(<<"ac">>, find(from_list(L), <<"ac">>)).

delete_test() ->
    L = [{<<"ab">>, <<"b">>}, {<<"aa">>, <<"a">>}, {<<"ac">>, <<"c">>},
         {<<"abc">>, <<"abc">>}],
    ?assertEqual(not_found, find(delete(from_list(L), <<"abc">>), <<"abc">>)).


size_test() ->
    NumKeys = 10000,

    Start = 10000000,
    L = lists:map(fun (I) -> {list_to_binary(integer_to_list(I)), <<0>>} end,
                  lists:seq(Start, Start + NumKeys)),
    StartTime = now(),
    B = from_list(L),
    timer:now_diff(now(), StartTime) / 1000.



%%
%% PROPER
%%

prop_test() ->
    ?assert(proper:quickcheck(prop_insert())),
    ?assert(proper:quickcheck(prop_delete())).


prop__key() ->
    non_empty(binary(2)).


prop__value() ->
    non_empty(binary(1)).

prop__pair() ->
    {prop__key(), prop__value()}.


prop__unique(G) ->
    ?LET(L, G, lists:ukeysort(1, L)).

prop__pairs() ->
    prop__unique(?SIZED(Size, prop__pairs(Size-1))).

prop__pairs(0) ->
    [prop__pair()];
prop__pairs(S) ->
    [prop__pair() | prop__pairs(S-1)].


prop_insert() ->
    ?FORALL(L, prop__pairs(),
            begin
                [{K, V} | _] = L,
                B = from_list(L),
                find(B, K) =:= V
            end).

prop_delete() ->
    ?FORALL(L, prop__pairs(),
            begin
                [{K, V} | _] = L,
                B = from_list(L),
                find(B, K) =:= V andalso
                    find(delete(B, K), K) =:= not_found
            end).
