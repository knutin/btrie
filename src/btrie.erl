-module(btrie).
-compile([export_all]).


-define(MAGIC, "btrie").
-define(VALUE, "v").


-include_lib("eunit/include/eunit.hrl").



%% new() ->
%%     <<?MAGIC>>.

%% insert(<<?MAGIC, B/binary>>, K, V) ->

node(K, Children) ->
    <<K:8, (size(Children)):32/integer, Children/binary>>.


leaf(K, Value) ->
    <<K:8, (size(Value)+1):32/integer, (<<?VALUE, Value/binary>>)/binary>>.


new() ->
    Tree = node($a,
                <<
                  (leaf($a, <<"foo">>))/binary,
                  (leaf($b, <<"bar">>))/binary,
                  (node($c,
                        << (leaf($c, <<"quu">>))/binary >>
                       ))/binary
                >>
               ),

    <<?MAGIC, Tree/binary>>.



find(<<?MAGIC, B/binary>>, Key) ->
    case find(B, Key) of
        <<?VALUE, Value/binary>> ->
            Value;
        _ ->
            error(badarg)
    end;

find(<<K:8, Size:32/integer, Value:Size/binary, _/binary>>, <<K:8>>) ->
    Value;

find(<<>>, _Key) ->
    error(badarg);

find(<<K, ChildrenSize:32/integer, Children:ChildrenSize/binary, _/binary>>,
     <<K, KeyRest/binary>>) ->
    find(Children, KeyRest);


find(<<_:8, ChildrenSize:32/integer, _:ChildrenSize/binary, Siblings/binary>>,
     Key) ->
    find(Siblings, Key).


insert(<<?MAGIC, B/binary>>, Key, Value) when is_binary(Key) andalso is_binary(Value) ->
    insert(B, Key, Value);

insert(<<>>, <<Key>>, Value) ->
    node(Key, Value);

insert(<<?VALUE, _/binary>>, <<_Key>>, _Value) ->
    error(badarg);

insert(<<>>, <<K:8, KeyRest/binary>>, Value) ->
    node(K, insert(<<>>, KeyRest, Value));


insert(<<K:8, Size:32/integer, Children:Size/binary, Siblings/binary>>,
       <<K, KeyRest/binary>>, Value) ->
    NewChildren = insert(Children, KeyRest, Value),
    <<K, (size(NewChildren)):32, NewChildren/binary, Siblings/binary>>;

insert(<<K:8, Size:32/integer, Children:Size/binary, Siblings/binary>>,
       Key, Value) ->
    NewSiblings = insert(Siblings, Key, Value),
    <<K, Size:32, Children/binary, NewSiblings/binary>>.



delete(<<?MAGIC, B/binary>>, Key) when is_binary(Key) ->
    delete(B, Key);

delete(<<K:8, Size:32/integer, _:Size/binary, Siblings/binary>>, <<K:8>>) ->
    Siblings;

delete(<<K:8, Size:32/integer, Children:Size/binary, Siblings/binary>>,
       <<K, KeyRest/binary>>) ->
    NewChildren = delete(Children, KeyRest),
    <<K, (size(NewChildren)):32, NewChildren/binary, Siblings/binary>>;

delete(<<K:8, Size:32/integer, Children:Size/binary, Siblings/binary>>,
       Key) ->
    NewSiblings = delete(Siblings, Key),
    <<K, Size:32, Children/binary, NewSiblings/binary>>.




%%
%% TESTS
%%

find_test() ->
    ?assertEqual(<<"foo">>, find(new(), <<"aa">>)).

insert_test() ->
    ?assertEqual(<<"new">>, find(insert(new(), <<"def">>, <<"new">>), <<"def">>)),
    ?assertError(badarg, insert(new(), <<"aaa">>, <<"foo">>)).

delete_test() ->
    ?assertEqual(not_found, find(
                              delete(
                                insert(new(), <<"def">>, <<"new">>),
                                <<"def">>),
                              <<"def">>)).

