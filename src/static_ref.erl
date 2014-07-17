-module(static_ref).

-include("ast_helpers.hrl").

-export([
         compile/2
        ]).

%% =============================================================================
%%% API
%% =============================================================================

-spec compile(Module, [{RefName, [{K, V}]}]) -> ok when
      Module :: atom(),
      RefName :: binary(),
      K :: any(),
      V :: any().
compile(Module, Description) ->

    UnknownClause =
        ?clause([?underscore], none, [?error(?atom(unknown))]),
    Funs =
        lists:flatten(
          [
           [?function(
               from(RefName),
               [
                ?clause([?abstract(K)], none, [?ok(?abstract(V))])
                || {K, V} <- Data
               ] ++ [UnknownClause]),
            ?function(
               to(RefName),
               [
                ?clause([?abstract(V)], none, [?ok(?abstract(K))])
                || {K, V} <- Data
               ] ++ [UnknownClause])
           ] || {RefName, Data} <- Description
          ]),
    Ast =
        [
         ?attribute(module, [?atom(Module)]),
         ?export_funs(Funs),
         Funs
        ],
    Ast2 = [erl_syntax:revert(A) || A <- lists:flatten(Ast)],
    {ok, Module, BinaryCode} = compile:forms(Ast2),
    {module, Module} = code:load_binary(Module, "nofile", BinaryCode),
    ok.

%% =============================================================================
%%% Internal functions
%% =============================================================================

binary_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

from(RefName) -> binary_to_atom(<<"from_", RefName/binary>>).
to(RefName) -> binary_to_atom(<<"to_", RefName/binary>>).

%% =============================================================================
%%% Tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compile_test_() ->
    Module = ad_refs,
    Ref1 =
        {<<"status">>,
         [
          {blocked, 1},
          {full_access, 2}
         ]},
    Ref2 =
        {<<"ref">>,
         [
          {1, a},
          {2, b},
          {3, c}
         ]},
    Refs = [Ref1, Ref2],
    lists:flatten(
      [
       {"Init test", fun() -> ok = static_dict:compile(Module, Refs) end},
       [
        [
         fun() -> ?assertEqual({ok, V}, Module:(from(RefName))(K)) end,
         fun() -> ?assertEqual({ok, K}, Module:(to(RefName))(V)) end
        ] || {RefName, Data} <- Refs, {K, V} <- Data
       ],
       [
        [
         fun() -> ?assertEqual({error, unknown}, Module:(from(RefName))(z)) end,
         fun() -> ?assertEqual({error, unknown}, Module:(to(RefName))(z)) end
        ] || {RefName, _Data} <- Refs
       ]
      ]).

-endif.
