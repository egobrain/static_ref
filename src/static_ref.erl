-module(static_ref).

-include("ast_helpers.hrl").

-export([
         compile/2
        ]).

-record(opts, {
          validators = false
         }).

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

    Description2 = lists:map(fun opts/1, Description),

    Funs =
        lists:flatten(
          [
           [
            ?function(
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
               ] ++ [UnknownClause]),
            case Opts#opts.validators of
                true -> validators(RefName, Data);
                false -> []
            end
           ] || {RefName, Data, Opts} <- Description2
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

validators(RefName, Data) ->
    UnknownClause =
        ?clause([?underscore], none, [?error(?atom(unknown))]),
    [
     ?function(
         valid_key(RefName),
         [
          ?clause([?abstract(K)], none, [?atom(ok)])
          || {K, _V} <- Data
         ] ++ [UnknownClause]),
     ?function(
        valid_value(RefName),
        [
         ?clause([?abstract(V)], none, [?atom(ok)])
         || {_K, V} <- Data
        ] ++ [UnknownClause])
    ].

%% =============================================================================
%%% Internal functions
%% =============================================================================


opts({RefName, Data}) -> {RefName, Data, #opts{}};
opts({RefName, Data, Opts}) ->
    Opts2 = lists:foldl(fun set_opt/2, #opts{}, Opts),
    {RefName, Data, Opts2}.

set_opt(validators, Opts) -> Opts#opts{validators=true};
set_opt(_, Opts) -> Opts.

binary_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

from(RefName) -> binary_to_atom(<<"from_", RefName/binary>>).
to(RefName) -> binary_to_atom(<<"to_", RefName/binary>>).

valid_key(RefName) -> binary_to_atom(<<"valid_", RefName/binary, "_key">>).
valid_value(RefName) -> binary_to_atom(<<"valid_", RefName/binary, "_value">>).

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
         ], []},
    Ref2 =
        {<<"ref">>,
         [
          {1, a},
          {2, b},
          {3, c}
         ], [validators]},
    Refs = [Ref1, Ref2],
    lists:flatten(
      [
       {"Init test",
        fun() ->
                ok = static_ref:compile(Module, Refs),
                ?assertEqual({file, "nofile"}, code:is_loaded(Module))
        end},
       [
        [
         fun() -> ?assertEqual({ok, V}, Module:(from(RefName))(K)) end,
         fun() -> ?assertEqual({ok, K}, Module:(to(RefName))(V)) end
        ] || {RefName, Data, _} <- Refs, {K, V} <- Data
       ],
       [
        [
         fun() -> ?assertEqual({error, unknown}, Module:(from(RefName))(z)) end,
         fun() -> ?assertEqual({error, unknown}, Module:(to(RefName))(z)) end
        ] || {RefName, _Data, _} <- Refs
       ],
       %% Validators
       [
        [
         [
          fun() ->
                  ?assertEqual(
                     false,
                     erlang:function_exported(Module, valid_key(RefName), 1)),
                  ?assertEqual(
                     false,
                     erlang:function_exported(Module, valid_value(RefName), 1))
          end
         ] || {RefName, _Data, _Opts} <- [Ref1]
        ],
        [
         [
          fun() ->
                  ?assertEqual(
                     true,
                     erlang:function_exported(Module, valid_key(RefName), 1)),
                  ?assertEqual(
                     true,
                     erlang:function_exported(Module, valid_value(RefName), 1))
          end,
          [
           fun() ->
                   ?assertEqual(ok, Module:(valid_key(RefName))(K)),
                   ?assertEqual(ok, Module:(valid_value(RefName))(V))
           end  || {K, V} <- Data
          ],
          fun() ->
                  ?assertEqual(
                     {error, unknown},
                     Module:(valid_key(RefName))(z)),
                  ?assertEqual(
                     {error, unknown},
                     Module:(valid_value(RefName))(z))
          end
         ] || {RefName, Data, _Opts} <- [Ref2]
        ]
       ]
      ]).

-endif.
