%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_phone_numbers_v1).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,billing/1
        ,validate/1, validate/2, validate/3
        ,validate_request/1
        ,authorize/1
        ,authenticate/1
        ,put/2, put/3
        ,post/2
        ,delete/2
        ,summary/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(CB_LIST, <<"phone_numbers/crossbar_listing">>).

-define(ACTIVATE, <<"activate">>).
-define(RESERVE, <<"reserve">>).
-define(PORT, <<"port">>).

-define(CLASSIFIERS, <<"classifiers">>).
-define(IDENTIFY, <<"identify">>).
-define(COLLECTION, <<"collection">>).
-define(MIME_TYPES, [{<<"application">>, <<"pdf">>}
                    ,{<<"application">>, <<"x-gzip">>}
                    ,{<<"application">>, <<"zip">>}
                    ,{<<"application">>, <<"x-rar-compressed">>}
                    ,{<<"application">>, <<"x-tar">>}
                    ,{<<"image">>, <<"*">>}
                    ,{<<"text">>, <<"plain">>}
                    ,{<<"application">>, <<"base64">>}
                    ,{<<"application">>, <<"x-base64">>}
                    ]).
-define(PHONE_NUMBERS_CONFIG_CAT, <<"crossbar.phone_numbers">>).
-define(FIND_NUMBER_SCHEMA, "{\"$schema\": \"http://json-schema.org/draft-03/schema#\", \"id\": \"http://json-schema.org/draft-03/schema#\", \"properties\": {\"prefix\": {\"required\": \"true\", \"type\": \"string\", \"minLength\": 3, \"maxLength\": 10}, \"quantity\": {\"default\": 1, \"type\": \"integer\", \"minimum\": 1}}}").

-define(MAX_TOKENS, kapps_config:get_integer(?PHONE_NUMBERS_CONFIG_CAT, <<"activations_per_day">>, 100)).

-define(PREFIX, <<"prefix">>).
-define(COUNTRY, <<"country">>).
-define(OFFSET, <<"offset">>).
-define(QUANTITY, <<"quantity">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.phone_numbers">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.phone_numbers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.phone_numbers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.phone_numbers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.phone_numbers">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.phone_numbers">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.phone_numbers">>, ?MODULE, 'delete'),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

allowed_methods(?CLASSIFIERS) ->
    [?HTTP_GET];
allowed_methods(?COLLECTION) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_, ?ACTIVATE) ->
    [?HTTP_PUT];
allowed_methods(_, ?RESERVE) ->
    [?HTTP_PUT];
allowed_methods(_, ?PORT) ->
    [?HTTP_PUT];
allowed_methods(_, ?IDENTIFY) ->
    [?HTTP_GET];
allowed_methods(?COLLECTION, ?ACTIVATE) ->
    [?HTTP_PUT].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists() -> 'true'.

resource_exists(_) -> 'true'.

resource_exists(_, ?ACTIVATE) -> 'true';
resource_exists(_, ?RESERVE) -> 'true';
resource_exists(_, ?PORT) -> 'true';
resource_exists(_, ?IDENTIFY) -> 'true';
resource_exists(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for phone_numbers
%% @end
%%--------------------------------------------------------------------
-spec billing(cb_context:context()) -> cb_context:context().
-spec billing(cb_context:context(), req_verb(), req_nouns()) -> cb_context:context().
billing(Context) ->
    billing(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

billing(Context, ?HTTP_GET, [{<<"phone_numbers">>, _}|_]) -> Context;
billing(Context, _, [{<<"phone_numbers">>, _}|_]) ->
    try kz_services:allow_updates(cb_context:account_id(Context)) of
        'true' -> Context
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_util:to_binary(Error), 500, Reason, Context)
    end;
billing(Context, _Verb, _Nouns) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'true'.
-spec authenticate(http_method(), req_nouns()) -> 'true'.
authenticate(Context) ->
    authenticate(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authenticate(?HTTP_GET, [{<<"phone_numbers">>, []}]) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'true'.
-spec authorize(http_method(), req_nouns()) -> 'true'.
authorize(Context) ->
    authorize(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authorize(?HTTP_GET, [{<<"phone_numbers">>,[]}]) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate_1(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_2(cb_context:context(), http_method(), path_token()) -> cb_context:context().
-spec validate_3(cb_context:context(), http_method(), path_token(), path_token()) -> cb_context:context().

validate(Context) ->
    validate_1(Context, cb_context:req_verb(Context)).
validate_1(Context, ?HTTP_GET) ->
    case cb_context:account_id(Context) of
        'undefined' -> find_numbers(Context);
        _ -> summary(Context)
    end.

validate(Context, PathToken1) ->
    validate_2(Context, cb_context:req_verb(Context), PathToken1).
validate_2(Context, ?HTTP_PUT, ?COLLECTION) ->
    validate_request(Context);
validate_2(Context, ?HTTP_POST, ?COLLECTION) ->
    validate_request(Context);
validate_2(Context, ?HTTP_DELETE, ?COLLECTION) ->
    validate_delete(Context);
validate_2(Context, ?HTTP_GET, ?CLASSIFIERS) ->
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                            ,knm_converters:available_classifiers()
                            );
validate_2(Context, ?HTTP_GET, Number) ->
    read(Context, Number);
validate_2(Context, ?HTTP_POST, _Number) ->
    validate_request(Context);
validate_2(Context, ?HTTP_PUT, _Number) ->
    validate_request(Context);
validate_2(Context, ?HTTP_DELETE, _Number) ->
    validate_delete(Context).

validate(Context, PathToken1, PathToken2) ->
    validate_3(Context, cb_context:req_verb(Context), PathToken1, PathToken2).
validate_3(Context, ?HTTP_PUT, ?COLLECTION, ?ACTIVATE) ->
    validate_request(Context);
validate_3(Context, ?HTTP_PUT, _Number, ?ACTIVATE) ->
    case has_tokens(Context) of
        'true' -> validate_request(Context);
        'false' -> cb_context:add_system_error('too_many_requests', Context)
    end;
validate_3(Context, ?HTTP_PUT, _Number, ?RESERVE) ->
    validate_request(Context);
validate_3(Context, ?HTTP_PUT, _Number, ?PORT) ->
    validate_request(Context);
validate_3(Context, ?HTTP_GET, Number, ?IDENTIFY) ->
    identify(Context, Number).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?COLLECTION) ->
    set_response(collection_process(Context), <<>>, Context);
post(Context, Number) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ],
    JObj = cb_context:doc(Context),
    Result = knm_number:update(Number, [{fun knm_phone_number:reset_doc/2, JObj}], Options),
    set_response(Result, Number, Context).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ?COLLECTION) ->
    Results = collection_process(Context),
    set_response(Results, <<>>, Context);
put(Context, Number) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:create(Number, Options),
    set_response(Result, Number, Context).

put(Context, ?COLLECTION, ?ACTIVATE) ->
    Results = collection_process(Context, ?ACTIVATE),
    set_response(Results, <<>>, Context);
put(Context, Number, ?ACTIVATE) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = case knm_number:move(Number, cb_context:account_id(Context), Options) of
                 {'ok', KNum} ->
                     {'ok', kz_json:delete_key(<<"numbers">>, knm_number:to_public_json(KNum))};
                 _Else -> _Else
             end,
    set_response(Result, Number, Context);
put(Context, Number, ?RESERVE) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:reserve(Number, Options),
    set_response(Result, Number, Context);
put(Context, Number, ?PORT) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ,{'state', ?NUMBER_STATE_PORT_IN}
              ],
    Result = knm_number:create(Number, Options),
    set_response(Result, Number, Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?COLLECTION) ->
    Results = collection_process(Context),
    set_response(Results, <<>>, Context);
delete(Context, Number) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ],
    Result = knm_number:release(Number, Options),
    set_response(Result, Number, Context).

-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = crossbar_doc:load_view(?CB_LIST, [], rename_qs_filters(Context), fun normalize_view_results/2),
    ListOfNumProps = cb_context:resp_data(Context1),
    NumbersJObj = lists:foldl(fun kz_json:merge_jobjs/2, kz_json:new(), ListOfNumProps),
    Service = kz_services:fetch(cb_context:account_id(Context)),
    Quantity = kz_services:cascade_category_quantity(<<"phone_numbers">>, [], Service),
    NewRespData = kz_json:from_list([{<<"numbers">>, NumbersJObj}
                                    ,{<<"casquade_quantity">>, Quantity}
                                    ]),
    cb_context:set_resp_data(Context1, NewRespData).

%% @private
-spec rename_qs_filters(cb_context:context()) -> cb_context:context().
rename_qs_filters(Context) ->
    Renamer = fun (<<"filter_state">>, Value)       -> {<<"filter_pvt_state">>, Value};
                  (<<"filter_assigned_to">>, Value) -> {<<"filter_pvt_assigned_to">>, Value};
                  (<<"filter_locality">>, Value)    -> {<<"filter_pvt_locality">>, Value};
                  (K, V) -> {K, V}
              end,
    NewQS = kz_json:map(Renamer, cb_context:query_string(Context)),
    cb_context:set_query_string(Context, NewQS).

%% @private
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    Number = kz_json:get_value(<<"key">>, JObj),
    Properties = kz_json:get_value(<<"value">>, JObj),
    [kz_json:set_value(Number, Properties, kz_json:new())
     | Acc
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec identify(cb_context:context(), ne_binary()) -> cb_context:context().
identify(Context, Number) ->
    case knm_number:lookup_account(Number) of
        {'error', 'not_reconcilable'} ->
            cb_context:add_system_error(
              'bad_identifier'
                                       ,kz_json:from_list([{<<"cause">>, Number}])
                                       ,Context
             );
        {'error', E} ->
            set_response({kz_util:to_binary(E), <<>>}, Number, Context);
        {'ok', AccountId, Options} ->
            JObj = kz_json:set_values([{<<"account_id">>, AccountId}
                                      ,{<<"number">>, knm_number_options:number(Options)}
                                      ]
                                     ,kz_json:new()
                                     ),
            set_response({'ok', JObj}, Number, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Load an instance from the database
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Number) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ],
    Result = case knm_number:get(Number, Options) of
                 {'ok', KNum} -> {'ok', knm_number:to_public_json(KNum)};
                 {'error', _R}=Error -> Error
             end,
    set_response(Result, Number, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(cb_context:context()) -> cb_context:context().
find_numbers(Context) ->
    AccountId = cb_context:auth_account_id(Context),
    QS = cb_context:query_string(Context),
    Country = kz_json:get_ne_value(?COUNTRY, QS, ?KNM_DEFAULT_COUNTRY),
    Prefix = kz_util:remove_white_spaces(kz_json:get_ne_value(?PREFIX, QS)),
    Offset = kz_json:get_integer_value(?OFFSET, QS, 0),
    Token = cb_context:auth_token(Context),
    HashKey = <<AccountId/binary, "-", Token/binary>>,
    Hash = kz_base64url:encode(crypto:hash(sha, HashKey)),
    QueryId = list_to_binary([Country, "-", Prefix, "-", Hash]),
    Dialcode = knm_util:prefix_for_country(Country),
    NormalizedPrefix = <<Dialcode/binary, Prefix/binary>>,
    Options = props:filter_undefined(
                [{'quantity', max(1, kz_json:get_integer_value(?QUANTITY, QS, 1))}
                ,{'prefix', Prefix}
                ,{'normalized_prefix', NormalizedPrefix}
                ,{'country', Country}
                ,{'dialcode', Dialcode}
                ,{'offset', Offset}
                ,{'account_id', AccountId}
                ,{'reseller_id', cb_context:reseller_id(Context)}
                ,{'query_id', QueryId}
                ]),
    OnSuccess =
        fun(C) ->
                Found = knm_search:find(Options),
                cb_context:setters(C
                                  ,[{fun cb_context:set_resp_data/2, Found}
                                   ,{fun cb_context:set_resp_status/2, 'success'}
                                   ])
        end,
    Schema = kz_json:decode(?FIND_NUMBER_SCHEMA),
    Context1 = cb_context:set_req_data(Context, knm_carriers:options_to_jobj(Options)),
    cb_context:validate_request_data(Schema, Context1, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(cb_context:context()) -> cb_context:context().
validate_request(Context) ->
    cb_context:validate_request_data(<<"phone_numbers">>, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec validate_delete(cb_context:context()) -> cb_context:context().
validate_delete(Context) ->
    cb_context:set_doc(cb_context:set_resp_status(Context, 'success')
                      ,'undefined'
                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_response({'ok', kz_json:object()} |
                   kz_json:object() |
                   knm_number_return() |
                   {binary(), binary()}
                  ,binary()
                  ,cb_context:context()
                  ) ->
                          cb_context:context().
set_response({'ok', {'ok', Doc}}, _, Context) ->
    crossbar_util:response(Doc, Context);
set_response({'ok', Doc}, _, Context) ->
    case knm_number:is_number(Doc) of
        'true' -> crossbar_util:response(knm_number:to_public_json(Doc), Context);
        'false' -> crossbar_util:response(Doc, Context)
    end;
set_response({'error', 'not_found'}, _, Context) ->
    Msg = kz_json:from_list([{<<"message">>, <<"bad identifier">>}
                            ,{<<"not_found">>, <<"The number could not be found">>}
                            ]),
    cb_context:add_system_error('bad_identifier', Msg, Context);
set_response({'error', Data}, _, Context) ->
    case kz_json:is_json_object(Data) of
        'true' ->
            Code = knm_errors:code(Data),
            Msg = knm_errors:error(Data),
            lager:debug("error ~p: ~p", [Code, Msg]),
            cb_context:add_system_error(Code, Msg, Data, Context);
        'false' ->
            lager:debug("error: ~p", [Data]),
            crossbar_util:response_400(<<"client error">>, Data, Context)
    end;
set_response({Error, Reason}, _, Context) ->
    crossbar_util:response('error', kz_util:to_binary(Error), 500, Reason, Context);
set_response(_Else, _, Context) ->
    lager:debug("unexpected response: ~p", [_Else]),
    cb_context:add_system_error('unspecified_fault', Context).

-spec collection_process(cb_context:context()) -> kz_json:object().
-spec collection_process(cb_context:context(), ne_binary() | ne_binaries()) -> kz_json:object().
-spec collection_process(cb_context:context(), ne_binary() | ne_binaries(), ne_binary()) -> kz_json:object().
collection_process(Context) ->
    Numbers = kz_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    collection_process(Context, Numbers).

collection_process(Context, ?ACTIVATE) ->
    Numbers = kz_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    collection_process(Context, Numbers, ?ACTIVATE);
collection_process(Context, Numbers) ->
    Temp = kz_json:from_list([{<<"success">>, kz_json:new()}
                             ,{<<"error">>, kz_json:new()}
                             ]),
    lists:foldl(fun(Number, Acc) ->
                        collection_process_fold(Number, Acc, Context)
                end
               ,Temp
               ,Numbers
               ).

collection_process(Context, Numbers, Action) ->
    Base = kz_json:from_list([{<<"success">>, kz_json:new()}
                             ,{<<"error">>, kz_json:new()}
                             ]
                            ),
    lists:foldl(fun(Number, Acc) -> collection_process_action_fold(Number, Acc, Context, Action) end
               ,Base
               ,Numbers
               ).

-spec collection_process_action_fold(ne_binary(), kz_json:object(), cb_context:context(), ne_binary()) ->
                                            kz_json:object().
collection_process_action_fold(Number, Acc, Context, Action) ->
    case collection_action(Context, cb_context:req_verb(Context), Number, Action) of
        {'ok', JObj} ->
            kz_json:set_value([<<"success">>, Number], JObj, Acc);
        {'error', KNMError} ->
            JObj = kz_json:set_value(<<"reason">>, knm_errors:cause(KNMError), kz_json:new()),
            kz_json:set_value([<<"error">>, Number], JObj, Acc)
    end.

-spec collection_process_fold(ne_binary(), kz_json:object(), cb_context:context()) ->
                                     kz_json:object().
collection_process_fold(Number, Acc, Context) ->
    case collection_action(Context, cb_context:req_verb(Context), Number) of
        {'ok', KNum} ->
            JObj = knm_number:to_public_json(KNum),
            kz_json:set_value([<<"success">>, Number], JObj, Acc);
        {'error', KNMError} ->
            JObj = kz_json:set_value(<<"reason">>, knm_errors:cause(KNMError), kz_json:new()),
            kz_json:set_value([<<"error">>, Number], JObj, Acc)
    end.

-spec collection_action(cb_context:context(), http_method(), ne_binary()) -> knm_number_return().
-spec collection_action(cb_context:context(), http_method(), ne_binary(), ne_binary()) ->
                               knm_number_return() |
                               {'ok', kz_json:object()}.

collection_action(Context, ?HTTP_PUT, Number) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'public_fields', kz_json:delete_key(<<"numbers">>, cb_context:doc(Context))}
              ],
    knm_number:create(Number, Options);
collection_action(Context, ?HTTP_POST, Number) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ],
    JObj = kz_json:delete_key(<<"numbers">>, cb_context:doc(Context)),
    knm_number:update(Number, [{fun knm_phone_number:reset_doc/2, JObj}], Options);
collection_action(Context, ?HTTP_DELETE, Number) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ],
    knm_number:release(Number, Options).

collection_action(Context, ?HTTP_PUT, Number, ?ACTIVATE) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ,{'public_fields', kz_json:delete_key(<<"numbers">>, cb_context:doc(Context))}
              ],
    case knm_number:move(Number, cb_context:account_id(Context), Options) of
        {'ok', KNum} ->
            kz_json:delete_key(<<"numbers">>, knm_number:to_public_json(KNum));
        _Else -> _Else
    end.

-spec has_tokens(cb_context:context()) -> boolean().
-spec has_tokens(cb_context:context(), pos_integer()) -> boolean().
has_tokens(Context) -> has_tokens(Context, 1).
has_tokens(Context, Count) ->
    Name = <<(cb_context:account_id(Context))/binary, "/", ?PHONE_NUMBERS_CONFIG_CAT/binary>>,
    Cost = cb_modules_util:token_cost(Context, Count),
    case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
        'true' -> 'true';
        'false' ->
            lager:warning("rate limiting activation limit reached, rejecting"),
            'false'
    end.
