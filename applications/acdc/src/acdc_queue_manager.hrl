-ifndef(ACDC_QUEUE_MANAGER_HRL).

%% rr :: Round Robin
%% mi :: Most Idle
-type queue_strategy() :: 'rr' | 'mi'.

-record(state, {ignored_member_calls = dict:new() :: dict:dict()
               ,account_id :: api_binary()
               ,queue_id :: api_binary()
               ,supervisor :: pid()
               ,strategy = 'rr' :: queue_strategy() % round-robin | most-idle
               ,strategy_state :: strategy_state() % based on the strategy
               ,known_agents = dict:new() :: dict:dict() % how many agent processes are available {AgentId, Count}
               ,enter_when_empty = 'true' :: boolean() % allow caller into queue if no agents are logged in
               ,moh :: api_binary()
               ,current_member_calls = [] :: list() % ordered list of current members waiting
               }).
-type mgr_state() :: #state{}.

-type queue_strategy_state() :: queue:queue() | ne_binaries().
-type ss_details() :: {non_neg_integer(), 'busy' | 'undefined'}.
-record(strategy_state, {agents :: queue_strategy_state()
                                   %% details include # of agent processes and availability
                        ,details = dict:new() :: dict:dict(ne_binary(), ss_details())
                        }).
-type strategy_state() :: #strategy_state{}.

-define(ACDC_QUEUE_MANAGER_HRL, 'true').
-endif.
