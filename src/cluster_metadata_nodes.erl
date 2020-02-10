-module(cluster_metadata_nodes).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([up_nodes/0,
         callback/1]).

-define(SERVER, ?MODULE).

-record(state, {
          nodes :: list(),
          broadcast_fun :: function()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(BroadcastFun) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [BroadcastFun], []).

up_nodes() ->
    [erlang:node()|erlang:nodes()].

callback(Fun) ->
    gen_server:call(?MODULE, {callback, Fun}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([BroadcastFun]) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, #state{broadcast_fun = BroadcastFun, nodes = up_nodes()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({callback, Fun}, _From, State) ->
    {reply, ok, State#state{broadcast_fun = Fun}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({nodedown, Node}, State = #state{broadcast_fun = BroadcastFun,
                                             nodes = Nodes}) ->
    BroadcastFun(Nodes -- [Node]),
    {noreply, State};

handle_info({nodeup, Node}, State = #state{broadcast_fun = BroadcastFun,
                                           nodes = Nodes}) ->
    BroadcastFun([Node|Nodes]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
