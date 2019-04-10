%%%-------------------------------------------------------------------
%% @doc cluster_metadata top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cluster_metadata_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    DataDir = application:get_env(cluster_metadata, data_dir, default_data_dir()),
    Opts = [{data_dir, DataDir}],

    Manager = #{id => cluster_metadata_manager, 
                start => {cluster_metadata_manager, start_link, [Opts]}},
    Hashtree = #{id => cluster_metadata_hashtree, 
                 start => {cluster_metadata_hashtree, start_link, []}},

    {ok, {{one_for_all, 0, 1}, [Manager, Hashtree]}}.

%%====================================================================
%% Internal functions
%%====================================================================

default_data_dir() ->
    atom_to_list(node()).
