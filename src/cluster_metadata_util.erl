-module(cluster_metadata_util).

-export([build_tree/3,
         sha/1,
         md5/1,
         integer_to_list/2]).

%%%===================================================================
%%% API 
%%%===================================================================

%% @doc Convert a list of elements into an N-ary tree. This conversion
%%      works by treating the list as an array-based tree where, for
%%      example in a binary 2-ary tree, a node at index i has children
%%      2i and 2i+1. The conversion also supports a "cycles" mode where
%%      the array is logically wrapped around to ensure leaf nodes also
%%      have children by giving them backedges to other elements.

-spec build_tree(N :: integer(), Nodes :: [term()], Opts :: [term()])
                -> orddict:orddict().
build_tree(N, Nodes, Opts) ->
    case lists:member(cycles, Opts) of
        true ->
            Expand = lists:flatten(lists:duplicate(N+1, Nodes));
        false ->
            Expand = Nodes
    end,
    {Tree, _} =
        lists:foldl(fun(Elm, {Result, Worklist}) ->
                            Len = erlang:min(N, length(Worklist)),
                            {Children, Rest} = lists:split(Len, Worklist),
                            NewResult = [{Elm, Children} | Result],
                            {NewResult, Rest}
                    end, {[], tl(Expand)}, Nodes),
    orddict:from_list(Tree).

sha(Bin) ->
    crypto:hash(sha, Bin).

md5(Bin) ->
    crypto:hash(md5, Bin).

%%% @spec integer_to_list(Integer :: integer(), Base :: integer()) ->
%%          string()
%% @doc Convert an integer to its string representation in the given
%%      base.  Bases 2-62 are supported.
integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base)
  when is_integer(I), is_integer(Base),Base >= 2, Base =< 1+$Z-$A+10+1+$z-$a ->
    if I < 0 ->
            [$-|integer_to_list(-I, Base, [])];
       true ->
            integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).% @spec integer_to_list(integer(), integer(), string()) -> string()
integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 36 ->
		 [D-36+$a|R0];
	    D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


