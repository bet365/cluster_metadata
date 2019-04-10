-module(cluster_metadata_broadcast_handler).

%% Return a two-tuple of message id and payload from a given broadcast
-callback broadcast_data(any()) -> {any(), any()}.

%% Given the message id and payload, merge the message in the local state.
%% If the message has already been received return `false', otherwise return `true'
-callback merge(any(), any()) -> boolean().

%% Return true if the message (given the message id) has already been received.
%% `false' otherwise
-callback is_stale(any()) -> boolean().

%% Return the message associated with the given message id. In some cases a message
%% has already been sent with information that subsumes the message associated with the given
%% message id. In this case, `stale' is returned.
-callback graft(any()) -> stale | {ok, any()} | {error, any()}.

%% Trigger an exchange between the local handler and the handler on the given node.
%% How the exchange is performed is not defined but it should be performed as a background
%% process and ensure that it delivers any messages missing on either the local or remote node.
%% The exchange does not need to account for messages in-flight when it is started or broadcast
%% during its operation. These can be taken care of in future exchanges.
-callback exchange(node()) -> {ok, pid()} | {error, term()}.
