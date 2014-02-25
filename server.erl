-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").


%Regisster client to server
loop(St, {connect,ClientPID,Nick}) ->
	ClientList = St#server_st.clients,
	case {isNickTaken(Nick, ClientList)} of
		{true} -> 
			NewSt = St,
			Result = {error, user_already_connected, "You are already connected to this server"};
		{false} ->
			Result = ok,
			NewSt = St#server_st{clients=[{ClientPID,Nick}] ++ ClientList }
	end,
	{Result, NewSt};

loop(St, {disconnect, ClientPID}) ->
	ClientList = St#server_st.clients,
	case {isPIDConnected(ClientPID, ClientList)} of
	{true} ->
		{ _, Nick} = lists:keyfind(ClientPID ,1 ,ClientList),
		NewClientList = lists:delete({ClientPID, Nick}, ClientList),
		NewSt = St#server_st{clients = NewClientList},
		Result = ok;
	{false} -> 
		NewSt = St,
		Result = {error, user_not_connected,"You are not connected to this server"}
	end,
	{Result, NewSt};

loop(St, {join, Channel,ClientPID}) ->
	case {lists:member(list_to_atom(Channel),St#server_st.channels)} of
		{false} -> io:format("Creating new channel"),
				genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:loop/2),
				ChannelList = St#server_st.channels,
				NewSt = St#server_st{channels=ChannelList++[list_to_atom(Channel)]},
				Result = genserver:request(list_to_atom(Channel),{join,ClientPID});
		{true} -> Result = genserver:request(list_to_atom(Channel),{join,ClientPID}),
				NewSt=St
	end,
	io:format("~p",[Result]),
	{Result, NewSt};
	
loop(St, _Msg) ->  
    {ok, St}.

isNickTaken(Nick, ClientList) ->
	{_, Nicks} = lists:unzip(ClientList),
	io:format("~p~n~p",[Nick,Nicks]),
	case {lists:member(Nick, Nicks)} of
	{true} -> Result = true;
	{false} -> Result = false
	end,
	{Result}.

isPIDConnected(ClientPID, ClientList) ->
	{ClientPIDs, _} = lists:unzip(ClientList),
	Result = lists:member(ClientPID, ClientPIDs),
	{Result}.


initial_state(_Server) ->
    #server_st{name=list_to_atom(_Server),clients=[]}.
