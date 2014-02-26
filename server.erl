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
			NewClientList = [{ClientPID, Nick}] ++ ClientList,
			NewSt = St#server_st{clients=NewClientList },
			io:format("Connect: ~n~p~n",[NewSt])
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

%Join channel
loop(St, {join, Channel,ClientPID}) ->
	case {lists:member(list_to_atom(Channel),St#server_st.channels)} of
		{false} -> %Channel does not exist, create new channel
				genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:loop/2),
				NewSt = St#server_st{channels=St#server_st.channels++[list_to_atom(Channel)]},
				Result = genserver:request(list_to_atom(Channel),{join,ClientPID});
		{true} -> %Channel exists, join channel
				Result = genserver:request(list_to_atom(Channel),{join,ClientPID}),
				NewSt=St
	end,
	{Result, NewSt};
	
loop(St, _Msg) ->  
    {ok, St}.

isNickTaken(Nick, ClientList) ->
	case {lists:keyfind(Nick,2,ClientList)} of
		{false} -> Result= false;
		{_} -> Result = true
	end,
	Result.
isPIDConnected(ClientPID, ClientList) ->
	case {lists:keyfind(ClientPID,1,ClientList)} of
		{false} -> Result= false;
		{_} -> Result = true
	end,
	Result.

initial_state(_Server) ->
    #server_st{name=list_to_atom(_Server),clients=[]}.
