-module(channel).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {join, ClientPID}) ->
	case {lists:member(ClientPID, St#channel_st.clients)} of
	{true} -> 
		{{error, user_already_joined,"You are already joined"}, St};
	{false} ->
		NewSt = St#channel_st{clients=[ClientPID] ++ St#channel_st.clients },
		{ok, NewSt}
	end;
	
loop(St, {mesage, ClientSt, ClientPID, _Channel, Message}) ->
	case {lists:member(ClientPID,St#channel_st.clients)} of
	{true} ->
		ListSendTo = lists:delete(ClientPID,St#channel_st.clients),
		lists:foreach(fun(PID)->
			genserver:request(PID, {message,_Channel,ClientSt#cl_st.nick, Message}) end,
			ListSendTo),
		{ok, St};
	{false} ->
		{{error, user_not_joined,"You are not in this channel"}, St}
	end;

loop(St, {leave, ClientPID}) ->
	case {lists:member(ClientPID, St#channel_st.clients)} of 
	{true} ->
		ClientList = lists:delete(ClientPID,St#channel_st.clients),
		NewSt = St#channel_st{clients = ClientList},
		{ok, NewSt};
	{false} ->
		{{error, user_not_joined,"You are not in this channel"}, St}
	end;

loop(St, _Msg) ->  
    {ok, St}.

initial_state(_Channel) ->
    #channel_st{name=list_to_atom(_Channel)}.
