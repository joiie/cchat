-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
	case {whereis(list_to_atom(_Server))} of
	{undefined} ->
		NewSt = St,
		Result = {error, server_not_reached,"Connect timeout"};
	{_} ->
		NewServer = list_to_atom(_Server),
		Result = genserver:request(NewServer,{connect,self(),St#cl_st.nick}),
		NewSt = St#cl_st{server =  NewServer}
	end,
	{Result, NewSt};
%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
	NoOfChannels = length(St#cl_st.channels),
	case{NoOfChannels > 0} of
	{true} ->
		Result = {error, leave_channels_first,"Leave channels first!"};
	{false} ->
		case {St#cl_st.server == undefined} of
			{true} -> Result = {error, user_not_connected, "you are not connected to any server"};
			{false} -> Result = genserver:request(St#cl_st.server, {disconnect,self()})
		end
	end,
    {Result, St} ;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
	Result = genserver:request(St#cl_st.server,{join,_Channel,self()}),
    ChannelList = St#cl_st.channels,
    NewSt = St#cl_st{channels = [list_to_atom(_Channel)] ++ ChannelList},
    {Result, NewSt} ;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
	OldChannelList = St#cl_st.channels,
	NewChannelList = lists:delete(list_to_atom(_Channel),OldChannelList),
	NewSt = St#cl_st{channels = NewChannelList},
	Result = genserver:request(list_to_atom(_Channel), {leave, self()}),
    {Result, NewSt} ;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
	Result = genserver:request(list_to_atom(_Channel),{mesage, St, self(), _Channel, _Msg}),
    {Result, St} ;

%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
	Uname = St#cl_st.nick,
    {Uname, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
	NewSt = St#cl_st{nick = _Nick},
    {ok, NewSt} ;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, {message,Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St};

loop(St, _) ->
	{ok, St}.

initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, channels = []}.
