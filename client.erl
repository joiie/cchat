-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
	case {string:equal(_Server, "shire")} of
	{false} ->
		NewSt = St,
		Result = {error, server_not_reached,"Connect timeout"};
	{true} ->
		Result = genserver:request(list_to_atom(_Server),{connect,self()}),
		NewSt = St#cl_st{server =  list_to_atom(_Server)}
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
	GUIName = St#cl_st.gui,
	io:format("I am ~p~nMy gui is ~p",[Uname,GUIName]),
    {Uname, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
	NewSt = St#cl_st{nick = _Nick},
	io:format("Client nick is: ~p~n",[NewSt#cl_st.nick]),
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
	io:format("No match"),
	{ok, St}.

initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, channels = []}.
