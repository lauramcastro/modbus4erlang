%% ---
%% 
%%---
-module(collect_conf).

-behaviour(gen_server).
-export([start/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-compile(export_all).


start() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call({global, ?MODULE}, stop).

add_serial_server(IpPort, Devs) -> gen_server:call({global, ?MODULE}, {add_serial_server, IpPort, Devs}).
% return old devs
modify_serial_server(IpPort, Devs)  -> gen_server:call({global, ?MODULE}, {modify_serial_server, IpPort, Devs}).
del_serial_server(IpPort) -> gen_server:call({global, ?MODULE}, {del_serial_server, IpPort}).
get_all_serial_server() -> gen_server:call({global, ?MODULE}, {get_serial_server, all}).
get_serial_server(IpPort) -> gen_server:call({global, ?MODULE}, {get_serial_server, IpPort}).



init([]) ->
	{ ok, ets:new(collect_conf_serial_server,[set]) }.

handle_call({add_serial_server, IpPort, Devs}, _From, Tab) ->
	io:format("add_serial_server = ~p  ~p~n",[Tab,  IpPort]),
    Reply = case ets:lookup(Tab, IpPort) of
		[]  ->
			ets:insert(Tab, {IpPort, Devs});
		[_] ->
			{error_already, {IpPort, Devs}}
	    end,
    {reply, Reply, Tab};
handle_call({modify_serial_server, IpPort, Devs}, _From, Tab) ->
    Reply = case ets:lookup(Tab, IpPort) of
		[]  ->
			{not_found, IpPort};
		[{IpPort,OldDevs}] ->
		    ets:insert(Tab, {IpPort, Devs}),
		    OldDevs
	    end,
    {reply, Reply, Tab};
handle_call({del_serial_server,IpPort}, _From, Tab) ->
    Reply = ets:delete(Tab, IpPort),
    {reply, Reply, Tab};
handle_call({get_serial_server, all}, _From, Tab) ->
    Reply = ets:tab2list(Tab),
    {reply, Reply, Tab};
handle_call({get_serial_server, IpPort}, _From, Tab) ->
    Reply = case ets:lookup(Tab, IpPort) of
		[]  ->
			{not_found, IpPort};
		[{IpPort,Devs}] ->
		    Devs
	    end,
    {reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
	ets:delete(Tab),
	{stop, normal, stopped, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


    
