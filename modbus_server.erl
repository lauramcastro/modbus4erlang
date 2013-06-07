%% ---
%% 
%%---
-module(modbus_server).  
-export([start/2, stop/1]). 

start({Ip, Port, PortType}, Time) ->
	spawn(fun() -> loop({Ip, Port, PortType}, Time) end).

stop(Pid) -> Pid ! stop.


loop({Ip, Port, PortType}, Time, DevCollInfos) ->
    receive
	stop ->
	    void
    after Time ->
    	try 
    		Data = lists:map(fun(X)-> X/10 end, modbus_rtu_ipraw:send(Ip, Port, 2,3,0,2)),
    	   	io:format("get = ~p  ~p~n",[{Ip, Port, PortType},  Data])
    	catch
   		    throw:X -> io:format("~p~n",[{caught, thrown, X}]);
    	end,
    	loop({Ip, Port, PortType}, Time)
    end.

%% 解析完了
collectTheDev({Ip, Port, PortType, Time, SlaveId}, {DevType,{[], Res}}, Res2) ->
%% 解析
collectTheDev({Ip, Port, PortType, Time, SlaveId}, {DevType,{[Parse|T], Res}}, Res2) ->


%% 命令下完了
collectTheDev({Ip, Port, PortType, Time, SlaveId}, {DevType,{[], P}}, Res) ->
%% 下命令
collectTheDev({Ip, Port, PortType, Time, SlaveId}, {DevType,{[Cmd|T], P}}, Res) ->
    FunSend = fun(FunCode, Start, Number) -> ( modbus_rtu_ipraw:send(Ip, Port, SlaveId, FunCode, Start, Number) ) end,



