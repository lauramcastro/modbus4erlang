-module(modbus_rtu_ipraw).
-export([send/6, send/4]).
-export([collect/2]).

%% ---------------
%% 设备定位， 命令及解析
%% Dev : {Ip, Port, SlaveId}
%% return [{key, value}, ...]
collect(Dev, CmdKeyRatios) ->
    collect_4(Dev, CmdKeyRatios, [], 0).

collect_4(_Dev, [], Rdata, _) ->
    Rdata;
collect_4({Ip, Port, SlaveId},  [{{FunCode, Start, Number}, KeyRatios} |T],  Rdata,  Sleep) ->
    if
        Sleep > 0 ->
            timer:sleep(Sleep);
        true -> void.
    end,
    collect_4({Ip, Port, SlaveId},  T,  [modbus:dataMatchKey(KeyRatios, send(Ip, Port, SlaveId, FunCode, Start, Number)) | Rdata],  200).


%% ---------------
%% return [D1, D2 ...]
send(Ip, Port, SlaveId, FunCode, Start, Number) ->
    ReqBin = modbus:createRequest(SlaveId, FunCode, Start, Number),
    send(Ip, Port, ReqBin, data).


%% ---------------
%% Ip, Port, ReqBin, ReturnType
%% return binary
send(Ip, Port, ReqBin, binary) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    % io:format("client opened socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, Ip, Port, ReqBin),
    ResponseBin = receive
        {udp, Socket, _, _, Bin} ->
            Bin
        after 2000 ->
            throw({timeoutError, 2000}),
            gen_udp:close(Socket)
        end,
    gen_udp:close(Socket),
    ResponseBin;
%% ---------------
%% Ip, Port, ReqBin, ReturnType
%% return  {SlaveId, FunCode, DataByteSize, Data[]}
send(Ip, Port, ReqBin, pdu) ->
    ResponseBin =send(Ip, Port, ReqBin, binary),
    ResponseBinSize = byte_size(ResponseBin),
    if
        ResponseBinSize =< 2 ->
            throw({argumentError, ResponseBinSize});
        true -> ok
    end,
    Pdu = binary_to_list(ResponseBin, 1, ResponseBinSize-2),
    ResCrc = list_to_tuple(binary_to_list(ResponseBin, ResponseBinSize-1, ResponseBinSize)),
    case crc:checkCRC(Pdu, ResCrc) of
        true ->
            modbus:parseResponse(Pdu);
        false ->
            throw({error, response_crc_error, ResCrc})
    end;
%% ---------------
%% Ip, Port, ReqBin, ReturnType
%% return Data
send(Ip, Port, ReqBin, data) ->
    {_SlaveId, FunCode, DataByteSize, Data} = send(Ip, Port, ReqBin, pdu),
    modbus:parseData(FunCode, DataByteSize, Data).
