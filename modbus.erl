-module(modbus).
-export([createRequest/4, parseResponse/1, parseData/3]).
-export([parseCoilData/2, parseRegisterData/1, dataMatchKey/2]).

%% ---------------
%% return Cmd Bin
createRequest(SlaveId, FunCode, Start, Number) ->
    % validate
    if
        (SlaveId < 0) or (SlaveId > 247) ->
            throw({argumentError, SlaveId});
        true -> ok
    end,
    if
        (FunCode < 0) or (FunCode > 255) ->
            throw({argumentError, FunCode});
        true -> ok
    end,
    if
        (Start < 0) or (Start > 65535) ->
            throw({argumentError, Start});
        true -> ok
    end,
    if
        (Number < 1) or (Number > 2000) ->
            throw({argumentError, Number});
        true -> ok
    end,
    
    <<StartHi:8, StartLo:8>> = <<Start:16>>,
    <<NumberHi:8, NumberLo:8>> = <<Number:16>>,
    {CrcHighByte, CrcLowByte} = crc:calculateCRC([SlaveId, FunCode, StartHi, StartLo, NumberHi, NumberLo]),
    <<SlaveId:8, FunCode:8, Start:16, Number:16, CrcHighByte:8, CrcLowByte:8>>.

%% ---------------
%% parse read response pdu
%% return {SlaveId, FunCode, DataByteSize, Data}
parseResponse(Pdu) ->
    [SlaveId, FunCode, DataByteSize | Data] = Pdu,
    {SlaveId, FunCode, DataByteSize, Data}.
 

%% ----------------
%% 解析数据值   
parseData(FunCode, DataByteSize, Data) ->
    % io:format("FunCode=~p~n",[FunCode]),
    case FunCode of
        T when T =:= 1 ; T =:= 2 ->
            % 离散量
            parseCoilData(Data, DataByteSize);
        T when T =:= 3 ; T =:= 4  ->
            % 线圈
            parseRegisterData(Data)
    end.

%% ----------------
%% 解析离散量数据值, 两个字节（16bit）组成一个值
parseRegisterData(Data) ->
    parseRegisterData_2(Data, []).

parseRegisterData_2([], Rdata) ->
    lists:reverse(Rdata);
parseRegisterData_2([Hi, Lo | T], Rdata) ->
    <<D:16>> = <<Hi:8, Lo:8>>,
    parseRegisterData_2(T, [D | Rdata]).

%% ----------------
%% 解析线圈数据值, 一个字节（8bit）拆为8个值
parseCoilData(Data, DataByteSize) ->
    lists:sublist(parseCoilData_2(Data, []), DataByteSize).

parseCoilData_2([], Rdata) ->
    lists:reverse(Rdata);
parseCoilData_2([H | T], Rdata) ->
    <<D8:1, D7:1, D6:1, D5:1, D4:1, D3:1, D2:1, D1:1>> = <<H:8>>,
    parseCoilData_2(T, [D8, D7, D6, D5, D4, D3, D2, D1 | Rdata]).

%% ----------------
%% 值匹配到key
%% return [{key, value}, ...]
dataMatchKey(KeyRatios, Values) ->
    dataMatchKey_3(KeyRatios, Values, []).

dataMatchKey_3([], [], Rdata) ->
    lists:reverse(Rdata);
dataMatchKey_3([{Key, Ratio} | Tk], [Value | Tv], Rdata) ->
    dataMatchKey_3(Tk, Tv, [{Key, Value/Ratio} | Rdata]).
