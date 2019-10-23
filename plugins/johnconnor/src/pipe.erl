-module(pipe).
-export([read/0, write/2]).

read() ->
    Fifo = open_port("/tmp/skypeernet_write", [eof, in]),
    loop(Fifo, 0, []).

dispatch(Data) ->
    devices ! {message, {Data}}.

loop(Fifo, 0, []) ->
    receive
        {Fifo, {data, Data}} ->
            msgstart(Fifo, Data);
        {_Pid, eof} ->
            read();
        _Unsupported ->
            loop(Fifo, 0, [])
    end;
loop(Fifo, Size, Rest) ->
    receive
        {Fifo, {data, Data}} ->
            NewData = lists:append(Rest, Data),
            msgready(Fifo, NewData, Size);
        {_Pid, eof} ->
            read();
        _Unsupported ->
            loop(Fifo, 0, [])
    end.

msgready(Fifo, NewData, Size) when length(NewData) > Size ->
    {Arrived, ToProcess} = lists:split(Size, NewData),
    dispatch(Arrived),
    msgstart(Fifo, ToProcess);
msgready(Fifo, NewData, Size) when length(NewData) == Size ->
    dispatch(NewData),
    loop(Fifo, 0, []);
msgready(Fifo, NewData, Size) ->
    loop(Fifo, Size, NewData).

msgstart(Fifo, Data) ->
    {ListSize, Rest} = lists:split(4, Data),
    <<Size:32>> = list_to_binary(lists:reverse(ListSize)),
    msgready(Fifo, Rest, Size).

write(Fifo, Json) ->
    Size = byte_size(Json),
    Data = list_to_binary([<<Size:32>>, Json]),
    true = port_command(Fifo, Data).
