-module(pipe).
-export([read/0, write/1]).

read() ->
    Fifo = open_port("/tmp/skypeernet_write", [eof]),
    loop(Fifo, 0, []).

dispatch(Data) ->
    devices ! {message, {Data}}.

loop(Fifo, 0, []) ->
    receive
        {Fifo, {data, Data}} ->
            {ListSize, Rest} = lists:split(4, Data),
            <<Size:32>> = list_to_binary(lists:reverse(ListSize)),
            if Size == length(Rest) ->
                dispatch(Rest),
                loop(Fifo, 0, []);
            true ->
                loop(Fifo, Size, Rest)
            end;
        {_Pid, eof} ->
            read();
        _Unsupported ->
            loop(Fifo, 0, [])
    end;
loop(Fifo, Size, Rest) ->
    receive
        {Fifo, {data, Data}} ->
            NewData = lists:append(Rest, Data),
            if length(NewData) == Size ->
                dispatch(NewData),
                loop(Fifo, 0, []);
            true ->
                loop(Fifo, Size, NewData)
            end;
        {_Pid, eof} ->
            read();
        _Unsupported ->
            loop(Fifo, 0, [])
    end.

write(Json) ->
    Fifo = open_port("/tmp/skypeernet_read", [eof]),
    port_command(Fifo, Json),
    port_close(Fifo).
