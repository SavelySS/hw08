-module(keylist).
-export([start_link/1, init/1, loop/0]).


% ожидает сообщения от сервера. Если поступает сообщение stop, процесс выходит
% из цикла рекурсивных вызовов (не вызывает loop). Другие сообщения игнорируются,
% но извлекаются из mailbox (можно использовать в receive шаблон “_”).

start_link(Name) -> 
    Pid = spawn_link(?MODULE, init, [Name]),
    {ok, Pid}.

init(Name) -> 
    register(Name, self()),
    loop(). 

loop() -> 
    receive 
        stop -> 
            ok;
        _ ->
            loop()
    end.