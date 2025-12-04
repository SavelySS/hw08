-module(keylist_manager).
-export([loop/1, start/0]).

-include("keylist.hrl").

loop(State) ->
    % типы сообщений
    % {From, start_child, Name}
    % {From, stop_child, Name}
    % stop
    % {From, get_state} 
    % {'EXIT', Pid, Reason} 
    receive 

        {From, start_child, Name} ->
            % проверить запущен дочерний процесс с именем Name или нет.
            case proplists:get_value(Name, State#state.children) of 
                undefined ->
                    % запустить процесс
                    {ok, Pid} = keylist:start_link(Name),
                    %  сохранить {Name, Pid} в #state.children.
                    NewChild = [{Name, Pid} | State#state.children],
                    % Child process started keylist1, <0.90.0>
                    io:format("Child process started ~p, ~p~n", [Name, Pid]),
                    % Вернуть {ok, Pid} процессу From.
                    From ! {ok, Pid},
                    loop(State#state{children = NewChild});

                Pid ->
                    From ! {ok, Pid},
                    loop(State)
            end;

        {From, stop_child, Name} ->
            % проверить запущен дочерний процесс с именем Name или нет.
            case proplists:get_value(Name, State#state.children) of 
                undefined ->
                    % Вернуть результат процессу {error, not_found}.
                    From ! {error, not_found},
                    loop(State);

                Pid ->
                    Pid ! stop, 
                    % может лучше exit уточнить
                    NewChild = proplists:delete(Name, State#state.children),
                    From ! ok,
                    loop(State#state{children = NewChild})
            end;

        % завершить работу серверного процесса (менеджера).
        % > keylist_manager ! stop.
        % Server terminated
        stop ->
            io:format("Server terminated~n"), 
            lists:foreach(fun({_Name, Pid}) -> Pid ! stop end, State#state.children),
            % выход из reecive
            ok;

        % {From, get_state} — вернуть процессу From список имен запущенных процессов из #state.children.
        {From, get_state} ->
            Names = [Name || {Name, _Pid} <- State#state.children],
            From ! Names,
            loop(State);

        % {'EXIT', Pid, Reason} — вывести на консоль информацию о завершении работы процесса Pid. Удалить процесс из #state.children.
        % > exit(<0.90.0>, error).
        % Trap: child process <0.90.0> terminated: error

        {'EXIT', Pid, Reason} ->
            io:format("Trap: child process ~p terminated: ~p~n", [Pid, Reason]),
            NewChild = lists:keydelete(Pid, 2, State#state.children),
            loop(State#state{children = NewChild});

        _ ->
            loop(State)
    end.


% запускает серверный процесс, регистрирует его с именем ?MODULE, связывает вызывающий
% процесс с сервером монитором. Функция start/0 должна вернуть {ok, ServerPid, MonitorRef}.
% Серверный процесс запускает функцию init, в которой настраивает перехват
% сигналов о завершении дочерних процессов: process_flag(trap_exit, true).

start() ->
    {ServerPid, MonitorRef} = spawn_monitor(fun() -> init() end),
    register(?MODULE, ServerPid),
    {ok, ServerPid, MonitorRef}.


% перехват сигналов о завершении дочерних процессов: process_flag(trap_exit, true).
init() ->
    process_flag(trap_exit, true),
    loop(#state{}).




% get_value(Key, List)
% Equivalent to get_value(Key, List, undefined).

% get_value(Key, List, Default)
% Returns the value of a simple key/value property in List. If lookup(Key, List) would yield {Key, Value}, this function returns the corresponding Value, otherwise Default.

% 4> proplists:get_value('5', ['1', '2', '3'], notfound).
% notfound