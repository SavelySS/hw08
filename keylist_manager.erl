-module(keylist_manager).
-export([start/0, start_child/1, stop_child/1, get_names/0, stop/0]).

-include("keylist.hrl").

-type strat() :: permanent | temporary.
-type params() :: #{name => atom(), restart => strat()}.

loop(State) ->
    receive 
        {From, start_child, #{name := Name, restart := Restart} = _Params} ->
            case proplists:get_value(Name, State#state.children) of 
                undefined ->
                    {ok, Pid} = keylist:start_link(Name),
                    NewChild = [{Name, Pid} | State#state.children],

                    NewPerm = case Restart of
                        permanent -> 
                            [Pid | State#state.permanent];
                        temporary ->
                            State#state.permanent
                    end,

                    io:format("Child process started ~p ~p, ~p~n", [Name, Restart, Pid]),
                    From ! {ok, Pid},
                    loop(State#state{children = NewChild, permanent = NewPerm});

                Pid ->
                    From ! {ok, Pid},
                    loop(State)
            end;

        {From, stop_child, Name} ->
            case proplists:get_value(Name, State#state.children) of 
                undefined ->
                    From ! {error, not_found},
                    loop(State);
                Pid ->
                    Pid ! stop, 
                    NewChild = proplists:delete(Name, State#state.children),
                    NewPerm = lists:delete(Pid, State#state.permanent),
                    From ! ok,
                    loop(State#state{children = NewChild, permanent = NewPerm})
            end;

        stop ->
            io:format("Server terminated~n"), 
            lists:foreach(fun({_Name, Pid}) -> Pid ! stop end, State#state.children),
            ok;

        {From, get_names} ->
            Names = [Name || {Name, _Pid} <- State#state.children],
            From ! Names,
            loop(State);


        {'EXIT', Pid, Reason} ->
            io:format("Trap: child process ~p terminated: ~p~n", [Pid, Reason]),

            case find_pid(Pid, State#state.children) of
                {Name, Pid} ->
                    NewChild = proplists:delete(Name, State#state.children),
                    NewPerm = lists:delete(Pid, State#state.permanent),
                    
                    case lists:member(Pid, State#state.permanent) of
                        true ->
                            io:format("Restarting permanent process ~p~n", [Name]),
                            {ok, NewPid} = keylist:start_link(Name),
                            FinChild = [{Name, NewPid} | NewChild],
                            FinPerm = [NewPid | NewPerm],
                            loop(State#state{
                                children = FinChild,
                                permanent = FinPerm
                            });
                            
                        false ->
                            loop(State#state{
                                children = NewChild,
                                permanent = NewPerm
                            })
                    end;
                    
                false ->
                    loop(State)
            end;

        _ ->
            loop(State)
    end.

-spec find_pid(pid(), [{atom(), pid()}]) -> {atom(), pid()} | false.
find_pid(Pid, Children) ->
    case lists:keyfind(Pid, 2, Children) of
        {Name, Pid} -> {Name, Pid};
        false -> false
    end.

%% @doc запускает серверный процесс, регистрирует его с именем ?MODULE, связывает вызывающий процесс с сервером монитором, запускает функцию init. 
%% @returns {ok, pid(), reference()} | { error, term()}.
-spec start() -> {ok, pid(), reference()} | { error, term()}.
start() ->
    {ServerPid, MonitorRef} = spawn_monitor(fun() -> init() end),
    register(?MODULE, ServerPid),
    {ok, ServerPid, MonitorRef}.    

%% @doc перехват сигналов о завершении дочерних процессов: process_flag(trap_exit, true).
-spec init() -> no_return().
init() ->
    process_flag(trap_exit, true),
    loop(#state{}).

%% @doc запуск дочернего процесса с параметрами
%% @param  Params :: #{name => atom(), restart => permanent | temporary}
%% @returns ok | {error, term()}.
-spec start_child(params()) -> ok | {error, term()}.
start_child(Params) -> 
    keylist_manager ! {self(), start_child, Params},

    receive 
        {ok, _Pid} -> ok;
        {error, Reason} -> {error, Reason}
    after 5000 -> 
        {error, timeout}
    end.

%% @doc остановка дочернегго процесса по имени
%% @returns ok | {error, term()}.
-spec stop_child(atom()) -> ok  | {error, term()}.
stop_child(Name) -> 
    keylist_manager ! {self(), stop_child, Name},

    receive 
        ok -> ok;
        {error, Reason} -> {error, Reason}
    after 5000 -> 
        {error, timeout}
    end.

%% @doc получение списка имен запущенных проццессов
%% @returns [atom()] | {error, term()}.
-spec get_names() -> [atom()]  | {error, term()}.
get_names() -> 
    keylist_manager ! {self(), get_names},

    receive 
        Names -> Names
    after 5000 -> 
        {error, timeout}
    end.

%% @doc остановка всех процессов
%% @returns ok.
-spec stop() -> ok.
stop() -> 
    keylist_manager ! stop, ok.