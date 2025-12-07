-module(keylist).

-export([start_link/1, add/4, is_member/2, take/2, find/2, delete/2]).

-type key() :: any().
-type value() :: any().
-type comment() :: any().
-type triple() :: {key(), value(), comment()}.

%% @doc запускает процесс с заданным именем
%% @param Name имя для регистрации процесса
%% @returns {ok, pid()}
-spec start_link(atom()) -> {ok, pid()}.
start_link(Name) ->
    Pid = spawn_link(fun() -> init(Name, []) end),
    {ok, Pid}.

%% @doc добавляет в список процесса тройку 
%% @param Name имя процесса
%% @param Key ключ
%% @param Value значение
%% @param Comment комментарий
%% @returns ok
-spec add(atom(), key(), value(), comment()) -> ok.
add(Name, Key, Value, Comment) ->
    Name ! {self(), add, Key, Value, Comment},

    receive
        ok -> ok
    after 5000 ->
        {error, timeout}
    end.

%% @doc проверяет наличие в списке процесса тройки с ключом Key
%% @param Name имя процесса
%% @param Key ключ для проверки
%% @returns boolean()
-spec is_member(atom(), key()) -> boolean().
is_member(Name, Key) ->
    Name ! {self(), is_member, Key},

    receive
        Result -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @doc извлекает и удаляет тройку по ключу
%% @param Name имя процесса
%% @param Key ключ для извлечения
%% @returns {ok, triple()} | {ok, not_found}
-spec take(atom(), key()) -> {ok, triple()} | {ok, not_found}.
take(Name, Key) ->
    Name ! {self(), take, Key},
    receive
        Result -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @doc возвращает тройку не удаляя её из списка
%% @param Name имя процесса
%% @param Key ключ для поиска
%% @returns {ok, triple()} | {ok, not_found}
-spec find(atom(), key()) -> {ok, triple()} | {ok, not_found}.
find(Name, Key) ->
    Name ! {self(), find, Key},

    receive
        Result -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @doc удаляет тройку по ключу
%% @param Name имя процесса
%% @param Key ключ для удаления
%% @returns ok | {ok, not_found}
-spec delete(atom(), key()) -> ok | {ok, not_found}.
delete(Name, Key) ->
    Name ! {self(), delete, Key},

    receive
        Result -> Result
    after 5000 ->
        {error, timeout}
    end.

%% @doc инициализация
-spec init(atom(), [triple()]) -> no_return().
init(Name, Data) ->
    register(Name, self()),
    loop(Name, Data).

%% @doc цикл обработки сообщений
-spec loop(atom(), [triple()]) -> no_return().
loop(Name, Data) ->
    receive

        {From, add, Key, Value, Comment} ->
            NewData = [{Key, Value, Comment} | Data],
            From ! ok,
            loop(Name, NewData);

        {From, is_member, Key} ->
            Result = lists:keymember(Key, 1, Data),
            From ! Result,
            loop(Name, Data);

        {From, take, Key} ->
            case lists:keytake(Key, 1, Data) of
                {value, Triple, NewData} ->
                    From ! {ok, Triple},
                    loop(Name, NewData);
                false ->
                    From ! {ok, not_found},
                    loop(Name, Data)
            end;

        {From, find, Key} ->
            case lists:keyfind(Key, 1, Data) of
                Triple when Triple =/= false ->
                    From ! {ok, Triple};
                false ->
                    From ! {ok, not_found}
            end,
            loop(Name, Data);

        {From, delete, Key} ->
            case lists:keytake(Key, 1, Data) of
                {value, _, NewData} ->
                    From ! ok,
                    loop(Name, NewData);
                false ->
                    From ! {ok, not_found},
                    loop(Name, Data)
            end;

        stop ->
            ok;

        _ ->
            loop(Name, Data)
    end.