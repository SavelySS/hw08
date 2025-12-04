Итоговый архив должен содержать следующие файлы:
· keylist.hrl
· keylist_manager.erl
· keylist.erl
· Текстовый файл Eshell.txt с результатами выполнения команд в
Eshell (тестирование сервера)
1. Обновите модуль keylist_manager.erl из домашнего задания 7
Добавьте в keylist_manager.erl клиентские функции (API) для передачи
сообщений:
 {From, start_child, Name}
 {From, stop_child, Name}
 stop
 {From, get_names}
Например:
start_child(Name) ->
keylist_manager ! {self(), start_child, Name}.
Теперь клиент модуля keylist_manager может вызывать функцию
keylist_manager:start_child(keylist1).
2. Обновите API для keylist_manger:start_child/1 — вместо Name
используйте Params. Например: {From, start_child, Params},
где Params :: #{name => atom(), restart => permanent | temporary}
Если процесс со стратегией permanent добавьте его в список
#state.permanent. Если keylist (процесс keylist.erl) завершается
аварийно - мы должны его перезапустить. Не забудьте обновить
#state.permanent (удалить старый Pid и добавить новый перезапущенный
Pid).
Если restart = temporary, то при аварийном завершении keylist мы
должны только залогировать на экран факт завершения и ничего не
запускать.
В обоих случаях не забудьте обновить #state.children.
Обновите рекорд #state{children, permanent}. Элемент permanent
мы будем использовать для сохранения Pid процессов, которые нам нужно
перезапустить в случае их падения: список [Pid1, …, PidN]. Подумайте
какое значение по умолчанию нужно задать для поля permanent.
Проверьте, что стратегия перезапуска restart => permanent |
temporary работает. С помощью Eshell. Запустите процессы и остановите
их с использованием exit(Pid, Reason).
Функцией whereis/1 проверьте жив ли процесс по его имени.
3. Добавьте spec в keylist_manager.erl для экспортируемых
функций и записей (record).
Добавьте документацию для модуля keylist_manager.erl и
экспортируемых функций.
4. Обновите модуль keylist.erl
Добавьте в keylist.erl API-функции для всех типов сообщений
(необходимо расширить реализацию keylist.erl):
 {From, add, Key, Value, Comment} — добавляет в список процесса
тройку (Key, Value, Comment);
 {From, is_member, Key} — проверяет наличие в списке процесса
тройки с ключом Key (результат true/false);
 {From, take, Key} — возвращает тройку или {ok, not_found} и
удаляет её из списка;
 {From, find, Key} — возвращает тройку не удаляя её из списка
или {ok, not_found};
 {From, delete, Key} — удаляет тройку, возвращает ok или {ok,
not_found}.
Например:
add(Name, Key, Value, Comment) ->
Name ! {self(), add, Key, Value, Comment}.
Теперь клиент может вызывать
keylist:add(keylist1, key1, “value1”, “comment1”).
Добавьте spec в keylist.erl для экспортируемых функций и записей.
Добавьте документацию для модуля keylist.erl и экспортируемых
функций.