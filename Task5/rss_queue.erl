-module(rss_queue).
-include("logging.hrl").
-export([start/0, start/1, server/2, add_item/2, add_feed/2, get_all/1]).

% Таймаут на время ожижания приема сообщений = 1 с.
-define(TIMEOUT,10000).


% init([url()]) -> pid
% Функция Инициализации очереди и создание процесса, возвращает PID вызванного процесса.
init([]) ->
	Q = [],
	spawn(?MODULE,server,[Q, sets:new()]);

init([Url]) ->
	QPid = init([]),
	rss_reader:start(Url, QPid).


% start() -> pid
% Функция для запуска нового серверного процесса, обслуживающего очередь, возвращает Pid созданного процесса.
start() ->
	init([]).

% start(Url::url()) -> pid
% Функция для запуска нового серверного процесса, обслуживающего очередь, возвращает Pid созданного процесса.
start(Url)->
	init([Url]).


% server([rssItem()], set())
% Функция, реализующая цикл сервера процесса очереди RSS и  осуществляющая
% управление очередью и администрирование подписок.
server(Q, Subscribers) ->
	receive
		{add_item, RSSItem} ->
	    	NewQ = add_item_to_q(RSSItem, Q, Subscribers), 
	    	SortQ = sort(NewQ),
	    	server(SortQ, Subscribers);

	    {get_all, ReqPid} ->
	      	ReqPid ! {self(),Q},
	      	?INFO("Sent rss items to ~p~n",[ReqPid]),
	      	server(Q, Subscribers);

	    {subscribe, QPid} ->
	    	case sets:is_element(QPid, Subscribers) of
	    		true ->
	    			?INFO("This pid ~p already exists in the set~n", [QPid]),
	    			warning;
	    		false ->
	    			erlang:monitor(process,QPid),
	    			?INFO("New subscriber ~p to ~p~n",[QPid,self()]),
	    			[add_item(QPid,Item) || Item <- Q],
	    			server(Q,sets:add_element(QPid, Subscribers))
	    	end;

	    {unsubscribe, QPid} ->
	    	case sets:is_element(QPid, Subscribers) of
	    		true ->
	    			?INFO("Process ~p is unsubscribed~n", [QPid]),
	    			server(Q,sets:del_element(QPid, Subscribers));
	    		false ->
	    			?INFO("This process ~p does not exist in the set~n", QPid),
	    			error
	    	end;

	    {'DOWN', _, _, QPid,Reason} ->
	    	?INFO("This process ~p is down: ~p. ~n", [QPid, Reason]),
	    	server(Q,sets:del_element(QPid, Subscribers));

	    _Msg -> ?ERROR("Unknown message: ~p~n",[_Msg]), {unknown_msg,_Msg}
	end.


% add_item(QPid::pid(), Item::rssItem())
% Вспомогательная функция, упрощающая процедуру отправки элемента в очередь;
% QPid - это PID процесса очереди, а Item - это элемент добавляемый к очереди.
add_item(QPid, Item) when is_pid(QPid) ->
 	QPid ! {add_item, Item}, ok.

% add_feed(QPid::pid(), RSS2Feed::rssDoc())
% Эта функция должна извлекать все элементы из документа ленты,  и отправлять все элементы по порядку в очередь.
add_feed(QPid,RSS2Feed) when is_pid(QPid) ->
	case rss_parse:is_rss2_feed(RSS2Feed) of
		true ->
			Items = rss_parse:get_feed_items(RSS2Feed),
			[add_item(QPid,Item) || Item <- Items],
			io:format("Size of RSS2Feed: ~p ~n", [length(Items)]),
			ok;
		false ->
			?ERROR("Version not 2.0! PID: ~p~n", [QPid]),
			error
	end.

% get_all(QPid::pid())
% Эта вспомогательная функция упрощает процедуру получения списка элементов ленты от процесса.
get_all(QPid) when is_pid(QPid) ->
 	QPid ! {get_all, self()},
 	receive
 		{QPid, Q} -> 
 			io:format("Queue size = ~p items from the feed to ~p ~n", [length(Q),QPid]), 
 			{ok, Q};
 		_Msg -> ?ERROR("Unknown message: ~p~n",[_Msg]), {unknown_msg,_Msg}
 	after 
 		?TIMEOUT -> 
 			?ERROR("Timeout exceeded: module ~p, ~p in line ~p.~n", 
 					[?MODULE_STRING, ?FUNCTION_NAME, ?LINE]),
 			{error,timeout}
 	end.

% add_item_to_q(NewItem::rssItem(), Q::[rssItem()], Subscribers::set())
% Вспомогательная функция инициирующая процесс добавления нового элемента ленты в очередь.
add_item_to_q(NewItem, Q, Subscribers) ->
	add_item_to_q(NewItem, [], Q, Subscribers).

% add_item_to_q(NewItem::rssItem(), L1::[rssItem()], [], Subscribers::set())
% Вспомогательная функция, осуществляющая добавление 
% нового элемента в пустую очередь.
add_item_to_q(NewItem, L1, [], Subscribers) ->
	?INFO("New item added to Queue. PID: ~p~n", [self()]), 
	[add_item(Pid, NewItem) || Pid <- sets:to_list(Subscribers)],
	L1++[NewItem];

% add_item_to_q(NewItem::rssItem(), L1::[rssItem()], L::[rssItem()], Subscribers::set())
% Вспомогательная функция, осуществляющая добавление нового элемента в непустую очередь.
add_item_to_q(NewItem, L1, L = [OldItem | Rest], Subscribers) ->
	case rss_parse:compare_feed_items(OldItem, NewItem) of
		same -> 
			?INFO("Same items. Item ignored. PID: ~p~n",[self()]),
			L1++L;
		updated -> 
			?INFO("Updated item. PID: ~p~n",[self()]),
			[add_item(Pid, NewItem) || Pid <- sets:to_list(Subscribers)],
			L1++Rest++[NewItem];
		different -> 
			add_item_to_q(NewItem, L1++[OldItem], Rest, Subscribers)
	end.

% sort([rssItem()])
% Функция сортировки списка элементов очереди по возрастанию даты публикации.
sort([]) -> [];
sort([H|T]) -> sort([X || X <- T, rss_parse:get_item_time(X) < rss_parse:get_item_time(H)]) 
					++ [H] 
					++ sort([X || X <-T , rss_parse:get_item_time(X) >= rss_parse:get_item_time(H)]).