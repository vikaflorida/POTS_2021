-module(rss_reader).
-include("logging.hrl").
-export([start/2, server/2]).
-define(RETRIEVE_INTERVAL, 10000). % 200 seconds

% start(Url::url(), QPid::pid()) -> pid
% Функция должна просто запускать новый процесс, вызывая функцию server(Url, QPid), 
% которая реализует основной цикл процесса rss_reader.
start(Url, QPid) ->
	inets:start(),
	spawn(?MODULE, server, [Url, QPid]).

% server(Url::url(), QPid::pid()) 
% Функция реализует цикл отправки http запросов и обработки полученных ответов через определенное
% количество времени TIMEOUT.
server(Url, QPid) ->
	{ok, {_Status = {_, Code, _}, _, Load}} = httpc:request(Url),
	?INFO("HTTP Response: ~p~n", [Code]),
	case Code of
		200 ->
			Feed = xmerl_scan:string(Load),
			case rss_parse:is_rss2_feed(Feed) of
				true -> 
					rss_queue:add_feed(QPid,Feed),
					receive
						after ?RETRIEVE_INTERVAL ->
							server(Url, QPid)
					end;
				false ->
					?ERROR("Version not 2.0! PID: ~p~n", [QPid]),
					error
			end;
		_Else -> error
	end.