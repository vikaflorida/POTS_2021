-module(mobius).
-export([is_prime/1, prime_factors/1, find_square_multiples/2]).

% Функция, вычисляющяя, является ли N простым числом 
% 1 не является простым числом (особое число)
is_prime(1) -> false;
% Если >= 2, осуществляем поиск делителя
is_prime(N) when N >= 2 -> is_prime(N, 2).							

% Вспомогательная функция, осуществляющая поиск делителя числа N в диапазоне [2, sqrt(N)]
% Если Divisor > sqrt(N), то делителя нет - число простое
is_prime(N, Divisor) when Divisor * Divisor > N -> true;			
is_prime(N, Divisor) -> 
	if
		% Делитель найден - число составное
		N rem Divisor == 0 -> false;
		% Инкрементируем Divisor и продолжаем поиск
		true -> is_prime(N, Divisor + 1)
	end.

% Функция, возвращающая список простых сомножителей числа N
prime_factors(N) -> prime_factors(N, 2, []).

% Вспомогательная функция, осуществляющая поиск делителя числа N в диапазоне от [2, N]
prime_factors(N, Divisor, Result) when Divisor > N -> Result;
prime_factors(N, Divisor, Result) ->
	case is_prime(Divisor) of
		true -> case is_prime(N) of
					%Если N - простое, записываем его в список		
					true -> [N | Result];		
					false ->
						if 
							% Если делиться без остатка и Divisor - простое
							% Копируем в список и делаем рекурсивный вызов с результатом деления	
							N rem Divisor == 0 ->
								prime_factors(N div Divisor, Divisor, [Divisor | Result]); 	
							% Иначе, инкрементируем делитель
							true -> prime_factors(N, Divisor + 1, Result) 						
						end 
				end;
		false -> prime_factors(N, Divisor + 1, Result) 
	end.

% Функция, определяющая, делится ли аргумент на квадрат простого числа
is_square_multiple(N) -> 
	% Получаем осортированный список сомножителей числа N
	SortedPrimeFactors = lists:sort(prime_factors(N)),					
	% Если длина списка болььше размера множества - значит есть повторяющиеся сомножители
	length(SortedPrimeFactors) > sets:size(sets:from_list(SortedPrimeFactors)).	
	
% Возвращает первое число из найденно последовательности чисел в диапазоне [2, MaxN],
% делящихся на квадрат простого числа, длинной Count,
% если последовательность не найдена, возвращает fail
find_square_multiples(Count, MaxN) -> find_square_multiples(Count, MaxN, 2, 2, 0).

% Вспомогательная функция Current - текущее число последовательности,
% First - первое число искомой последовательности, SeqLen - ее длина.
% Если наши Count чисел, возвращаем первое
find_square_multiples(Count, _, _, First, SeqLen) when SeqLen == Count -> First;
% Если текущий элемент больше MaxN (а SeqLen != Count), то fail		
find_square_multiples(_, MaxN, Current, _, _) when Current > MaxN -> fail;				
find_square_multiples(Count, MaxN, Current, First, SeqLen) ->
	case is_square_multiple(Current) of
		% Запоминаем в First первое число и жвижемся дальше
		true -> find_square_multiples(Count, MaxN, Current+1, First, SeqLen+1);			
		% Переходим на новую итерацию
		false -> find_square_multiples(Count, MaxN, Current+1, Current+1, 0)			
	end.
