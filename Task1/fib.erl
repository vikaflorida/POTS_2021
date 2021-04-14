-module(fib).
-export([fib_p/1, fib_g/1, tail_fib/1]).
% 2. Используйте сравнение аргумента с образцом
fib_p(X) ->
	if
		X == 0 -> 0;
		X == 1 -> 1;
		X >= 2 -> fib_p(X - 1) + fib_p(X - 2)
	end.

% 3. Используйте сторожевые последовательности
fib_g(X) when X == 0 -> 0;
fib_g(X) when X == 1 -> 1;
fib_g(X) when X >= 2 -> 
	fib_g(X-1) + fib_g(X-2).

% 4. Используйте хвостовую рекурсию
tail_fib(X) -> tail_fib_helper(X, 0, 1).
tail_fib_helper(0, Result, _) -> Result;
tail_fib_helper(1, _, Result) -> Result;
tail_fib_helper(X, Previous, Result) when X >= 2 -> 
    tail_fib_helper(X-1, Result, Result+Previous).