% warehouse.erl by Geoff McCord
% Uses Mnesia tables

-module(warehouse).
-include_lib("stdlib/include/qlc.hrl").
-record(inv, {pid, name, count}).
-export([loop/0]).



% how many of this item do we have? note: mnesia returns empty list if no entry
getcount(Name) ->
	Pid = self(),
	Term = #inv{pid = Pid, name = Name, _ = '_'},
	F = fun() ->
		mnesia:match_object(Term)
	end,
	Result = mnesia:transaction(F),
	Status = element(1, Result),
	Value = element(2, Result),
	if
		Value == []      -> 0;
		Status == atomic -> element(4, hd(Value));
		true             -> io:format("Error: getcount():~n ~p~n", [Result]), 0
	end.



% add to inventory of item. note: since the table is a bag, the item must be wholly deleted
addcount(Count, Item) ->
	Pid = self(),
	F = fun() ->
		Pattern = #inv{pid = Pid, name = Item, count = '$1'},
		Result = mnesia:select(inv, [{ Pattern, [], ['$1'] }]),
		if
			Result == [] -> mnesia:write(#inv{pid = Pid, name = Item, count = Count});
			is_number(hd(Result)) ->
				NewCount = hd(Result) + Count,
				mnesia:delete_object(#inv{pid = Pid, name = Item, count = hd(Result)}),
				mnesia:write(#inv{pid = Pid, name = Item, count = NewCount});
			true -> io:format("Error: addcount():~n ~p~n", [Result]), fail
		end
	end,
	mnesia:transaction(F).



% remove some inventory or return atom 'fail' to indicate insufficient quantity
sellcount(Count, Item) ->
	Pid = self(),
	F = fun() ->
		Pattern = #inv{pid = Pid, name = Item, count = '$1'},
		Result = mnesia:select(inv, [{ Pattern, [], ['$1'] }]),
		if
			Result == []              -> fail;
			not is_number(hd(Result)) -> io:format("Error: sellcount():~n ~p~n", [Result]);
			hd(Result) < Count        -> fail;
			true                      ->
				NewCount = hd(Result) - Count,
				mnesia:delete_object(#inv{pid = Pid, name = Item, count = hd(Result)}),
				mnesia:write(#inv{pid = Pid, name = Item, count = NewCount})
		end
	end,
	mnesia:transaction(F).



% the main message function
loop() ->
	receive
		{Ho, ask, Item} ->
				Ho ! {askreply, self(), getcount(Item), Item};

		{Ho, add, Count, Item} ->
				case addcount(Count, Item) of
					{atomic, ok} -> Ho ! {addreply, self(), Count, Item};
					_ -> Ho ! {addfail, self(), Count, Item}
				end;

		{Ho, sell, Count, Item} ->
				case sellcount(Count, Item) of
					{atomic, fail} -> Ho ! {sellfail, self(), Count, Item};
					{atomic, ok} -> Ho ! {sellreply, self(), Count, Item};
					_ -> ok
				end;

		_ ->
				io:format("Error: Unknown message received by warehouse.~n")
	end,
	loop().