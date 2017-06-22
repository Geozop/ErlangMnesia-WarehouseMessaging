% Project #4 w/ Mnesia
% Geoffrey McCord

% To start, do "H = spawn(fun homeoffice:loop/0)"
% Homeoffice available message commands:
%   {createwh, Warehouse_name}
%   {ask, Warehouse_name, Item_name}
%   {add, Warehouse_name, Count, Item_name}
%   {sell, Warehouse_name, Count, Item_name}
%   {transfer, From_warehouse_name, To_warehouse_name, Count, Item_name}
%   {inquire, Item_name)   (note: this function calculates total, code based off pmap)

-module(homeoffice).
-include_lib("stdlib/include/qlc.hrl").
-record(wh, {name, pid}).
-record(inv, {pid, name, count}).
-export([loop/0]).



% user wishes to add a warehouse
addwarehouse(Name) ->
	F = fun() ->
		Pid = spawn(fun warehouse:loop/0),
		Entry = #wh{name = Name, pid = Pid},
		mnesia:write(Entry)
	end,
	mnesia:transaction(F).



% is the user-supplied name an actual warehouse?
iswarehouse(Name) ->
	F = fun() -> mnesia:read(wh, Name) end,
	R = mnesia:transaction(F),
	% Status = element(1, R),
	% Result = element(2, R),
	% io:format("~p '~p'~n", [Status, Result]),
	case element(1, R) of
		aborted -> fail;
		atomic ->
			case element(2, R) of
				[] -> false;
				_ -> true
			end
	end.



% get the list of all warehouse names; foldl used since mnesia returns a list of tuples
getwarehouses() ->
	case mnesia:system_info(is_running) of
		no -> 	[];
		_ ->	
			Term = mnesia:table_info(wh, wild_pattern),
			F = fun() ->
				mnesia:match_object(Term)
			end,
			{atomic, Whs} = mnesia:transaction(F),
			lists:foldl(fun(X, List) -> lists:append(List, [element(3, X)]) end, [], Whs)
	end.



% convert a warehouse name to its Pid for messaging purposes
getwarehousepid(Name) ->
	Term = #wh{name = Name, _ = '_'},
	F = fun() ->
		mnesia:match_object(Term)
	end,
	R = mnesia:transaction(F),
	{atomic, Result} = R,
	if
		Result == [] -> io:format("Warning: no such warehouse name; returning self()~n"), self();
		true -> element(3, hd(Result))
	end.



% messages received by the homeoffice contain only the warehouse's Pid, so convert for printing purposes
getwarehousename(Pid) ->
	Term = #wh{pid = Pid, _ = '_'},
	F = fun() ->
		mnesia:match_object(Term)
	end,
	{atomic, Result} = mnesia:transaction(F),
	if
		Result == [] -> io:format("Warning: no such warehouse name; returning fake name~n"),nosuchwarehouse;
		true -> element(2, hd(Result))
	end.



% gather messages from all warehouses to obtain sum
gatherinquiry([Wh|T], Running) ->
	receive
		{askreply, Wh, Ret, Item} ->
			io:format("Warehouse '~p' has ~p of '~p'~n",[getwarehousename(Wh), Ret, Item]),
			gatherinquiry(T, Ret + Running)
	end;

gatherinquiry([], Running) -> Running.



% "intro" function to set up lists and send messages
gatherinquirybegin(Whs, Item) ->
	Selfpid = self(),
	lists:foreach(fun(W) -> W ! {Selfpid, ask, Item} end, Whs),
	gatherinquiry(Whs, 0).



% confirm FromWh has enough of item before doing transfer
transfercheck(FromWhPid, ToWhPid, Count, Item) ->
	receive
		{askreply, FromWhPid, Ret, Item} ->
		if
			Ret < Count -> io:format("Warehouse '~p' does not have ~p of item '~p'. Aborted.~n", [getwarehousename(FromWhPid), Count, Item]);
			true ->
				Selfpid = self(),
				FromWhPid ! {Selfpid, sell, Count, Item},
				ToWhPid ! {Selfpid, add, Count, Item}
		end;

		_ -> io:format("Unknown message during transfercheck(). Aborting.~n")
	end.



% the main message function
loop() ->
	receive

% % % % % Warehouse controls

		{ask, Wh, Item} ->
				case iswarehouse(Wh) of
					true -> getwarehousepid(Wh) ! {self(), ask, Item};
					false -> io:format("No such warehouse '~p'~n", [Wh]);
					Result -> io:format("Error: iswarehouse() returned ~p~n", [Result])
				end;

		{add, Wh, Count, Item} when is_number(Count) ->
				case iswarehouse(Wh) of
					true -> getwarehousepid(Wh) ! {self(), add, Count, Item};
					false -> io:format("No such warehouse '~p'~n", [Wh]);
					Result -> io:format("Error: iswarehouse() returned ~p~n", [Result])
				end;

		{sell, Wh, Count, Item} when is_number(Count) ->
				case iswarehouse(Wh) of
					true -> getwarehousepid(Wh) ! {self(), sell, Count, Item};
					false -> io:format("No such warehouse '~p'~n", [Wh]);
					Result -> io:format("Error: iswarehouse() returned ~p~n", [Result])
				end;

		{inquire, Item} ->
				Whs = getwarehouses(),
				io:format("Total: ~p of item '~p'~n", [gatherinquirybegin(Whs, Item), Item]);

		{transfer, FromWh, ToWh, Count, Item} when is_number(Count) ->
				ToWhBool = true /= iswarehouse(ToWh),
				FromWhBool = true /= iswarehouse(FromWh),
				if
					ToWhBool -> io:format("Error: Warehouse '~p' doesn't exist.~n", [ToWh]);
					FromWhBool -> io:format("Error: Warehouse '~p' doesn't exist.~n", [FromWh]);
					ToWh == FromWh -> io:format("Error: Source and destination are the same!~n");
					true ->
						getwarehousepid(FromWh) ! {self(), ask, Item},
						transfercheck(getwarehousepid(FromWh), getwarehousepid(ToWh), Count, Item)
				end;

% % % % % Warehouse Replies

		{addfail, Wh, Count, Item} ->
				io:format("Error: Warehouse '~p' could not add ~p of item '~p'. Aborted.~n", [getwarehousename(Wh), Count, Item]);

		{sellfail, Wh, Count, Item} ->
				io:format("Error: Warehouse '~p' does not have ~p of item '~p'. Aborted.~n", [getwarehousename(Wh), Count, Item]);

		{sellreply, Wh, Count, Item} ->
				io:format("Warehouse '~p' successfully removed ~p of item '~p'.~n", [getwarehousename(Wh), Count, Item]);

		{addreply, Wh, Count, Item} ->
				io:format("Warehouse '~p' successfully added ~p of item '~p'.~n", [getwarehousename(Wh), Count, Item]);

		{askreply, Wh, Count, Item} ->
				io:format("Warehouse '~p' reports ~p of item '~p'.~n", [getwarehousename(Wh), Count, Item]);

% % % % % Initialization Procedures

		{createwh, Wh} ->
				case iswarehouse(Wh) of
					true -> io:format("Warehouse '~p' already exists. Aborted.~n", [Wh]), loop();
					false ->
						Result = addwarehouse(Wh),
						if
							Result == {atomic, ok} -> io:format("Warehouse '~p' initialized.~n", [Wh]);
							true                   -> io:format("Error: ~p.~nWarehouse '~p' NOT initialized.~n", [Result, Wh])
						end;
					Result -> io:format("Error: iswarehouse() returned ~p~n", [Result])
				end;

		startup ->
				% assume these never fail
				mnesia:create_schema([node()]),
				mnesia:start(),
				mnesia:create_table(wh, [{attributes, record_info(fields, wh)}]),
				mnesia:create_table(inv, [{attributes, record_info(fields, inv)}, {type, bag}]),
				io:format("Homeoffice ready.~n");

		{startup} ->
				self() ! startup;

		_ ->
				io:format("Unknown message received by homeoffice.~n")
	end,
	loop().



% f(), c(homeoffice), c(warehouse), H = spawn(fun homeoffice:loop/0), H ! startup, H ! {createwh, one}, H ! {createwh, two}, H ! {add, one, 2, dogs}.
