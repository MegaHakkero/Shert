% sevenz.erl - 7zip wrapper
% Copyright (C) 2024  Marisa <private>
% 
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as published
% by the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Affero General Public License for more details.
% 
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.

-module(sevenz).
-export([listing/1, listing/2, extract/2]).

-include("logging.hrl").

with_7z(F) when is_function(F) ->
	case os:find_executable("7z") of
		false -> {error, sevenz_not_found};
		File -> apply(F, [File])
	end.

get_paths(L) ->
	M = maps:get(listing, L),
	case maps:get(names_only, L) of
		true -> M;
		false -> lists:map(fun(E) -> maps:get("Path", E) end, M)
	end.

% 7z returns an empty line followed by the error
% at least in the case of a nonexistent archive
parse([""|Rest]) -> parse(Rest);
parse([V|Rest]) ->
	S = string:lowercase(string:slice(V, 0, 5)),
	if
		S =:= "error" -> ?LOG_ERROR("7z: ~ts", [V]), {error, sevenz, V};
		true -> parse([], #{}, [V|Rest])
	end.

parse(Acc, _, []) -> {ok, lists:reverse(Acc)};
parse(Acc, CurrentFile, [""|Rest]) when map_size(CurrentFile) == 0 ->
	parse(Acc, #{}, Rest);
parse(Acc, CurrentFile, [""|Rest]) ->
	parse([CurrentFile|Acc], #{}, Rest);
parse(Acc, CurrentFile, [Field|Rest]) ->
	case string:split(Field, " = ") of
		[K,""] -> parse(Acc, maps:put(K, "N/A", CurrentFile), Rest);
		[K,V]  -> parse(Acc, maps:put(K, V, CurrentFile), Rest);
		[K|_]  -> parse(Acc, maps:put(K, "N/A", CurrentFile), Rest)
	end.

finalize_listing(L, false) -> L;
finalize_listing(L, true) -> lists:map(fun(E) -> maps:get("Path", E) end, L).

% I wish I could use erlang's zip library. this is atrocious
listing(FileName) -> listing(FileName, true).
listing(FileName, NamesOnly) ->
	with_7z(fun(E) -> listing(FileName, NamesOnly, E) end).
listing(FileName, NamesOnly, Exec) ->
	PropsRaw = string:split(os:cmd(Exec ++ " l -ba -slt " ++ FileName), "\n", all),
	WindowsProof = lists:map(fun(A) -> string:trim(A, both, "\r") end, PropsRaw),
	case parse(WindowsProof) of
		{ok, Listing} -> #{
			path_name => filename:absname(FileName),
			listing => finalize_listing(Listing, NamesOnly),
			names_only => NamesOnly
		};
		W -> W
	end.

extract(Listing, Path) ->
	with_7z(fun(E) -> extract(Listing, Path, E) end).
extract(Listing, Path, Exec) ->
	Command = Exec ++ " -ba -so e " ++
		lists:join(" ", [maps:get(path_name, Listing), Path]),
	case lists:any(fun(P) -> string:equal(P, Path) end, get_paths(Listing)) of
		true -> os:cmd(Command);
		false -> {error, path_not_found}
	end.
