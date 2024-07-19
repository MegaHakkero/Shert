% shert.erl - main module; loader and utility routines
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

-module(shert).
-export([
	load/1, load/2,
	cellref/1,
	cellref_alpha_portion/1,
	sheet/2,
	celldata/3,
	celldata_area/4
]).

-include("mimetype.hrl").

parse_ctypes(SzListing) ->
	{XmlContentTypes, _} =
		xmerl_scan:string(sevenz:extract(SzListing, "[Content_Types].xml")),
	Overrides = xmlutil:elementsByName(XmlContentTypes, 'Override'),
	lists:map(fun(X) -> {
		string:trim(xmlutil:getAttribute(X, 'PartName'), leading, "/"),
		xmlutil:getAttribute(X, 'ContentType')
		} end, Overrides).

get_part_by_type(CT, T) ->
	{value, {P, _}} = lists:search(fun(X) ->
		{_, XT} = X, XT == T end, CT),
	P.

% the ETS table passed to load/2 should be a set table with keypos 1 (default)
load(Path) -> load(ets:new(spreadsheet, [set]), Path).
load(T, Path) ->
	L = sevenz:listing(Path),
	CTypes = parse_ctypes(L),
	% there can only be one SST, workbook and stylesheet, which means
	% I can use [Content_Types].xml to locate them directly
	% instead of relying on annoying relationship garbage.
	% TODO: implement cell styles - this is a huge task (microsoft sucks)
	SST = sst:parse(L, get_part_by_type(CTypes, ?MIME_SST)),
	Worksheets = workbook:parse(L, get_part_by_type(CTypes, ?MIME_WORKBOOK)),
	lists:foreach(fun({I, E}) -> ets:insert(T, {{sst_entry, I}, E}) end,
		lists:enumerate(0, SST)),
	lists:foreach(fun({P, N, I}) -> ets:insert(T, {{sheet, I}, P, N}) end,
		Worksheets),
	worksheet:load_all_ets(L, T),
	T.

cellref(RefA1) ->
	{C, Rest} = cellref_alpha_portion(RefA1),
	{list_to_integer(Rest) - 1, C}.

cellref_alpha_portion(RefA1) ->
	cellref_alpha_portion(string:uppercase(RefA1), 0).
cellref_alpha_portion([C|Rest], Acc) when C >= $A, C =< $Z ->
	cellref_alpha_portion(Rest, Acc * 26 + (C - ($A - 1)));
cellref_alpha_portion([C|Rest], Acc) when C >= $0, C =< $9 ->
	{Acc - 1, [C|Rest]};
cellref_alpha_portion([], Acc) -> {Acc - 1, []}.

sheet(T, Name) ->
	[[ID]] = ets:match(T, {{sheet, '$1'}, '_', Name}), ID.

celldata_fetch(_, []) -> no_cell;
celldata_fetch(T, [{_, _, CT, CV}]) -> celldata_fetch(T, CT, CV).

celldata_fetch(_, _, blank) -> blank;
celldata_fetch(T, CT, CV) ->
	case CT of
		bool -> CV;
		date_iso8601 -> CV;
		error -> CV;
		inline_string -> CV;
		number -> CV;
		sst_index ->
			[{_, S}] = ets:lookup(T, {sst_entry, CV}), S;
		plain_string -> CV
	end.

celldata(T, ID, {R, C}) ->
	celldata_fetch(T, ets:lookup(T, {cell, ID, {R, C}}));
celldata(T, ID, RefA1) -> celldata(T, ID, cellref(RefA1)).

celldata_area(T, ID, {R1, C1}, {R2, C2}) ->
	lists:map(fun(Row) -> celldata_row(T, ID, Row, C1, C2) end,
		lists:seq(R1, R2));
celldata_area(T, ID, Ref1A1, Ref2A1) ->
	celldata_area(T, ID, cellref(Ref1A1), cellref(Ref2A1)).

celldata_row(T, ID, Row, From, To) ->
	lists:map(fun(C) -> celldata(T, ID, {Row, C}) end, lists:seq(From, To)).
