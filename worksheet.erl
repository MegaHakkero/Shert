% worksheet.erl - worksheet loader, ets only
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

-module(worksheet).
-export([load_all_ets/2]).

-include_lib("xmerl/include/xmerl.hrl").

load_all_ets(L, T) ->
	lists:foreach(fun([ID, Path]) ->
		{X, _} = xmerl_scan:string(sevenz:extract(L, Path)),
		{value, SDXML} = xmlutil:firstByName(X, 'sheetData'),
		load(T, ID, SDXML) end,
		ets:match(T, {{sheet, '$1'}, '$2', '_'})),
	ok.

load(T, ID, SheetDataXML) ->
	Rows = xmlutil:elementsByName(SheetDataXML, row),
	lists:foreach(fun(R) ->
		RowRef = xmlutil:getAttributeInt(R, r) - 1, % convert to 0-based
		load_cells(T, ID, xmlutil:elementsByName(R, c), RowRef) end,
		Rows),
	ok.

load_cells(T, ID, Cells, RowRef) ->
	lists:foreach(fun(C) ->
		{Col, _} = shert:cellref_alpha_portion(xmlutil:getAttribute(C, r)),
		{Type, Val} = parse_value(C, xmlutil:getAttribute(C, t, "n")),
		ets:insert(T, {{cell, ID, {RowRef, Col}}, style_placeholder, Type, Val}) end,
		Cells),
	ok.

func_or_no_value(Cell, Tag, Type, F) ->
	case xmlutil:firstByName(Cell, Tag) of
		{value, V} -> {Type, apply(F, [V])};
		false -> {Type, blank}
	end.

parse_number(TextNode) ->
	case catch list_to_integer(TextNode) of
		{'EXIT', _} -> case catch list_to_float(TextNode) of
			{'EXIT', _} -> TextNode;
			F -> F
		end;
		I -> I
	end.

% not my job to check validity - only to report data
parse_value(Cell, "b") ->
	func_or_no_value(Cell, v, bool, fun(VX) ->
		{value, V} = xmlutil:firstTextNode(VX),
		list_to_integer(string:trim(V)) end);

% why is this a datatype? excel already has formatting
% for dates via number format codes
parse_value(Cell, "d") ->
	func_or_no_value(Cell, v, date_iso8601, fun(VX) ->
		{value, V} = xmlutil:firstTextNode(VX), string:trim(V) end);

parse_value(Cell, "e") ->
	func_or_no_value(Cell, v, error, fun(VX) ->
		{value, V} = xmlutil:firstTextNode(VX), string:trim(V) end);

parse_value(Cell, "inlineStr") ->
	func_or_no_value(Cell, is, inline_string, fun(SX) ->
		sst:parse_item(SX) end);

parse_value(Cell, "n") ->
	func_or_no_value(Cell, v, number, fun(VX) ->
		{value, V} = xmlutil:firstTextNode(VX),
		parse_number(string:trim(V)) end);

parse_value(Cell, "s") ->
	func_or_no_value(Cell, v, sst_index, fun(VX) ->
		{value, V} = xmlutil:firstTextNode(VX),
		list_to_integer(string:trim(V)) end);

parse_value(Cell, "str") ->
	func_or_no_value(Cell, v, plain_string, fun(VX) ->
		{value, V} = xmlutil:firstTextNode(VX), string:trim(V) end).
