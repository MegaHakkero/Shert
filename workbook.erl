% workbook.erl - workbook parser
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

-module(workbook).
-export([parse/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include("logging.hrl").

parse_rel_file(L, P) ->
	[D, B] = string:split(P, "/", trailing),
	RP = D ++ "/_rels/" ++ B ++ ".rels",
	{XmlRels, _} = xmerl_scan:string(sevenz:extract(L, RP)),
	lists:map(fun(X) -> {
		xmlutil:getAttribute(X, 'Id'),
		D ++ "/" ++ xmlutil:getAttribute(X, 'Target')
		} end, xmlutil:elementsByName(XmlRels, 'Relationship')).

str_ends_with(S, E) -> string:equal(string:slice(S,
		string:length(S) - string:length(E)), E).

% I can't rely on the entire namespace URI because
% namespaces in XML are super garbage for availability and consistency.
% I also can't assume that the id attribute will always
% use the "r" prefix, so I have to compute the actual prefix
% like this. :/
get_rel_namespace(Xml) ->
	{value, {NSName, _}} = lists:search(fun(X) ->
		{_, T} = X, str_ends_with(atom_to_list(T), "/relationships") end,
		Xml#xmlElement.namespace#xmlNamespace.nodes),
	NSName.

make_sheet_list(XmlSheets, Rels) ->
	NSRel = get_rel_namespace(XmlSheets),
	Sheets = xmlutil:elementsByName(XmlSheets, sheet),
	lists:map(fun(X) ->
		RelID = xmlutil:getAttributeNS(X, NSRel, "id"),
		{value, {_, Path}} = lists:search(fun(Y) -> {R, _} = Y, R == RelID end, Rels),
		{
			Path,
			xmlutil:getAttribute(X, name),
			xmlutil:getAttributeInt(X, sheetId)
		} end, Sheets).

parse(L, Path) ->
	{XmlWorkbook, _} = xmerl_scan:string(sevenz:extract(L, Path)),
	{value, XmlSheets} = xmlutil:firstByName(XmlWorkbook, sheets),
	Rels = parse_rel_file(L, Path),
	Sheets = make_sheet_list(XmlSheets, Rels),
	lists:foreach(fun(S) ->
		{_, N, ID} = S, ?LOG_INFO("sheetId=~B, ~ts", [ID, N]) end, Sheets),
	Sheets.
