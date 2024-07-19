% stylesheet.erl - [WIP] - stylesheet parser
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

-module(stylesheet).
-export([parse/2]).

-include("logging.hrl").

parse_cell_style_xfs(CellStyleXfsXML) ->
	Xfs = xmlutil:elementsByName(CellStyleXfsXML, xf),
	lists:map(fun(X) ->
		case xmlutil:getAttributeInt(X, 'applyNumberFormat', 1) of
			0 -> number;
			1 -> format_code:num_fmt_to_type(xmlutil:getAttributeInt(X, 'numFmtId', 0))
		end end, Xfs).

parse_cell_xfs(CellXfsXML, StyleXfs) ->
	Xfs = xmlutil:elementsByName(CellXfsXML, xf),
	lists:map(fun(X) ->
		XfIndex = xmlutil:getAttributeInt(X, 'xfId'),
		case xmlutil:getAttributeInt(X, 'applyNumberFormat', 0) of
			0 -> lists:nth(XfIndex + 1, StyleXfs);
			1 -> format_code:num_fmt_to_type(xmlutil:getAttributeInt(X, 'numFmtId', 0))
		end end, Xfs).

parse(L, Path) ->
	{StylesheetXML, _} = xmerl_scan:string(sevenz:extract(L, Path)),
	{value, StyleXfsXML} = xmlutil:firstByName(StylesheetXML, 'cellStyleXfs'),
	{value, XfsXML} = xmlutil:firstByName(StylesheetXML, 'cellXfs'),
	StyleXfs = parse_cell_style_xfs(StyleXfsXML),
	Xfs = parse_cell_xfs(XfsXML, StyleXfs),
	?LOG_INFO("cellStyleXfs=~B, cellXfs=~B", [length(StyleXfs), length(Xfs)]),
	Xfs.
