% sst.erl - shared string table parser
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

-module(sst).
-export([parse/2, parse_item/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("logging.hrl").

% each fragment is either an <r> or <t> element
parse_item(#xmlElement{content = C}) -> parse_item([], C).

parse_item(Acc, [F|Rest]) ->
	case parse_frag(F) of
		false -> parse_item(Acc, Rest);
		S -> parse_item([S|Acc], Rest)
	end;
parse_item(Acc, []) -> lists:concat(lists:reverse(Acc)).

% plain text element, contains an #xmlText node
parse_frag(#xmlElement{name = t, content = [C|_]}) -> C#xmlText.value;
% rich text element, contains format stuff and a plain text element
parse_frag(RichText = #xmlElement{name = r}) ->
	{value, Text} = xmlutil:firstByName(RichText, t),
	parse_frag(Text);
% ignore text nodes at the item level. they pop up for indentation lol
parse_frag(#xmlText{}) -> false.

parse_strings(SST = #xmlElement{name = sst}) ->
	Str = xmlutil:elementsByName(SST, si),
	lists:map(fun(E) -> parse_item(E) end, Str).

parse(SzListing, Path) ->
	{XmlSST, _} =
		xmerl_scan:string(sevenz:extract(SzListing, Path)),
	?LOG_INFO("~B strings", [xmlutil:getAttributeInt(XmlSST, uniqueCount)]),
	parse_strings(XmlSST).
