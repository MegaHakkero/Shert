% xmlutil.erl - XML utilities
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

-module(xmlutil).
-export([
	tryGetAttribute/2,
	getAttribute/2,
	getAttribute/3,
	tryGetAttributeNS/3,
	getAttributeNS/3,
	getAttributeNS/4,
	tryGetAttributeInt/2,
	getAttributeInt/2,
	getAttributeInt/3,
	elementsByName/2,
	firstByName/2,
	textNodes/1,
	firstTextNode/1
]).

-include_lib("xmerl/include/xmerl.hrl").

get_name(#xmlElement{name = N}) -> N;
get_name(_) -> false.

value_or_default(S, D) ->
	case S of
		{value, V} -> V;
		_ -> D
	end.

value_func(S, F) ->
	case S of
		{value, V} -> {value, apply(F, [V])};
		Fail -> Fail
	end.

tryGetAttribute(#xmlElement{attributes = Attrib}, Name) ->
	S = lists:search(fun(X) -> X#xmlAttribute.name == Name end, Attrib),
	value_func(S, fun(V) -> V#xmlAttribute.value end).

getAttribute(E, Name) -> {value, V} = tryGetAttribute(E, Name), V.
getAttribute(E, Name, Default) ->
	value_or_default(tryGetAttribute(E, Name), Default).

tryGetAttributeNS(#xmlElement{attributes = Attrib}, Namespace, Name) ->
	S = lists:search(fun(X) -> case X#xmlAttribute.nsinfo of
		{NS, N} -> (NS == Namespace) and (N == Name);
		_ -> false
		end end, Attrib),
	value_func(S, fun(V) -> V#xmlAttribute.value end).

getAttributeNS(E, Namespace, Name) ->
	{value, V} = tryGetAttributeNS(E, Namespace, Name), V.
getAttributeNS(E, Namespace, Name, Default) ->
	value_or_default(tryGetAttributeNS(E, Namespace, Name), Default).

tryGetAttributeInt(E, Name) ->
	S = tryGetAttribute(E, Name),
	value_func(S, fun(V) -> {I, _} = string:to_integer(V), I end).

getAttributeInt(E, Name) -> {value, V} = tryGetAttributeInt(E, Name), V.
getAttributeInt(E, Name, Default) ->
	value_or_default(tryGetAttributeInt(E, Name), Default).

elementsByName(#xmlElement{content = C}, Name) ->
	lists:filter(fun(X) -> get_name(X) == Name end, C).

firstByName(#xmlElement{content = C}, Name) ->
	lists:search(fun(X) -> get_name(X) == Name end, C).

textNodes(#xmlElement{content = C}) ->
	lists:map(fun(#xmlText{value = V}) -> V end,
		lists:filter(fun(X) -> is_record(X, 'xmlText') end, C)).

firstTextNode(#xmlElement{content = C}) ->
	value_func(lists:search(fun(X) -> is_record(X, 'xmlText') end, C),
		fun(#xmlText{value = V}) -> V end).
