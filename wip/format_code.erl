% format_code.erl - [WIP] - stylesheet formatCode routines
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

-module(format_code).
-export([parse/1, num_fmt_to_type/1]).

-define(DUP3(V), {V, V, V}).
-define(CODE_TIME_ONLY, {time_only, number, time_only}).
% ^ negative time value doesn't make sense

% this module is only this long because for some reason microsoft
% decided to make months use the same character as minutes,
% rather than simply picking some other, unambiguous character. nice!

% false = state unset
% do not pass the last state field to other clauses
parse(S) ->
	Sects = [parse(X, { number, false }) ||
		X <- split_sections(list_to_binary(S))],
	construct_code(Sects).

% code[0] = positive number type
% code[1] = negative number type
% code[2] = zero number type
% code[3] is ignored, text format
construct_code([P,N,Z|_]) -> {P,N,Z};
construct_code([P,N]) -> {P,N,number};
construct_code([P]) -> {P,number,number}.

parse(<<>>, { Fmt, _ }) -> Fmt;
% unambiguous (stateless) specifiers - date
parse(<<"yy", _/binary>>, _) -> date_time;
parse(<<"mmm", _/binary>>, _) -> date_time;
parse(<<"d", _/binary>>, _) -> date_time;
parse(<<"e", _/binary>>, _) -> date_time;
parse(<<"g", _/binary>>, _) -> date_time;
parse(<<"r", _/binary>>, _) -> date_time;

% "N time elapsed" formats; months make no sense here.
% they also don't make sense combined with dates.
% probably OK to stop parsing
parse(<<"[hh]", _/binary>>, _) -> elapsed;
parse(<<"[h]", _/binary>>, _) -> elapsed;
parse(<<"[mm]", _/binary>>, _) -> elapsed;
parse(<<"[m]", _/binary>>, _) -> elapsed;
parse(<<"[ss]", _/binary>>, _) -> elapsed;
parse(<<"[s]", _/binary>>, _) -> elapsed;

% general format overrides everything
parse(<<"General", _/binary>>, _) -> number;

% hour
parse(<<"h", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"hh", R/binary>>, _) -> parse(R, { time_only, has_hour });
parse(<<"h", R/binary>>, _) -> parse(R, { time_only, has_hour });

% minute
parse(<<"mm", R/binary>>, { _, has_hour }) ->
	parse(R, { time_only, false });

parse(<<"mm", R/binary>>, { F, false }) ->
	parse(R, { F, has_minute });

parse(<<"m", R/binary>>, { _, has_hour }) ->
	parse(R, { time_only, false });

parse(<<"m", R/binary>>, { F, false }) ->
	parse(R, { F, has_minute });

% second
parse(<<"ss", R/binary>>, _) -> parse(R, { time_only, false });
parse(<<"s", R/binary>>, _) -> parse(R, { time_only, false });

% code stubs - only used to terminate parsing when m/mm precedes them
parse(<<"0", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"#", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"?", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"%", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"E-", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"E+", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"e-", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"e+", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"*", _/binary>>, { _, has_minute }) -> date_time;
parse(<<"@", _/binary>>, { _, has_minute }) -> date_time;

% final catch-alls
parse(<<"\\", _/utf8, R/binary>>, S) -> parse(R, S);
parse(<<"\"", R/binary>>, S) -> parse(skip_string(R), S);
parse(<<"[", R/binary>>, S) -> parse(skip_instruction(R), S);
parse(<<_/utf8, R/binary>>, S) -> parse(R, S).

skip_string(<<"\\", _/utf8, R/binary>>) -> skip_string(R);
skip_string(<<"\"", R/binary>>) -> R;
skip_string(<<_/utf8, R/binary>>) -> skip_string(R).

skip_instruction(<<"]", R/binary>>) -> R;
skip_instruction(<<_/utf8, R/binary>>) -> skip_instruction(R).

num_fmt_to_type(N) when N < 14 -> ?DUP3(number);
num_fmt_to_type(N) when N > 36, N < 41 -> ?DUP3(number);
num_fmt_to_type(48) -> ?DUP3(number);
num_fmt_to_type(N) when N > 13, N < 18 -> ?DUP3(date_time);
num_fmt_to_type(22) -> ?DUP3(date_time);
num_fmt_to_type(N) when N > 17, N < 22 -> ?CODE_TIME_ONLY;
num_fmt_to_type(45) -> ?CODE_TIME_ONLY;
num_fmt_to_type(47) -> ?CODE_TIME_ONLY;
num_fmt_to_type(46) -> ?DUP3(elapsed);
num_fmt_to_type(_) -> ?DUP3(number).

% split the entire format code into sections for format detection
split_sections(S) -> split_sections(S, <<>>, []).

split_sections(<<>>, C, Acc) -> lists:reverse([C|Acc]);
split_sections(<<";", R/binary>>, C, Acc) -> split_sections(R, <<>>, [C|Acc]);
split_sections(<<"\"", R/binary>>, C, Acc) ->
	{S, Rest} = read_string(R),
	split_sections(Rest, <<C/binary, "\""/utf8, S/binary>>, Acc);
split_sections(<<"[", R/binary>>, C, Acc) ->
	{I, Rest} = read_instruction(R),
	split_sections(Rest, <<C/binary, "["/utf8, I/binary>>, Acc);
split_sections(<<Ch/utf8, R/binary>>, C, Acc) ->
	split_sections(R, <<C/binary, Ch/utf8>>, Acc).

read_string(Bin) -> read_string(Bin, <<>>).
read_string(<<"\\", C/utf8, R/binary>>, Acc) -> read_string(R, <<Acc/binary, "\\"/utf8, C/utf8>>);
read_string(<<"\"", R/binary>>, Acc) -> {<<Acc/binary, "\""/utf8>>, R};
read_string(<<C/utf8, R/binary>>, Acc) -> read_string(R, <<Acc/binary, C/utf8>>).

read_instruction(Bin) -> read_instruction(Bin, <<>>).
read_instruction(<<"]", R/binary>>, Acc) -> {<<Acc/binary, "]"/utf8>>, R};
read_instruction(<<C/utf8, R/binary>>, Acc) -> read_instruction(R, <<Acc/binary, C/utf8>>).
