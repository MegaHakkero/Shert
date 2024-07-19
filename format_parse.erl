% format_parse.erl - stylesheet formatCode parser, pass 1
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

% I AM #1 PROGRAMMER. FUCK MICROSOFT
% <<< 1039852 EXCEL MONKEYS KILLED >>>
-module(format_parse).
-export([parse/1]).

fmt_is_digit(N) ->
	case N of
		zero_digit -> true;
		nozero_digit -> true;
		aligned_digit -> true;
		_ -> false
	end.

fmt_is_unaligned_digit(N) ->
	case N of
		zero_digit -> true;
		nozero_digit -> true;
		_ -> false
	end.

fmt_is_hour(N) ->
	case N of
		hours_zero -> true;
		hours -> true;
		_ -> false
	end.

with_type(T, Type) ->
	case lists:search(fun(E) -> E == Type end, T) of
		{value, _} -> T;
		false -> [Type|T]
	end.

replace_prev_code(F, PC, NC) ->
	FirstFmts = lists:takewhile(fun(X) -> X /= PC end, F),
	[_|RestFmts] = lists:dropwhile(fun(X) -> X /= PC end, F),
	FirstFmts ++ [NC|RestFmts].

parse(S) ->
	Sects = [parse(X, [], [], false) ||
		X <- split_sections(list_to_binary(S))],
	construct_code(Sects).

% code[0] = positive number type
% code[1] = negative number type
% code[2] = zero number type
% code[3] = text format (why is this here???)
-define(NUM_DEFAULT, {[number],[general_format]}).
-define(TEXT_DEFAULT, {[number],[text]}).
construct_code([P,N,Z,T|_]) -> {P,N,Z,T};
construct_code([P,N,Z]) -> {P,N,Z,?TEXT_DEFAULT};
construct_code([P,N]) -> {P,N,?NUM_DEFAULT,?TEXT_DEFAULT};
construct_code([P]) -> {P,?NUM_DEFAULT,?NUM_DEFAULT,?TEXT_DEFAULT}.

-define(PARSE_CLAUSE(Spec), parse(<<Spec, R/binary>>, T, F, PC)).
-define(PARSE_CLAUSE_EMPTY_FMT(Spec), parse(<<Spec, R/binary>>, T, [], PC)).
-define(FIX_SEC, {TN, FN} = ambiguous_mm_to_minutes(T, F, PC)).
-define(FIX_DIGIT, {TN, FN} = ambiguous_digit_fix(T, F, PC)).
-define(FIX_UNALIGNED_DIGIT, {TN, FN} = ambiguous_unaligned_digit_fix(T, F, PC)).
-define(FIX_DEFAULTS, {TN, FN} = ambiguous_default(T, F, PC)).
-define(FIX_DEFAULTS_EMPTY_FMT, {TN, FN} = ambiguous_default(T, [], PC)).
-define(ADD_CF(Fmt), [Fmt|FN], Fmt).

ambiguous_mm_to_minutes(T, F, PrevCode) ->
	case PrevCode of
		ambiguous_mm ->
			{with_type(T, time), replace_prev_code(F, PrevCode, minutes_zero)};
		ambiguous_m ->
			{with_type(T, time), replace_prev_code(F, PrevCode, minutes)};
		_ -> ambiguous_default(T, F, PrevCode)
	end.

ambiguous_digit_fix(T, [], _) -> {T, []}; % nothing to fix
ambiguous_digit_fix(T, F, PrevCode) ->
	[PFormat|_] = F,
	case PFormat of
		ambiguous_slash ->
			{with_type(T, fraction), replace_prev_code(F, PFormat, fract)};
		% PrevCode could contain ambiguous_m* codes which depend on
		% the previous proper format specifier instead of the previous
		% generic format object
		_ -> ambiguous_default(T, F, PrevCode)
	end.

ambiguous_unaligned_digit_fix(T, [], _) -> {T, []};
ambiguous_unaligned_digit_fix(T, F, PrevCode) ->
	[PFormat|_] = F,
	case PFormat of
		ambiguous_slash ->
			{with_type(T, fraction), replace_prev_code(F, PFormat, fract)};
		ambiguous_comma ->
			{T, replace_prev_code(F, PFormat, thousands_separator)};
		_ -> ambiguous_default(T, F, PrevCode)
	end.

ambiguous_default(T, F, PrevCode) ->
	case PrevCode of
		ambiguous_mm ->
			{with_type(T, date), replace_prev_code(F, PrevCode, months_zero)};
		ambiguous_m ->
			{with_type(T, date), replace_prev_code(F, PrevCode, months)};
		ambiguous_slash ->
			{T, replace_prev_code(F, PrevCode, {str, "/"})};
		ambiguous_comma ->
			{T, replace_prev_code(F, PrevCode, scale_factor)};
		_ -> {T, F}
	end.

parse(<<>>, T, F, PC) -> ?FIX_DEFAULTS,
	{TN, lists:reverse(FN)};

% numbers
?PARSE_CLAUSE("General") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, number), ?ADD_CF(general_format));
?PARSE_CLAUSE("0") -> ?FIX_UNALIGNED_DIGIT,
	parse(R, with_type(TN, number), ?ADD_CF(zero_digit));
?PARSE_CLAUSE("#") -> ?FIX_UNALIGNED_DIGIT,
	parse(R, with_type(TN, number), ?ADD_CF(nozero_digit));
?PARSE_CLAUSE("?") -> ?FIX_DIGIT,
	parse(R, with_type(TN, number), ?ADD_CF(aligned_digit));
?PARSE_CLAUSE_EMPTY_FMT("/") -> parse(R, T, [{str, "/"}], PC);
?PARSE_CLAUSE("/") ->
	[PFormat|_] = F,
	case fmt_is_digit(PFormat) of
		true -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(ambiguous_slash));
		% the lone slash is not a format specifier; no need to FIX_DEFAULTS
		false -> parse(R, T, [{str, "/"}|F], PC)
	end;
?PARSE_CLAUSE("%") -> ?FIX_DEFAULTS,
	case fmt_is_digit(PC) of
		true -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(percent));
		false -> parse(R, T, [{str, "%"}|F], PC)
	end;
?PARSE_CLAUSE("E-") -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(exponent));
?PARSE_CLAUSE("E+") -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(exponent));
?PARSE_CLAUSE("e-") -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(exponent));
?PARSE_CLAUSE("e+") -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(exponent));
?PARSE_CLAUSE_EMPTY_FMT(",") -> ?FIX_DEFAULTS_EMPTY_FMT,
	parse(R, TN, ?ADD_CF(scale_factor));
?PARSE_CLAUSE(",") -> ?FIX_DEFAULTS,
	[PFormat|_] = FN,
	case fmt_is_unaligned_digit(PFormat) of
		true -> parse(R, TN, ?ADD_CF(ambiguous_comma));
		false -> parse(R, TN, ?ADD_CF(scale_factor))
	end;
?PARSE_CLAUSE(".") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, decimal), ?ADD_CF(decimal));

% unambiguous (stateless) specifiers - date. no international
% formats for now (I need to do more research if I get to it,
% but the documentation for them sucks even more than the one
% for americans)
?PARSE_CLAUSE("yyyy") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(year_long));
?PARSE_CLAUSE("yy") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(year_short));
?PARSE_CLAUSE("mmmmm") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(month_name_initial));
?PARSE_CLAUSE("mmmm") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(month_name_full));
?PARSE_CLAUSE("mmm") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(month_name_short));
?PARSE_CLAUSE("dddd") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(day_name_full));
?PARSE_CLAUSE("ddd") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(day_name_short));
?PARSE_CLAUSE("dd") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(day_zero));
?PARSE_CLAUSE("d") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, date), ?ADD_CF(day));

% "N time elapsed" formats; they don't have an
% associated type because they are freestanding
?PARSE_CLAUSE("[hh]") -> ?FIX_DEFAULTS,
	parse(R, TN, ?ADD_CF(elapsed_hours_zero));
?PARSE_CLAUSE("[h]") -> ?FIX_DEFAULTS,
	parse(R, TN, ?ADD_CF(elapsed_hours));
?PARSE_CLAUSE("[mm]") -> ?FIX_DEFAULTS,
	parse(R, TN, ?ADD_CF(elapsed_minutes_zero));
?PARSE_CLAUSE("[m]") -> ?FIX_DEFAULTS,
	parse(R, TN, ?ADD_CF(elapsed_minutes));
?PARSE_CLAUSE("[ss]") -> ?FIX_DEFAULTS,
	parse(R, TN, ?ADD_CF(elapsed_seconds_zero));
?PARSE_CLAUSE("[s]") -> ?FIX_DEFAULTS,
	parse(R, TN, ?ADD_CF(elapsed_seconds));

% hour
?PARSE_CLAUSE("hh") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, time), ?ADD_CF(hours_zero));
?PARSE_CLAUSE("h") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, time), ?ADD_CF(hours));

% minutes/months
?PARSE_CLAUSE("mm") ->
	case fmt_is_hour(PC) of
		true -> parse(R, with_type(T, time), [minutes_zero|F], PC);
		false -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(ambiguous_mm))
	end;
?PARSE_CLAUSE("m") ->
	case fmt_is_hour(PC) of
		true -> parse(R, with_type(T, time), [minutes|F], PC);
		false -> ?FIX_DEFAULTS, parse(R, TN, ?ADD_CF(ambiguous_m))
	end;

% second
?PARSE_CLAUSE("ss") -> ?FIX_SEC,
	parse(R, with_type(TN, time), ?ADD_CF(seconds_zero));
?PARSE_CLAUSE("s") -> ?FIX_SEC,
	parse(R, with_type(TN, time), ?ADD_CF(seconds));

% misc operators
?PARSE_CLAUSE("*") -> ?FIX_DEFAULTS,
	parse(R, TN, ?ADD_CF(repeat)); % char is parsed separately
?PARSE_CLAUSE("@") -> ?FIX_DEFAULTS,
	parse(R, with_type(TN, text), ?ADD_CF(text));
parse(<<"_", _/utf8, R/binary>>, T, F, PC) ->
	parse(R, T, [{str," "}|F], PC);

% strings
parse(<<"\\", C/utf8, R/binary>>, T, F, PC) ->
	parse(R, T, [{str, [C]}|F], PC);
?PARSE_CLAUSE("\"") ->
	{S, R2} = parse_read_string(R),
	parse(R2, T, [{str, S}|F], PC);

% instructions
?PARSE_CLAUSE("[") -> ?FIX_DEFAULTS,
	{I, R2} = parse_read_instruction(R),
	parse(R2, TN, ?ADD_CF(I));

parse(<<C/utf8, R/binary>>, T, F, PC) ->
	parse(R, T, [{str, [C]}|F], PC).

parse_read_string(Bin) -> parse_read_string(Bin, <<>>).
parse_read_string(<<"\\", C/utf8, R/binary>>, Acc) -> parse_read_string(R, <<Acc/binary, "\\"/utf8, C/utf8>>);
parse_read_string(<<"\"", R/binary>>, Acc) -> {unicode:characters_to_list(Acc), R};
parse_read_string(<<C/utf8, R/binary>>, Acc) -> parse_read_string(R, <<Acc/binary, C/utf8>>).

parse_read_instruction(Bin) -> parse_read_instruction(Bin, []).
parse_read_instruction(<<"Black", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#000000}|Acc]);
parse_read_instruction(<<"Blue", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#0000FF}|Acc]);
parse_read_instruction(<<"Cyan", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#00FFFF}|Acc]);
parse_read_instruction(<<"Green", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#00FF00}|Acc]);
parse_read_instruction(<<"Magenta", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#FF00FF}|Acc]);
parse_read_instruction(<<"Red", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#FF0000}|Acc]);
parse_read_instruction(<<"White", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#FFFFFF}|Acc]);
parse_read_instruction(<<"Yellow", R/binary>>, Acc) ->
	parse_read_instruction(R, [{color,16#FFFF00}|Acc]);
parse_read_instruction(<<"Color", R/binary>>, Acc) ->
	parse_read_insn_color(R, Acc);
parse_read_instruction(<<"<>", R/binary>>, Acc) ->
	{N, R2} = parse_read_condition_value(R),
	parse_read_instruction(R2, [{condition,{not_equal,N}}|Acc]);
parse_read_instruction(<<"<=", R/binary>>, Acc) ->
	{N, R2} = parse_read_condition_value(R),
	parse_read_instruction(R2, [{condition,{less_or_equal,N}}|Acc]);
parse_read_instruction(<<">=", R/binary>>, Acc) ->
	{N, R2} = parse_read_condition_value(R),
	parse_read_instruction(R2, [{condition,{greater_or_equal,N}}|Acc]);
parse_read_instruction(<<"<", R/binary>>, Acc) ->
	{N, R2} = parse_read_condition_value(R),
	parse_read_instruction(R2, [{condition,{less,N}}|Acc]);
parse_read_instruction(<<">", R/binary>>, Acc) ->
	{N, R2} = parse_read_condition_value(R),
	parse_read_instruction(R2, [{condition,{greater,N}}|Acc]);
parse_read_instruction(<<"=", R/binary>>, Acc) ->
	{N, R2} = parse_read_condition_value(R),
	parse_read_instruction(R2, [{condition,{equal,N}}|Acc]);
parse_read_instruction(<<"]", R/binary>>, Acc) -> {Acc, R};
parse_read_instruction(<<C/utf8, R/binary>>, Acc) -> parse_read_instruction(R, <<Acc/binary, C/utf8>>).

parse_read_condition_value(Bin) ->
	parse_read_condition_value(Bin, 0).
parse_read_condition_value(<<C/utf8, R/binary>>, Acc) when C >= $0, C =< $9 ->
	parse_read_condition_value(R, Acc * 10 + (C - $0));
parse_read_condition_value(<<".", R/binary>>, Acc) ->
	parse_read_cv_decimal(R, Acc, []);
parse_read_condition_value(<<C/utf8, R/binary>>, Acc) ->
	{Acc, <<C/utf8, R/binary>>}.

parse_read_cv_decimal(<<C/utf8, R/binary>>, I, Acc) when C >= $0, C =< $9 ->
	parse_read_cv_decimal(R, I, [(C - $0)|Acc]);
parse_read_cv_decimal(<<C/utf8, R/binary>>, I, Acc) ->
	{I + lists:foldl(fun(N, A) -> (A + N) / 10 end, 0, Acc),
		<<C/utf8, R/binary>>}.

get_color_index(I) ->
	case I of
		1  -> 16#000000; 2  -> 16#FFFFFF; 3  -> 16#FF0000; 4  -> 16#00FF00;
		5  -> 16#0000FF; 6  -> 16#FFFF00; 7  -> 16#FF00FF; 8  -> 16#00FFFF;
		9  -> 16#800000; 10 -> 16#008000; 11 -> 16#000080; 12 -> 16#808000;
		13 -> 16#800080; 14 -> 16#008080; 15 -> 16#C0C0C0; 16 -> 16#808080;
		17 -> 16#9999FF; 18 -> 16#993366; 19 -> 16#FFFFCC; 20 -> 16#CCFFFF;
		21 -> 16#660066; 22 -> 16#FF8080; 23 -> 16#0066CC; 24 -> 16#CCCCFF;
		25 -> 16#000080; 26 -> 16#FF00FF; 27 -> 16#FFFF00; 28 -> 16#00FFFF;
		29 -> 16#800080; 30 -> 16#800000; 31 -> 16#008080; 32 -> 16#0000FF;
		33 -> 16#00CCFF; 34 -> 16#CCFFFF; 35 -> 16#CCFFCC; 36 -> 16#FFFF99;
		37 -> 16#99CCFF; 38 -> 16#FF99CC; 39 -> 16#CC99FF; 40 -> 16#FFCC99;
		41 -> 16#3366FF; 42 -> 16#33CCCC; 43 -> 16#99CC00; 44 -> 16#FFCC00;
		45 -> 16#FF9900; 46 -> 16#FF6600; 47 -> 16#666699; 48 -> 16#969696;
		49 -> 16#003366; 50 -> 16#339966; 51 -> 16#003300; 52 -> 16#333300;
		53 -> 16#993300; 54 -> 16#993366; 55 -> 16#333399; 56 -> 16#333333;
		_ -> invalid
	end.

parse_read_insn_color(B, Acc) ->
	parse_read_insn_color(B, Acc, 0).
parse_read_insn_color(<<C/utf8, R/binary>>, Acc, CI) when C >= $0, C =< $9 ->
	parse_read_insn_color(R, Acc, CI * 10 + (C - $0));
parse_read_insn_color(<<C/utf8, R/binary>>, Acc, CI) ->
	parse_read_instruction(<<C/utf8, R/binary>>, [{color,get_color_index(CI)}|Acc]).

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
