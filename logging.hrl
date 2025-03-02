% logging.hrl - logging macros
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

-define(LOG_INFO(Msg, Args),
	io:format("~s: " ++ Msg ++ "~n", [?MODULE|Args])).
-define(LOG_INFO(Msg), ?LOG_INFO(Msg, [])).

-define(LOG_ERROR(Msg, Args),
	io:format(standard_error, "~s: " ++ Msg ++ "~n", [?MODULE|Args])).
-define(LOG_ERROR(Msg), ?LOG_ERROR(Msg, [])).
