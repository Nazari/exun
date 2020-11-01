Definitions.

FLOAT_END = [0-9]+\.[0-9]*
FLOAT_START = [0-9]*\.[0-9]+
INTEGER = [0-9]+
P_OPEN = \(
P_CLOSE = \)
C_OPEN = \[
C_CLOSE = \]
WORD = [a-zA-Z][a-zA-Z0-9]*

PLUS = \+
MINUS = \-
DIVIDE = \/
MULTIPLY = \*
POWER = \^
DOT = \.
COMMA = \,
DERIV =\'
EQUAL =\=
INTEGRAL =\$
OPEN_MAT = \{
CLOSE_MAT = \}

SIGN = (\-|\+)

WHITESPACE = [\s\t\n\r]

STRING = "([^\\"]+|\\"|\\)*"

Rules.
% reserved words appear up here so they're not eaten by {WORD}

{INTEGER}    : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOAT_END}      : {token, {number, TokenLine, list_to_float(TokenChars)}}.
{FLOAT_START}      : {token, {number, TokenLine, list_to_float([48 | TokenChars])}}.
{P_OPEN}    : {token, {'(', TokenLine}}.
{P_CLOSE}   : {token, {')', TokenLine}}.
{C_OPEN}    : {token, {'[', TokenLine}}.
{C_CLOSE}   : {token, {']', TokenLine}}.
{PLUS}   : {token, {'+', TokenLine}}.
{MINUS}  : {token, {'-', TokenLine}}.
{DIVIDE} : {token, {'/', TokenLine}}.
{MULTIPLY} : {token, {'*', TokenLine}}.
{POWER} : {token, {'^', TokenLine}}.
{STRING} : {token, {string, TokenLine, parse_string(TokenChars)}}.
{WORD}  : {token, {word, TokenLine, list_to_binary(TokenChars)}}.
{DOT} : {token, {'.', TokenLine}}.
{COMMA} : {token, {',', TokenLine}}.
{EQUAL} : {token, {'=', TokenLine}}.
{DERIV} : {token, {'\'', TokenLine}}.
{INTEGRAL} : {token, {'$', TokenLine}}.
{OPEN_MAT} : {token, {'{', TokenLine}}.
{CLOSE_MAT} : {token, {'}', TokenLine}}.


{WHITESPACE}+ : skip_token.

Erlang code.
parse_string(Chars) -> list_to_binary(string:replace(string:replace('Elixir.String':slice(list_to_binary(Chars), 1, length(Chars) - 2), <<"\\\"">>, <<"\"">>, all), <<"\\\\">>, <<"\\">>, all)).