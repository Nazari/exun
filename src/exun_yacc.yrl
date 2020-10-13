Terminals  ',' '(' ')' '/' '*' '^' '+' '-' '[' ']' '\'' word number.
Nonterminals expr uexpr variable function arg_list arguments signed_number.
Rootsymbol expr.
Right 900 '^'.
Left 800 '['.
Left 750 '*' '/'.
Left 650 '+' '-'.
Unary 0 variable.

expr -> '(' expr ')' : '$2'.

expr -> expr '^' expr : {elev, '$1', '$3'}.
expr -> expr '/' expr : {divi, '$1', '$3'}.
expr -> expr '*' expr : {mult, '$1', '$3'}.
expr -> expr '+' expr : {suma, '$1', '$3'}.
expr -> expr '-' expr : {rest, '$1', '$3'}.


expr -> number : {numb, extract_token('$1')}.
expr -> signed_number : {numb, '$1'}.
expr -> variable : {vari, '$1' }.
expr -> function : '$1' .

signed_number -> '+' number : extract_token('$2').
signed_number -> '-' number : -extract_token('$2').

variable -> word : extract_token('$1').

function -> variable arg_list :  {fcall, '$1', '$2'}.
arg_list -> '(' ')' : [] .
arg_list -> '(' arguments ')' : lists:reverse('$2').
arguments -> expr : ['$1'] .
arguments -> arguments ',' expr : ['$3'|'$1'] .

uexpr -> expr '[' expr ']' : {unit, '$1', '$3'}.
expr -> '-' expr : {mult, {numb, -1}, '$2'}.
expr -> expr '\'' variable : {deriv, '$1', {vari, '$3'}}.
expr -> uexpr : '$1'.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
