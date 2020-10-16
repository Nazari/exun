Terminals  ',' '(' ')' '/' '*' '^' '+' '-' '[' ']' '\'' '=' '$' word number.
Nonterminals main expr uexpr variable function arg_list arguments signed_number.
Rootsymbol main.
Right 950 '$'.
Right 900 '^'.
Left 800 '['.
Left 750 '*' '/'.
Left 650 '+' '-'.
Unary 0 variable.

main -> expr : '$1' .
main -> expr '=' expr : {equal, '$1', '$3' }.

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
expr -> '$' expr ',' variable : {integ, '$2', {vari, '$4'}}.
expr -> uexpr : '$1'.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
