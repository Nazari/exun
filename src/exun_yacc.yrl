Terminals  '(' ')' '/' '*' '^' '+' '-' '[' ']' word number.
Nonterminals expr uexpr variable signed_number.
Rootsymbol expr.
Right 900 '^'.
Left 800 '['.
Left 750 '*' '/'.
Left 650 '+' '-'.
Unary 0 variable.

expr -> '(' expr ')' : '$2'.

expr -> uexpr '^' uexpr : {elev, '$1', '$3' }.
expr -> uexpr '/' uexpr : {divi, '$1', '$3'}.
expr -> uexpr '*' uexpr : {mult, '$1', '$3'}.
expr -> uexpr '+' uexpr : {suma, '$1', '$3'}.
expr -> uexpr '-' uexpr : {rest, '$1', '$3'}.

expr -> expr '^' expr : {elev, '$1', '$3'}.
expr -> expr '/' expr : {divi, '$1', '$3'}.
expr -> expr '*' expr : {mult, '$1', '$3'}.
expr -> expr '+' expr : {suma, '$1', '$3'}.
expr -> expr '-' expr : {rest, '$1', '$3'}.


expr -> number : {numb, extract_token('$1')}.
expr -> signed_number : {numb, '$1'}.
expr -> variable : {vari, '$1'}.

signed_number -> '+' number : extract_token('$2').
signed_number -> '-' number : -extract_token('$2').

variable -> word : extract_token('$1').

uexpr -> expr '[' expr ']' : {unit, '$1', '$3'}.
expr -> uexpr : '$1'.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
