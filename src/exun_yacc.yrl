Terminals  ',' '(' ')' '/' '*' '^' '+' '-' '[' ']' '\'' '=' '$' word number '{' '}'.
Nonterminals main expr mmm msm uexpr vector vectors matrix variable function arg_list arguments signed_number.
Rootsymbol main.
Right 950 '$'.
Left 950 '\''.
Right 900 '^'.
Left 800 '['.
Left 750 '*' '/'.
Left 650 '+' '-'.
Unary 0 variable.

main -> expr : '$1' .
main -> expr '=' expr : {equal, '$1', '$3'}.

expr -> '(' expr ')' : '$2'.

expr -> expr '^' expr : {elev, '$1', '$3'}.

mmm -> expr '*' expr : ['$1' , '$3'].
mmm -> expr '/' expr : ['$1' , {elev,'$3',{numb, -1}}].
mmm ->  mmm '*' expr : ['$3' | '$1'].
mmm ->  mmm '/' expr : [{elev,'$3',{numb, -1}} | '$1'].

msm -> expr '+' expr : ['$1' , '$3'].
msm -> expr '-' expr : ['$1' , {minus, '$3'}].
msm -> msm  '+' expr : ['$3' | '$1'].
msm -> msm  '-' expr : [{minus, '$3'} | '$1'].

expr -> mmm : {{m, mult}, lists:sort('$1')}.
expr -> msm : {{m, suma}, lists:sort('$1')}.

expr -> number : {numb, extract_token('$1')}.
expr -> signed_number : {numb, '$1'}.
expr -> variable : {vari, '$1' }.
expr -> function : '$1' .

signed_number -> '+' number : extract_token('$2').
signed_number -> '-' number : -extract_token('$2').

variable -> word : extract_token('$1').

matrix -> '{' vectors '}' : {{raw, length('$2'), vlen(lists:nth(1,'$2'))}, lists:reverse('$2'),[],[]}.
matrix -> '{' vector '}' : {{raw, 1, vlen('$2')}, ['$2'],[],[]}.
vectors -> vector ',' vector : ['$3','$1'].
vectors -> vectors ',' vector : ['$3'|'$1'].
vector -> '{' arguments '}' : {{vector, length('$2')}, lists:reverse('$2')}.

function -> variable arg_list :  {fcall, '$1', '$2'}.
arg_list -> '(' ')' : [] .
arg_list -> '(' arguments ')' : lists:reverse('$2').
arguments -> expr : ['$1'] .
arguments -> arguments ',' expr : ['$3'|'$1'] .

uexpr -> expr '[' expr ']' : {unit, '$1', '$3'}.
expr -> '-' expr : {minus, '$2'}.
expr -> expr '\'' variable : {deriv, '$1', {vari, '$3'}}.
expr -> '$' expr ',' variable : {integ, '$2', {vari, '$4'}}.
expr -> uexpr : '$1'.
expr -> matrix : '$1'.
expr -> vector : '$1'.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
vlen(Value) -> length(element(2,Value)).
