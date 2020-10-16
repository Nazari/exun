import Exun

expression = "(g(a^b,b^a)/g(b^a,a^b))'a"
context = %{"g(x,y)" => "(x^y/ln(sinh(y^x))+y^tanh(x)/cos(x*y))'x'y'x"}

Benchee.run(%{
  "parallel" => fn -> eval(expression, context) end
})
