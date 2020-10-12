defmodule Fun do

  @known %{
    "sin(x)" => { &:math.sin/1 , "-cos(x)" }
  }
end
