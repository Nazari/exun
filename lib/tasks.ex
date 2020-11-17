defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    ast =
      {{:m, :suma},
       [
         {:minus,
          {{:m, :mult},
           [
             {:numb, 1, 2},
             {{:m, :suma},
              [
                {:numb, 1, 1},
                {{:m, :mult},
                 [
                   {{:m, :suma}, [{:numb, 1, 1}, {:vari, "x"}]},
                   {:minus,
                    {:elev, {:minus, {{:m, :suma}, [{:numb, -1, 1}, {:vari, "x"}]}},
                     {:numb, -1, 1}}}
                 ]}
              ]},
             {:elev, {{:m, :suma}, [{:numb, 1, 1}, {:vari, "x"}]}, {:numb, -1, 2}},
             {:elev, {{:m, :suma}, [{:numb, -1, 1}, {:vari, "x"}]}, {:numb, 1, 2}}
           ]}},
         {{:m, :mult},
          [
            {:elev, {{:m, :suma}, [{:numb, -1, 1}, {:elev, {:vari, "x"}, {:numb, 2, 1}}]},
             {:numb, -1, 2}},
            {:vari, "x"}
          ]}
       ]}

    {{:m,:suma},[a,b]} = ast

    IO.puts Exun.UI.tostr(a)
    IO.puts Exun.UI.tostr(b)
  end
end
