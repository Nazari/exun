defimpl String.Chars, for: Exun do
  def to_string(%Exun{ast: val}) do
    Exun.UI.tostr(val)
  end
end

defimpl Inspect, for: Exun do
  def inspect(%Exun{ast: val, pc: map}, _opts) do
    tctx = if map != nil,
      do: for({k, v} <- map, into: "", do: "#{Exun.UI.tostr(k)}=#{Exun.UI.tostr(v)}\n"),
      else: ""

    "#Exun #{tctx} #{Exun.UI.tostr(val)}"
  end
end
