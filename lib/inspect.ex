defimpl Inspect, for: MerkleTree do
  import Inspect.Algebra

  def inspect(%MerkleTree{} = mt, opts) do
    concat([
      "MerkleTree.new(",
      Inspect.List.inspect(Enum.to_list(mt), opts),
      ", ",
      Inspect.List.inspect(mt.opts, opts),
      ")"
    ])
  end
end
