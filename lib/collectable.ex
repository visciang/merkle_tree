defimpl Collectable, for: MerkleTree do
  def into(%MerkleTree{} = mt) do
    collector_fun = fn
      mt_acc, {:cont, {k, v}} ->
        MerkleTree.put(mt_acc, k, v)

      mt_acc, :done ->
        mt_acc

      # coveralls-ignore-start
      _mt_acc, :halt ->
        :ok
        # coveralls-ignore-stop
    end

    initial_acc = mt

    {initial_acc, collector_fun}
  end
end
