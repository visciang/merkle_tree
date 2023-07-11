defmodule BenchUtils do
  def iter_all(nil), do: nil

  def iter_all({_item, iterator}) do
    iterator
    |> MerkleTree.next()
    |> iter_all()
  end
end

mt = MerkleTree.new(Stream.zip(1..100_000, 1..100_000))

Benchee.run(
  %{
    "iterator" => fn ->
      mt
      |> MerkleTree.iterator()
      |> MerkleTree.next()
      |> BenchUtils.iter_all()
    end,
    "enum" => fn ->
      Enum.each(mt, fn _ -> :ok end)
    end
  },
  warmup: 0.5,
  time: 2
)
