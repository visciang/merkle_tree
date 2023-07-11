Benchee.run(
  %{
    "has_key? - existing" => fn input -> true = MerkleTree.has_key?(input, 1) end,
    "has_key? - non existing" => fn input -> false = MerkleTree.has_key?(input, 0) end,
    "put - new" => fn input -> MerkleTree.put(input, 0, 0) end,
    "put - update" => fn input -> MerkleTree.put(input, 0, 0) end,
    "delete - existing" => fn input -> MerkleTree.delete(input, 1) end,
    "delete - non existing" => fn input -> MerkleTree.delete(input, 0) end,
    "fetch - existing" => fn input -> {:ok, 1} = MerkleTree.fetch(input, 1) end,
    "fetch - non existing" => fn input -> :error = MerkleTree.fetch(input, 0) end,
    "keys" => fn input -> MerkleTree.keys(input) end
  },
  inputs: %{
    "Tree size 100" => MerkleTree.new(Stream.zip(1..100, 1..100)),
    "Tree size 100_000" => MerkleTree.new(Stream.zip(1..100_000, 1..100_000)),
    "Tree size 1_000_000" => MerkleTree.new(Stream.zip(1..1_000_000, 1..1_000_000))
  },
  warmup: 0.5,
  time: 2
)
