mt = MerkleTree.new(Stream.zip(1..1_000_000, 1..1_000_000))

mt_diff_1 =
  Enum.reduce(1..1, mt, fn idx, mt -> MerkleTree.put(mt, idx, nil) end)

mt_diff_10 =
  Enum.reduce(1..10, mt, fn idx, mt -> MerkleTree.put(mt, idx, nil) end)

mt_diff_1000 =
  Enum.reduce(1..1_000, mt, fn idx, mt -> MerkleTree.put(mt, idx, nil) end)

Benchee.run(
  %{
    "merkle_diff" => fn {mt1, mt2} -> MerkleTree.merkle_diff(mt1, mt2) end,
    "diff" => fn {mt1, mt2} -> MerkleTree.diff(mt1, mt2) end
  },
  inputs: %{
    "Tree size 100_000, 1 diff" => {mt, mt_diff_1},
    "Tree size 100_000, 10 diff" => {mt, mt_diff_10},
    "Tree size 100_000, 1_000 diff" => {mt, mt_diff_1000}
  }
)
