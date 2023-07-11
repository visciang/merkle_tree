defmodule MerkleTreeTest do
  use ExUnit.Case

  test "new empty tree" do
    assert MerkleTree.new()
  end

  test "new tree from enumerable" do
    assert MerkleTree.new(k1: 1, k2: 2)
  end

  test "tree size" do
    assert MerkleTree.new() |> MerkleTree.size() == 0
    assert [k1: 1, k2: 2] |> MerkleTree.new() |> MerkleTree.size() == 2
    assert [k1: 1, k1: 1, k2: 2] |> MerkleTree.new() |> MerkleTree.size() == 2
  end

  test "keys" do
    assert MerkleTree.new() |> MerkleTree.keys() == []
    assert [k1: 1, k2: 2] |> MerkleTree.new() |> MerkleTree.keys() |> Enum.sort() == [:k1, :k2]
    assert [k1: 1, k1: "duplicate"] |> MerkleTree.new() |> MerkleTree.keys() == [:k1]
  end

  test "merkle_equal?" do
    assert MerkleTree.merkle_equal?(MerkleTree.new(), MerkleTree.new())

    keys = 1..10
    data = for k <- keys, do: {k, k}
    mt1 = MerkleTree.new(data)
    mt2 = MerkleTree.new(Enum.shuffle(data))
    assert MerkleTree.merkle_equal?(mt1, mt2)

    refute MerkleTree.merkle_equal?(mt1, MerkleTree.put(mt2, :extra, 1))
    refute MerkleTree.merkle_equal?(mt1, MerkleTree.delete(mt2, Enum.random(keys)))
  end

  test "equal?" do
    assert MerkleTree.equal?(MerkleTree.new(), MerkleTree.new())
    refute MerkleTree.equal?(MerkleTree.new(k1: 1), MerkleTree.new())

    # These merkle tree are reported as merkle_equal?
    # because of an hash collision (in env test we run with a 6 bit hash):
    # <<52::6>> = hash(%{k1: 1}) = hash(%{k1: 2})
    mt1 = MerkleTree.new(k1: 1)
    mt2 = MerkleTree.new(k1: 2)
    # different
    assert Enum.to_list(mt1) != Enum.to_list(mt2)
    # but reported as "probably equal"
    assert MerkleTree.merkle_equal?(mt1, mt2)
    # and equal? correctly reports them as different
    refute MerkleTree.equal?(mt1, mt2)

    # collision also between %{r1: 1} and %{r1: 39}
    mt1 = MerkleTree.new(z: 0, k1: 1, r1: 1)
    mt2 = MerkleTree.new(z: 0, k1: 2, r1: 39)
    assert MerkleTree.merkle_equal?(mt1, mt2)
    refute MerkleTree.equal?(mt1, mt2)
  end

  test "empty tree not has_key?" do
    refute MerkleTree.new() |> MerkleTree.has_key?(:k)
  end

  test "put => has_key?" do
    assert MerkleTree.new() |> MerkleTree.put(:k1, 1) |> MerkleTree.has_key?(:k1)

    mt = MerkleTree.new() |> MerkleTree.put(:k1, 1) |> MerkleTree.put(:k2, 2)
    assert MerkleTree.has_key?(mt, :k1)
    assert MerkleTree.has_key?(mt, :k2)
  end

  test "delete => not has_key?" do
    refute MerkleTree.new(k1: 1, k2: 2) |> MerkleTree.delete(:k1) |> MerkleTree.has_key?(:k1)
  end

  test "delete non existing keys" do
    mt = MerkleTree.new(k1: 1)

    for k <- 1..100 do
      assert MerkleTree.delete(mt, k) == mt
    end
  end

  test "fill all tree levels than empty the tree" do
    conflicting_leaf_keys = 0
    keys = 1..(2 ** Application.fetch_env!(:merkle_tree, :tree_levels) + conflicting_leaf_keys)

    data = for k <- keys, do: {k, k}
    mt = MerkleTree.new(data)

    delete_keys = Enum.shuffle(keys)

    mt =
      Enum.reduce(delete_keys, mt, fn key, mt ->
        MerkleTree.delete(mt, key)
      end)

    assert MerkleTree.size(mt) == 0
    assert MerkleTree.merkle_equal?(mt, MerkleTree.new())
  end

  test "fetch" do
    assert :error = MerkleTree.new() |> MerkleTree.fetch(:k)

    keys = 1..100
    data = for k <- keys, do: {k, to_string(k)}
    mt = MerkleTree.new(data)
    k = Enum.random(keys)
    v = to_string(k)
    assert {:ok, ^v} = MerkleTree.fetch(mt, k)
    assert :error = MerkleTree.fetch(mt, :not_present_key)
  end

  test "auto_update_hash opt" do
    keys = 1..100
    data = for k <- keys, do: {k, k}
    mt1 = MerkleTree.new(data)
    mt2 = MerkleTree.new(Enum.shuffle(data), auto_update_hash: false)

    assert_raise ArgumentError, fn ->
      assert MerkleTree.merkle_equal?(mt1, mt2)
    end

    mt2 = MerkleTree.update_hashes(mt2)
    assert MerkleTree.merkle_equal?(mt1, mt2)
    assert mt1.tree == mt2.tree
  end

  test "update_hashes should be called after a tree modification" do
    data = [k1: 1]
    mt1 = MerkleTree.new(data)
    mt2 = MerkleTree.new(data, auto_update_hash: false) |> MerkleTree.update_hashes()

    assert MerkleTree.merkle_equal?(mt1, mt2)

    mt2 = MerkleTree.put(mt2, :k4, 99_999)

    assert_raise ArgumentError, fn ->
      assert MerkleTree.merkle_equal?(mt1, mt2)
    end

    mt2 = MerkleTree.update_hashes(mt2)
    refute MerkleTree.merkle_equal?(mt1, mt2)
  end

  test "merkle_diff" do
    mt1 = MerkleTree.new()
    mt2 = MerkleTree.new()
    assert MerkleTree.merkle_diff(mt1, mt2) == []

    mt1 = MerkleTree.new(a: 1)
    mt2 = MerkleTree.new(a: 1)
    assert MerkleTree.merkle_diff(mt1, mt2) == []

    mt1 = MerkleTree.new()
    mt2 = MerkleTree.new(a: 1)
    assert MerkleTree.merkle_diff(mt1, mt2) == [:a]

    mt1 = MerkleTree.new(a: 1)
    mt2 = MerkleTree.new(a: 2)
    assert MerkleTree.merkle_diff(mt1, mt2) == [:a]

    mt1 = MerkleTree.new(a: 1, b: 0, d: 0)
    mt2 = MerkleTree.new(a: 2, c: 0)
    assert Enum.sort(MerkleTree.merkle_diff(mt1, mt2)) == [:a, :b, :c, :d]
  end

  test "enumerable" do
    mt = MerkleTree.new()
    assert Map.new(mt) == %{}
    # credo:disable-for-next-line Credo.Check.Warning.ExpensiveEmptyEnumCheck
    assert Enum.count(mt) == 0
    refute Enum.member?(mt, 1)

    keys = 1..100
    data = Map.new(keys, fn key -> {key, to_string(key)} end)
    mt = MerkleTree.new(data)

    assert Map.new(mt) == data
    assert Enum.count(mt) == 100
    assert Enum.member?(mt, 50)
    refute Enum.member?(mt, 0)

    assert MerkleTree.new(data) |> Enum.take(10)
    assert Enum.take_while(mt, fn {k, _v} -> k <= 100 end) |> Map.new() == data

    z_1_99 = Enum.zip(mt, 1..99)
    z_1_99_set = MapSet.new(z_1_99, fn {{k, v}, _idx} -> {k, v} end)
    assert MapSet.size(z_1_99_set) == 99
    assert MapSet.subset?(z_1_99_set, MapSet.new(data))

    assert Enum.slice(mt, 10..20)
  end

  test "collectable" do
    data = [a: 1, b: 2]
    assert Enum.into(data, MerkleTree.new()) == MerkleTree.new(data)
  end

  # test "merkle_diff_keys maps" do
  #   m1 = MerkleTree.new(%{foo: "bar", food: "good"}) |> MerkleTree.update_hashes()

  #   m2 =
  #     MerkleTree.new(%{foo: "baz", food: "good", drink: "also good"})
  #     |> MerkleTree.update_hashes()

  #   assert Enum.sort([:foo, :drink]) == Enum.sort(MerkleTree.diff_keys(m1, m2))
  # end

  # test "show diff_keys" do
  #   tree_one = MerkleTree.put(MerkleTree.new(), "foo", "bar") |> MerkleTree.update_hashes()
  #   assert [] = MerkleTree.diff_keys(tree_one, tree_one)
  #   tree_two = MerkleTree.put(MerkleTree.new(), "foo", "baz") |> MerkleTree.update_hashes()
  #   assert ["foo"] = MerkleTree.diff_keys(tree_one, tree_two)
  #   tree_three = MerkleTree.put(tree_one, "bar", "baz") |> MerkleTree.update_hashes()

  #   assert ["bar"] = MerkleTree.diff_keys(tree_one, tree_three)
  #   diff_keys = MerkleTree.diff_keys(tree_two, tree_three)
  #   assert Enum.sort(["foo", "bar"]) == Enum.sort(diff_keys)
  # end

  # test "subtree computes a sub tree" do
  #   subtree =
  #     Map.new(1..10000, fn x -> {x, x} end)
  #     |> MerkleTree.new()
  #     |> MerkleTree.update_hashes()
  #     |> MerkleTree.subtree("", 4)

  #   assert 4 = MerkleTree.max_depth(subtree)
  # end

  # property "diff_keys of itself is always empty" do
  #   check all key <- term(),
  #             value <- term() do
  #     map = %{key => value}
  #     tree = MerkleTree.new(map) |> MerkleTree.update_hashes()
  #     assert [] = MerkleTree.diff_keys(tree, tree)
  #   end
  # end

  # property "diff_keys identifies missing key" do
  #   check all key <- term(),
  #             value <- term() do
  #     map = %{key => value}
  #     tree = MerkleTree.new(map) |> MerkleTree.update_hashes()
  #     assert [_key] = MerkleTree.diff_keys(MerkleTree.new() |> MerkleTree.update_hashes(), tree)
  #   end
  # end

  # property "arbitrarily large map can still find one changed key" do
  #   tree =
  #     Map.new(1..1000, fn x -> {x, x * x} end) |> MerkleTree.new() |> MerkleTree.update_hashes()

  #   check all key <- term(),
  #             value <- term() do
  #     new_tree = MerkleTree.put(tree, key, value) |> MerkleTree.update_hashes()
  #     assert [_key] = MerkleTree.diff_keys(new_tree, tree)
  #   end
  # end
end
