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

    keys1 = 1..10
    keys2 = 10..1
    data1 = for k <- keys1, do: {k, k}
    data2 = for k <- keys2, do: {k, k}
    mt1 = MerkleTree.new(data1)
    mt2 = MerkleTree.new(data2)
    assert MerkleTree.merkle_equal?(mt1, mt2)

    refute MerkleTree.merkle_equal?(mt1, MerkleTree.put(mt2, :extra, 1))
    refute MerkleTree.merkle_equal?(mt1, MerkleTree.delete(mt2, 1))
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
    mt1 = MerkleTree.new(k11: 1, k1: 1, r1: 1)
    mt2 = MerkleTree.new(k11: 1, k1: 2, r1: 39)
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

  test "first / last" do
    assert MerkleTree.new() |> MerkleTree.first() == nil
    assert MerkleTree.new() |> MerkleTree.last() == nil

    assert MerkleTree.new(a: 1) |> MerkleTree.first() == {:a, 1}
    assert MerkleTree.new(a: 1) |> MerkleTree.last() == {:a, 1}

    mt = MerkleTree.new(Stream.zip(1..100, 1..100))
    assert mt |> Enum.to_list() |> List.first() == MerkleTree.first(mt)
    assert mt |> Enum.to_list() |> List.last() == MerkleTree.last(mt)
  end

  test "iterator" do
    assert MerkleTree.new() |> MerkleTree.iterator() |> MerkleTree.next() == nil

    iter = [a: 1] |> MerkleTree.new() |> MerkleTree.iterator()
    assert {{:a, 1}, iter} = MerkleTree.next(iter)
    assert MerkleTree.next(iter) == nil

    data = Stream.zip(1..100, 1..100) |> Map.new()
    mt = MerkleTree.new(data)
    iter = MerkleTree.iterator(mt)
    map = iter_to_map(MerkleTree.next(iter), %{})
    assert map == data
  end

  defp iter_to_map(nil, acc), do: acc

  defp iter_to_map({{k, v}, iterator}, acc) do
    acc = Map.put(acc, k, v)
    iter_to_map(MerkleTree.next(iterator), acc)
  end

  test "auto_comp_hash opt" do
    keys = 1..100
    data = for k <- keys, do: {k, k}
    mt1 = MerkleTree.new(data)
    mt2 = MerkleTree.new(Enum.shuffle(data), auto_comp_hash: false)

    assert_raise ArgumentError, fn ->
      assert MerkleTree.merkle_equal?(mt1, mt2)
    end

    mt2 = MerkleTree.compute_hashes(mt2)
    assert MerkleTree.merkle_equal?(mt1, mt2)
    assert mt1.tree == mt2.tree
  end

  test "compute_hashes should be called after a tree modification" do
    data = [k1: 1]
    mt1 = MerkleTree.new(data)
    mt2 = MerkleTree.new(data, auto_comp_hash: false) |> MerkleTree.compute_hashes()

    assert MerkleTree.merkle_equal?(mt1, mt2)

    mt2 = MerkleTree.put(mt2, :k4, 99_999)

    assert_raise ArgumentError, fn ->
      assert MerkleTree.merkle_equal?(mt1, mt2)
    end

    mt2 = MerkleTree.compute_hashes(mt2)
    refute MerkleTree.merkle_equal?(mt1, mt2)
  end

  test "merkle_diff / diff" do
    mt1 = MerkleTree.new()
    mt2 = MerkleTree.new()
    assert MerkleTree.merkle_diff(mt1, mt2) == []
    assert MerkleTree.diff(mt1, mt2) == []

    mt1 = MerkleTree.new(a: 1)
    mt2 = MerkleTree.new(a: 1)
    assert MerkleTree.merkle_diff(mt1, mt2) == []
    assert MerkleTree.diff(mt1, mt2) == []

    mt1 = MerkleTree.new()
    mt2 = MerkleTree.new(a: 1)
    assert MerkleTree.merkle_diff(mt1, mt2) == [{:add, :a}]
    assert MerkleTree.diff(mt1, mt2) == [{:add, :a}]

    mt1 = MerkleTree.new(a: 1)
    mt2 = MerkleTree.new(a: 2)
    assert MerkleTree.merkle_diff(mt1, mt2) == [{:upd, :a}]
    assert MerkleTree.diff(mt1, mt2) == [{:upd, :a}]

    mt1 = MerkleTree.new(a: 1, b: 0, d: 0)
    mt2 = MerkleTree.new(a: 2, c: 0)

    expected = [{:add, :c}, {:del, :b}, {:del, :d}, {:upd, :a}]
    assert Enum.sort(MerkleTree.merkle_diff(mt1, mt2)) == expected
    assert Enum.sort(MerkleTree.diff(mt1, mt2)) == expected

    mt1 = MerkleTree.new(Stream.zip(1..10_000, 1..10_000))
    mt2 = mt1 |> MerkleTree.delete(2_000) |> MerkleTree.put(9_000, 0) |> MerkleTree.put(0, 0)

    expected = [{:add, 0}, {:del, 2_000}, {:upd, 9_000}]
    assert Enum.sort(MerkleTree.merkle_diff(mt1, mt2)) == expected
    assert Enum.sort(MerkleTree.diff(mt1, mt2)) == expected
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

  test "inspect" do
    assert inspect(MerkleTree.new(a: 1, b: 2)) ==
             "MerkleTree.new([a: 1, b: 2], [auto_comp_hash: true])"
  end
end
