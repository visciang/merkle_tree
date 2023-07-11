defmodule MerkleTree do
  @moduledoc """
  (Sparse) Merkle Tree.

  `MerkleTree` implements `Enumerable` and `Collectable` protocols.
  """

  @enforce_keys [:opts, :tree, :size]
  defstruct @enforce_keys

  @typedoc "Merkle tree."
  @type t() :: %__MODULE__{
          opts: opts(),
          tree: tree(),
          size: non_neg_integer()
        }

  @typep undefined_hash :: :undefined_hash
  @undefined_hash :undefined_hash

  @typep empty_branch :: nil
  @empty_branch nil

  # coveralls-ignore-start
  @doc false
  defmacro empty_branch, do: @empty_branch
  # coveralls-ignore-stop

  @typep direction :: 0 | 1
  @left_branch 0
  @right_branch 1
  # coveralls-ignore-start
  @doc false
  defmacro left_branch, do: @left_branch
  @doc false
  defmacro right_branch, do: @right_branch
  # coveralls-ignore-stop

  @typedoc "A tree key."
  @type key :: term()
  @typedoc "A tree value."
  @type value :: term()

  @typep tree_level :: non_neg_integer()
  @typep hash :: undefined_hash() | bitstring()

  @typep leaf :: {k_hash :: hash(), leaf_map()}
  @typep leaf_map :: %{key() => value()}

  @typep tree :: branch()
  @typep branch :: leaf_node() | inner_node() | empty_branch()
  @typep leaf_node :: {hash(), leaf()}
  @typep inner_node :: {hash(), left :: branch(), right :: branch()}

  @typep deep_key_list :: [key() | deep_key_list()]

  @typedoc """
  `MerkleTree` options:
  - `auto_update_hash`: tree hashes are updated on every `put/3` / `delete/2`.
    If set to `false` hashes should be explicitly computed calling `update_hashes/1` before
    calling functions like `merkle_equal?/2`. Defaults to `true`.
  """
  @type opts :: [{:auto_update_hash, boolean()}]
  @default_opts [auto_update_hash: true]

  @tree_levels Application.compile_env(:merkle_tree, :tree_levels, 32)
  @tree_leaves 2 ** @tree_levels

  @doc """
  Create a new `t:MerkleTree.t/0` from an optional enumerable of key->value pairs.

  If `enumerable` is not provided it creates an empty tree.
  The `opts` optional parameter can be provided to customize the tree behaviour (refer to `t:opts/1`).
  """
  @spec new(Enumerable.t({key(), value()}) | nil, opts()) :: t()
  def new(enum \\ nil, opts \\ []) do
    opts = Keyword.merge(@default_opts, opts)
    empty_mt = %__MODULE__{tree: @empty_branch, opts: opts, size: 0}

    if enum == nil do
      empty_mt
    else
      Enum.reduce(enum, empty_mt, fn {key, value}, mt ->
        put(mt, key, value)
      end)
    end
  end

  @doc """
  Returns the number of keys stored in the tree.
  """
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{size: size}), do: size

  @doc """
  Returns whether the given key exists in the tree.
  """
  @spec has_key?(t(), key()) :: boolean()
  def has_key?(%__MODULE__{tree: tree}, key) do
    has_key?(tree, 0, hash(key), key)
  end

  @doc """
  Fetches a key value from the tree.
  """
  @spec fetch(t(), key()) :: {:ok, value()} | :error
  def fetch(%__MODULE__{tree: tree}, key) do
    fetch(tree, 0, hash(key), key)
  end

  @doc """
  Puts the given key/value in the tree.
  """
  @spec put(t(), key(), value()) :: t()
  def put(%__MODULE__{tree: tree, opts: opts, size: size}, key, value) do
    auto_update_hash = Keyword.fetch!(opts, :auto_update_hash)
    {tree, exists?} = put_leaf(tree, 0, {hash(key), %{key => value}}, auto_update_hash)

    %__MODULE__{
      tree: tree,
      opts: opts,
      size: if(exists?, do: size, else: size + 1)
    }
  end

  @doc """
  Deletes a tree key.
  """
  @spec delete(t(), key()) :: t()
  def delete(%__MODULE__{tree: tree, opts: opts, size: size}, key) do
    auto_update_hash = Keyword.fetch!(opts, :auto_update_hash)
    {tree, exists?} = delete(tree, 0, hash(key), key, auto_update_hash)

    %__MODULE__{
      tree: tree,
      opts: opts,
      size: if(exists?, do: size - 1, else: size)
    }
  end

  @doc """
  Returns all keys in the tree.
  """
  @spec keys(t()) :: [key()]
  def keys(%__MODULE__{tree: tree}) do
    tree_keys(tree) |> List.flatten()
  end

  @doc """
  Checks if two trees are (merkle) equal.

  The equality is checked leveraging the merkle tree hashes and hence
  in case of hash collisions a wrong result could be returned.

  More precisely if it reports that the trees are different than this is 100% true,
  otherwise it means the trees are "probably" equal.

  Raises if the hashes are not calculated (Refer to `:auto_update_hash` `t:opts/0`).

  See also `equal?/2`.
  """
  @spec merkle_equal?(t(), t()) :: boolean()
  def merkle_equal?(%__MODULE__{tree: tree1, opts: opts1}, %__MODULE__{tree: tree2, opts: opts2}) do
    unless opts1[:auto_update_hash], do: assert_hashes(tree1, "&MerkleTree.merkle_equal?/2")
    unless opts2[:auto_update_hash], do: assert_hashes(tree2, "&MerkleTree.merkle_equal?/2")

    tree_equal(tree1, tree2)
  end

  @doc """
  Checks if two trees are equal.

  The equality is first checked leveraging the merkle tree hashes and in case of
  uncertaintainty with a default comparison algorithm.

  Raises if the hashes are not calculated (Refer to `:auto_update_hash` `t:opts/0`).

  See also `merkle_equal?/2`.
  """
  @spec equal?(t(), t()) :: boolean()
  def equal?(
        %__MODULE__{tree: tree1, size: size1, opts: opts1} = mt1,
        %__MODULE__{tree: tree2, size: size2, opts: opts2} = mt2
      ) do
    unless opts1[:auto_update_hash], do: assert_hashes(tree1, "&MerkleTree.equal?/2")
    unless opts2[:auto_update_hash], do: assert_hashes(tree2, "&MerkleTree.equal?/2")

    if size1 != size2 or not merkle_equal?(mt1, mt2) do
      false
    else
      Stream.zip(mt1, mt2)
      |> Enum.reduce_while(true, fn
        {{k, v}, {k, v}}, true -> {:cont, true}
        _, true -> {:halt, false}
      end)
    end
  end

  @doc """
  Returns the keys that differ (changed / removed / added) between two trees.

  The diff is computed leveraging the merkle tree hashes and hence
  in case of hash collisions a wrong result could be returned.

  More preciselly the diff reported is correct but could be incomplete.

  Raises if the hashes are not calculated (Refer to `:auto_update_hash` `t:opts/0`).
  """
  @spec merkle_diff(t(), t()) :: [key()]
  def merkle_diff(%__MODULE__{tree: tree1, opts: opts1}, %__MODULE__{tree: tree2, opts: opts2}) do
    unless opts1[:auto_update_hash], do: assert_hashes(tree1, "&MerkleTree.merkle_diff/2")
    unless opts2[:auto_update_hash], do: assert_hashes(tree2, "&MerkleTree.merkle_diff/2")

    tree_diff(tree1, tree2) |> List.flatten() |> Enum.uniq()
  end

  @doc """
  Update the tree hashes.

  Refer to `:auto_update_hash` `t:opts/0`.
  """
  @spec update_hashes(t()) :: t()
  def update_hashes(%__MODULE__{tree: tree} = mt) do
    {_, tree} = calculate_hashes(tree)
    %__MODULE__{mt | tree: tree}
  end

  @doc false
  @spec leaf_direction(leaf() | hash(), tree_level()) :: direction()
  def leaf_direction({hash_key, _leaf_map}, tree_level) do
    leaf_direction(hash_key, tree_level)
  end

  def leaf_direction(hash, tree_level) do
    <<_::size(@tree_levels - 1 - tree_level), direction::1, _::bits>> = hash
    direction
  end

  @spec has_key?(branch(), tree_level(), hash(), key()) :: boolean()
  defp has_key?(@empty_branch, _tree_level, _hash_k, _key) do
    false
  end

  defp has_key?({_hash, branch_l, branch_r} = _inner_node, tree_level, hash_k, key) do
    case leaf_direction(hash_k, tree_level) do
      @left_branch -> has_key?(branch_l, tree_level + 1, hash_k, key)
      @right_branch -> has_key?(branch_r, tree_level + 1, hash_k, key)
    end
  end

  defp has_key?({_, {hash_k, leaf_map}} = _leaf, @tree_levels, hash_k, key) do
    Map.has_key?(leaf_map, key)
  end

  @spec fetch(branch(), tree_level(), hash(), key()) :: {:ok, value()} | :error
  defp fetch(@empty_branch, _tree_level, _hash_k, _key) do
    :error
  end

  defp fetch({_hash, branch_l, branch_r} = _inner_node, tree_level, hash_k, key) do
    case leaf_direction(hash_k, tree_level) do
      @left_branch -> fetch(branch_l, tree_level + 1, hash_k, key)
      @right_branch -> fetch(branch_r, tree_level + 1, hash_k, key)
    end
  end

  defp fetch({_, {hash_k, leaf_map}} = _leaf, @tree_levels, hash_k, key) do
    case leaf_map do
      %{^key => value} -> {:ok, value}
      _ -> :error
    end
  end

  @spec put_leaf(branch(), tree_level(), leaf(), update_hash? :: boolean()) ::
          {branch(), exists? :: boolean()}
  defp put_leaf(@empty_branch = _at_leaf_level, @tree_levels, leaf, update_hash?) do
    hash = leaf_hash(update_hash?, leaf)
    {{hash, leaf}, false}
  end

  defp put_leaf(@empty_branch = _at_non_leaf_level, tree_level, leaf, update_hash?) do
    {branch, _} =
      put_leaf({@undefined_hash, @empty_branch, @empty_branch}, tree_level, leaf, update_hash?)

    {branch, false}
  end

  defp put_leaf({_hash, branch_l, branch_r} = _inner_node, tree_level, leaf, update_hash?) do
    case leaf_direction(leaf, tree_level) do
      @left_branch ->
        {branch_l, exists?} = put_leaf(branch_l, tree_level + 1, leaf, update_hash?)
        hash = inner_node_hash(update_hash?, branch_l, branch_r)
        {{hash, branch_l, branch_r}, exists?}

      @right_branch ->
        {branch_r, exists?} = put_leaf(branch_r, tree_level + 1, leaf, update_hash?)
        hash = inner_node_hash(update_hash?, branch_l, branch_r)
        {{hash, branch_l, branch_r}, exists?}
    end
  end

  defp put_leaf(
         {_, {hash_k, leaf_map}} = _leaf,
         @tree_levels,
         {hash_k, new_leaf_map},
         update_hash?
       ) do
    [{key, _value}] = Map.to_list(new_leaf_map)
    exists? = Map.has_key?(leaf_map, key)
    leaf = {hash_k, Map.merge(leaf_map, new_leaf_map)}
    hash = leaf_hash(update_hash?, leaf)
    {{hash, leaf}, exists?}
  end

  @spec delete(branch(), tree_level(), hash(), key(), update_hash? :: boolean()) ::
          {branch(), exists? :: boolean()}
  defp delete(@empty_branch = _at_leaf_level, @tree_levels, _hash_key, _key, _update_hash?) do
    {@empty_branch, false}
  end

  defp delete(@empty_branch = _at_non_leaf_level, _tree_level, _hash_key, _key, _update_hash?) do
    {@empty_branch, false}
  end

  defp delete({_, {hash_k, leaf_map}} = _leaf, _tree_level, _hash, key, update_hash?) do
    exists? = Map.has_key?(leaf_map, key)
    leaf_map = Map.delete(leaf_map, key)

    if leaf_map == %{} do
      {@empty_branch, exists?}
    else
      leaf = {hash_k, leaf_map}
      hash = leaf_hash(update_hash?, leaf)
      {{hash, {hash_k, leaf_map}}, exists?}
    end
  end

  defp delete({_, branch_l, branch_r} = _inner_node, tree_level, hash_key, key, update_hash?) do
    case leaf_direction(hash_key, tree_level) do
      @left_branch ->
        {branch_l, exists?} = delete(branch_l, tree_level + 1, hash_key, key, update_hash?)
        {branch_l, branch_r, exists?}

      @right_branch ->
        {branch_r, exists?} = delete(branch_r, tree_level + 1, hash_key, key, update_hash?)
        {branch_l, branch_r, exists?}
    end
    |> case do
      {@empty_branch, @empty_branch, exists?} ->
        {@empty_branch, exists?}

      {branch_l, branch_r, exists?} ->
        hash = inner_node_hash(update_hash?, branch_l, branch_r)
        {{hash, branch_l, branch_r}, exists?}
    end
  end

  @spec tree_diff(branch(), branch()) :: deep_key_list()
  defp tree_diff(@empty_branch, @empty_branch), do: []
  defp tree_diff(@empty_branch, branch_2), do: tree_keys(branch_2)
  defp tree_diff(branch_1, @empty_branch), do: tree_keys(branch_1)

  defp tree_diff(
         {hash1, branch_l1, branch_r1} = _inner_node1,
         {hash2, branch_l2, branch_r2} = _inner_node2
       ) do
    if hash1 == hash2 do
      []
    else
      [
        tree_diff(branch_l1, branch_l2),
        tree_diff(branch_r1, branch_r2)
      ]
    end
  end

  defp tree_diff({hash1, {hash_k2, leaf_map1}} = _leaf1, {hash2, {hash_k2, leaf_map2}} = _leaf2) do
    if hash1 == hash2 do
      []
    else
      leaf_set1 = MapSet.new(leaf_map1)
      leaf_set2 = MapSet.new(leaf_map2)
      diff = MapSet.symmetric_difference(leaf_set1, leaf_set2)
      Enum.map(diff, fn {k, _v} -> k end)
    end
  end

  @spec tree_equal(branch(), branch()) :: boolean()
  defp tree_equal(@empty_branch, @empty_branch), do: true
  defp tree_equal({hash, _, _} = _inner_node1, {hash, _, _} = _inner_node2), do: true
  defp tree_equal(_, _), do: false

  @spec tree_keys(branch()) :: deep_key_list()
  defp tree_keys(@empty_branch), do: []
  defp tree_keys({_, {_, leaf_map}} = _leaf), do: Map.keys(leaf_map)

  defp tree_keys({_, branch_l, branch_r} = _inner_node) do
    [tree_keys(branch_l), tree_keys(branch_r)]
  end

  @spec assert_hashes(branch(), caller :: String.t()) :: nil | no_return()
  defp assert_hashes(branch, caller) when elem(branch, 0) == @undefined_hash do
    msg = "#{inspect(__MODULE__)}.update_hashes/1 should be called before calling #{caller}."
    raise ArgumentError, msg
  end

  defp assert_hashes(_, _) do
    nil
  end

  @spec calculate_hashes(branch()) :: {hash(), branch()}
  defp calculate_hashes(@empty_branch) do
    {@undefined_hash, @empty_branch}
  end

  defp calculate_hashes({@undefined_hash, branch_l, branch_r} = _inner_node) do
    {hash_l, branch_l} = calculate_hashes(branch_l)
    {hash_r, branch_r} = calculate_hashes(branch_r)
    hash_n = hash({hash_l, hash_r})
    {hash_n, {hash_n, branch_l, branch_r}}
  end

  defp calculate_hashes({@undefined_hash, {hash_key, leaf_map}} = _leaf) do
    c_hash = hash(leaf_map)
    leaf = {c_hash, {hash_key, leaf_map}}
    {c_hash, leaf}
  end

  # coveralls-ignore-start

  defp calculate_hashes({hash, _, _} = inner_node) do
    {hash, inner_node}
  end

  defp calculate_hashes({hash, _} = leaf) do
    {hash, leaf}
  end

  # coveralls-ignore-stop

  @spec leaf_hash(boolean(), leaf()) :: hash()
  defp leaf_hash(false, _), do: @undefined_hash
  defp leaf_hash(true, {_hash_key, leaf_map}), do: hash(leaf_map)

  @spec inner_node_hash(boolean(), branch(), branch()) :: hash()
  defp inner_node_hash(false, _, _), do: @undefined_hash

  defp inner_node_hash(true, branch_l, branch_r) do
    hash_l =
      case branch_l do
        @empty_branch -> @undefined_hash
        {hash_l, _leaf} -> hash_l
        {hash_l, _branch_l, _branch_r} -> hash_l
      end

    hash_r =
      case branch_r do
        @empty_branch -> @undefined_hash
        {hash_r, _leaf} -> hash_r
        {hash_r, _branch_l, _branch_r} -> hash_r
      end

    hash({hash_l, hash_r})
  end

  @spec hash(term()) :: bitstring()
  defp hash(data) do
    <<:erlang.phash2(data, @tree_leaves)::@tree_levels>>
  end
end
