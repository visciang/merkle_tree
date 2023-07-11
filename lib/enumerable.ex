defimpl Enumerable, for: MerkleTree do
  require MerkleTree

  def count(%MerkleTree{} = mt), do: {:ok, MerkleTree.size(mt)}
  def member?(%MerkleTree{} = mt, elem), do: {:ok, MerkleTree.has_key?(mt, elem)}
  def slice(%MerkleTree{}), do: {:error, __MODULE__}

  def reduce(%MerkleTree{tree: tree} = mt, acc, reducer_fn) do
    reduce_tree({mt, tree}, acc, reducer_fn)
  end

  defp reduce_tree({%MerkleTree{}, _branch}, {:halt, acc}, _reducer_fn) do
    {:halted, acc}
  end

  defp reduce_tree({%MerkleTree{}, MerkleTree.empty_branch()}, {:cont, acc}, _reducer_fn) do
    {:done, acc}
  end

  defp reduce_tree(
         {%MerkleTree{} = mt, {_hash, branch_l, branch_r} = _inner_node},
         {:cont, acc},
         reducer_fn
       ) do
    case reduce_tree({mt, branch_l}, {:cont, acc}, reducer_fn) do
      {:halted, acc} ->
        {:halted, acc}

      {:suspended, acc, continuation} ->
        {:suspended, acc, continuation}

      {:done, acc} ->
        reduce_tree({mt, branch_r}, {:cont, acc}, reducer_fn)
    end
  end

  defp reduce_tree(
         {%MerkleTree{} = mt, {_, {hash_k, leaf_map}} = _leaf},
         {:cont, acc},
         reducer_fn
       ) do
    leaf_map
    |> Enum.sort()
    |> Enum.reduce_while({:done, acc}, fn {k, v}, {:done, acc} ->
      case reducer_fn.({k, v}, acc) do
        {:halt, acc} ->
          {:halt, {:halted, acc}}

        {:suspend, acc} ->
          {:halt, {:suspended, acc, &reduce_tree_resume({mt, {hash_k, k}}, &1, reducer_fn)}}

        {:cont, next_acc} ->
          {:cont, {:done, next_acc}}
      end
    end)
  end

  defp reduce_tree_resume({%MerkleTree{tree: tree} = mt, {hash_k, k}}, acc, reducer_fn) do
    reduce_tree_resume({%MerkleTree{} = mt, 0, tree, {hash_k, k}}, acc, reducer_fn)
  end

  defp reduce_tree_resume(
         {%MerkleTree{}, _tree_level, _branch, {_hash_k, _k}},
         {:halt, acc},
         _reducer_fn
       ) do
    {:halted, acc}
  end

  # coveralls-ignore-start

  defp reduce_tree_resume(
         {%MerkleTree{} = mt, _tree_level, _branch, {hash_k, k}},
         {:suspend, acc},
         reducer_fn
       ) do
    {:suspended, acc, &reduce_tree_resume({mt, {hash_k, k}}, &1, reducer_fn)}
  end

  # coveralls-ignore-stop

  defp reduce_tree_resume(
         {%MerkleTree{} = mt, tree_level, {_hash, branch_l, branch_r} = _inner_node, {hash_k, k}},
         {:cont, acc},
         reducer_fn
       ) do
    case MerkleTree.leaf_direction(hash_k, tree_level) do
      MerkleTree.left_branch() ->
        case reduce_tree_resume(
               {%MerkleTree{} = mt, tree_level + 1, branch_l, {hash_k, k}},
               {:cont, acc},
               reducer_fn
             ) do
          # coveralls-ignore-start

          {:halted, acc} ->
            {:halted, acc}

          # coveralls-ignore-stop

          {:suspended, acc, continuation} ->
            {:suspended, acc, continuation}

          {:done, acc} ->
            reduce_tree({mt, branch_r}, {:cont, acc}, reducer_fn)
        end

      MerkleTree.right_branch() ->
        reduce_tree_resume(
          {%MerkleTree{} = mt, tree_level + 1, branch_r, {hash_k, k}},
          {:cont, acc},
          reducer_fn
        )
    end
  end

  defp reduce_tree_resume(
         {%MerkleTree{} = mt, _tree_level, {hash, {hash_k, leaf_map}} = _leaf,
          {hash_k, resume_from_key}},
         {:cont, acc},
         reducer_fn
       ) do
    leaf_map = Map.filter(leaf_map, fn {k, _v} -> k > resume_from_key end)
    reduce_tree({mt, {hash, {hash_k, leaf_map}}}, {:cont, acc}, reducer_fn)
  end
end
