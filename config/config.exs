import Config

if Mix.env() == :test do
  config :merkle_tree, :tree_levels, 6
end
