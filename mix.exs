defmodule MerkleTree.MixProject do
  use Mix.Project

  def project do
    [
      app: :merkle_tree,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.github": :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      test_coverage: [tool: ExCoveralls],
      deps: deps(),
      dialyzer: [
        plt_local_path: "_build/plts"
      ],
      docs: docs()
    ]
  end

  def application do
    []
  end

  defp deps do
    [
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.30", only: [:dev], runtime: false},
      {:credo, "~> 1.0", only: [:dev], runtime: false},
      {:excoveralls, "~> 0.16", only: [:test]},
      {:benchee, "~> 1.1", only: [:dev, :test]}
    ]
  end

  defp docs do
    [
      extras: ["README.md"],
      groups_for_docs: [
        Basic: &(&1[:group] == :basic),
        Iteration: &(&1[:group] == :iter),
        Difference: &(&1[:group] == :diff),
        Equality: &(&1[:group] == :eq)
      ]
    ]
  end
end
