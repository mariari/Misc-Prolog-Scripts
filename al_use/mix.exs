defmodule AlUse.MixProject do
  use Mix.Project

  def project do
    [
      app: :al_use,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      dialyzer: [
        plt_add_apps: [:ex_unit, :crypto, :mnesia],
        ignore_warnings: ".dialyzer_ignore.exs"
      ],
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:al, git: "https://github.com/anoma/AL-Ex.git", tag: "0.4.2"},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:ex_example, "~> 0.1.2"},
      {:gt_bridge, "~> 0.20.2", override: true}
    ]
  end
end
