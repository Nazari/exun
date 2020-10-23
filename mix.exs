defmodule Exun.MixProject do
  use Mix.Project

  def project do
    [
      app: :exun,
      version: "0.3.0",
      elixir: "~> 1.10",
      description: descripcion(),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package()
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
      {:ex_doc, "~> 0.23", only: :dev, runtime: false},
      {:benchee, "~> 1.0", only: :dev},
    ]
  end

  defp descripcion() do
    "Symbolic Math for elixir, with units support"
  end

  defp package() do
    [
      # This option is only needed when you don't want to use the OTP application name
      name: "exun",
      # These are the default files included in the package
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/Nazari/exun"}
    ]
  end
end
