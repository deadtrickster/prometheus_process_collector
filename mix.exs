defmodule PrometheusProcessCollector.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus_process_collector,
     version: "1.0.0-alpha1",
     description: description,
     package: package,
     deps: deps]
  end

  defp description do
    """
    Prometheus.io process collector.
    Collector exports the current state of process metrics including cpu, memory,
    file descriptor usage and native threads count as well as the process start and up times.
    """
  end

  defp package do
    [build_tools: ["rebar3"],
     maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus_process_collector.erl",
              "Prometheus.io Client" => "https://github.com/deadtrickster/prometheus.erl",
              "Elixir Plugs" => "https://hex.pm/packages/prometheus_plugs"},
     files: ["c_src", "src", "include", "priv", "README.md", "LICENSE", "rebar.config"]]
  end

  defp deps do
    [{:prometheus, "~> 3.0.0-alpha1"}
    ]
  end
end
