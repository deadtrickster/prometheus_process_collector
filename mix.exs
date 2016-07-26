defmodule PrometheusProcessCollector.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus_process_collector,
     version: "0.1.1",
     description: description,
     package: package,
     deps: deps]
  end

  defp description do
    """
    Prometheus.io process collector.
    """
  end

  defp package do
    [build_tools: ["rebar3"],
     maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus_process_collector.erl",
              "Prometheus.io Client" => "https://github.com/deadtrickster/prometheus.erl"},
     files: ["c_src", "src", "include", "priv", "README.md", "LICENSE", "rebar.config"]]
  end
  
  defp deps do
    [{:prometheus, "~> 1.6"}
    ]
  end
end
