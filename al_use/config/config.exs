import Config

config :logger,
  level: :error,
  handle_otp_reports: false,
  handle_sasl_reports: false

config :al,
  serialisation_dir: "src/al",
  transaction_programs: [
    AL.TransactionProgram.Bootstrap,
    AL.TransactionProgram.PackageSystem
  ],
  package_channels: [
    {:builtin, {:priv, "packages"}}
  ],
  package_environment: [:jack]

config :al, AL.MCP,
  enabled: true,
  ip: {127, 0, 0, 1},
  port: 3031
