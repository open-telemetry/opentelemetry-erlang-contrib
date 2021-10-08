# Used by "mix format"
[
  import_deps: [:ecto],
  inputs: ["*.{ex,exs}", "priv/*/seeds.exs", "{rel,config,lib,test}/**/*.{ex,exs}"],
  subdirectories: ["priv/*/migrations"],
  line_length: 120
]
