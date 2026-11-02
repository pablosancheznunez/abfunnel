# Contributing

Thank you for helping improve abfunnel. Bug reports with a small example, and proposals backed by the methods literature, are welcome.

## Running the tests

- `R CMD build . && R CMD check --as-cran abfunnel_*.tar.gz`
- or `devtools::check()`.

## Small changes (welcome as pull requests)

- Documentation, examples and the report layout.
- Compatibility fixes, as long as every test still passes unchanged.

## Changes to discuss in an issue first

- The statistical methods in `R/design.R`, `R/analyse.R` and `R/funnel.R`, and the synthetic data in `data-raw/`.
- New dependencies: abfunnel deliberately depends only on base R.

Tests are never weakened or deleted to make them pass.
