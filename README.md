# rush

<!-- badges: start -->
[![R-CMD-check](https://github.com/jeroenjanssens/rush/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jeroenjanssens/rush/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**`rush` brings R to the command line.** Run an R expression, wrangle a CSV,
query a Parquet file with SQL, or draw a plot right in your terminal — all
without opening an R session or writing a script.

```sh
rush run 'penguins |> dplyr::filter(body_mass_g > 5000) |> head()'
```

R has a wonderful ecosystem for working with data, but reaching for it usually
means launching R, loading packages, and reading files by hand. `rush` collapses
that into a single command you can pipe, redirect, and drop into any shell
pipeline — so R becomes just another Unix tool, at home next to `grep`, `awk`,
and `jq`.

## Highlights

- **One-liners, not scripts.** Evaluate any R expression straight from the shell.
- **Pipeline-native.** Reads from standard input and writes to standard output.
- **Reads what you have.** CSV, Parquet, JSON, Excel, Arrow, DuckDB, SPSS/Stata/SAS, SQLite, YAML, TOML, XML, and more.
- **Converts between formats.** The `convert` command handles it in one step.
- **Query with SQL.** Run DuckDB queries directly against your files.
- **Plots in your terminal.** Render ggplot2 graphics as ASCII art, or save to PNG/PDF.
- **Zero dependency management.** Packages are resolved on demand by `ir`.

## Installation

First install [`ir`](https://r-lib.github.io/ir/#installation). Then install
`rush` as an `ir` tool:

```sh
ir tool install github::jeroenjanssens/rush
```

## Quick tour

Evaluate an expression:

```sh
$ rush run '6 * 7'
#> [1] 42
```

Read a file into `df` and filter it:

```sh
$ rush run 'df |> dplyr::filter(body_mass_g > 5000)' penguins.csv
#> species,island,bill_length_mm,...
#> Gentoo,Biscoe,49.2,...
```

Pipe into SQL:

```sh
$ rush run 'head(df, 100)' penguins.csv | \
    rush sql "SELECT species, COUNT(*) AS n FROM stdin GROUP BY species" -
#> species,n
#> Adelie,44
#> Gentoo,34
#> Chinstrap,22
```

Convert between formats:

```sh
$ rush convert -o penguins.parquet penguins.csv
```

## Learn more

Full documentation is available at **<https://jeroenjanssens.github.io/rush>**.

## Code of Conduct

Please note that the rush project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
