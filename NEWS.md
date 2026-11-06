# rush (development version)

* `rush plot` with only `--x` now draws a bar chart for a text, factor, or
  logical column and a histogram for numbers, dates, and times, like the
  `qplot()` that rush 0.1 used. It always used a histogram, which fails for a
  discrete x (e.g. `rush plot --x time tips.csv`). Generated scripts use the
  new exported `geom_auto()`, which decides when the script runs.
* Terminal box plots no longer abort R when a group has a single value:
  scripts now use miniansi 0.1.3.
* Converting a database to another database (e.g.
  `rush convert -o copy.sqlite data.duckdb`) now copies its tables. It used
  to fail in `dbWriteTable()`, because a database input is a list of tables.
* When a generated script fails, rush now prints its error output. Without
  `--verbose` it used to discard it, so a failing plot exited with status 1
  without saying why.
* Terminal plotting now installs devout, devoutansi, and miniansi from forks
  under jeroenjanssens on GitHub, pinned to fixed commits, instead of from the
  unmaintained upstream repositories. Changes to those repositories can no
  longer break plotting.

# rush 0.2.1

* Scripts saved with `--dry-run` now run with `ir` on any machine (#10):
  * They refer to rush on GitHub, pinned to the commit or release that
    generated them, instead of as `rush`, which `ir` resolved to an unrelated
    package on CRAN.
  * They declare every package they use, including those loaded with
    `--tidyverse` and `--library`, and the readers and writers for the input
    and output formats.
* Reading delimited files no longer prints readr's column specification.

# rush 0.2.0

* `rush run` now outputs plots when the expression returns a ggplot: as
  ANSI/ASCII art in a terminal or with `-O ansi`/`-O ascii`, saved to the
  `--output` file, or as PNG bytes to stdout otherwise. Previously it silently
  wrote nothing (#8).
* `-O ascii` plot output is now plain text. It used to contain the same ANSI
  color codes as `-O ansi` (#9).

# rush 0.1.0

* Added exported R functions `rush_run()`, `rush_sql()`, `rush_plot()`, and
  `rush_convert()` as the primary package API.
* Removed `rush()` from exports (still available internally for CLI use).
* Moved `prettycode` from Imports to Suggests.
* Removed unused `LazyData` field from DESCRIPTION.

# rush 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
