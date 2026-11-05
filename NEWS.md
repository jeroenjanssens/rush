# rush (development version)

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
