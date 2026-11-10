# rush (development version)

* New `--geom-args` option for `rush plot` (and `geom_args` for `rush_plot()`)
  passes arguments to the geom, e.g. `--geom-args 'adjust = 3, alpha = 0.5'`
  for `geom_density(adjust = 3, alpha = 0.5)`. Unlike `--alpha`, which maps a
  column, it sets fixed values. It also works with the guessed geom.
* `rush plot -O svg` (or `pdf`, `jpeg`, and other ggplot2 devices) now writes
  that format to standard output. It used to be ignored, so PNG was written.
* Scripts that save a plot as SVG (`-o plot.svg` or `-O svg`) now declare
  svglite, which `ggsave()` needs for SVG. They used to fail when svglite
  wasn't installed.
* Writing a format that rush can only read (`.xls`, `.por`, `.fwf`) is now an
  error, as is an `-O` value that rush can't write. rush used to write CSV
  with that extension instead.
* Writing FASTA and FASTQ now works for data read from FASTA or FASTQ (e.g.
  `rush convert -o reads.fasta reads.fastq`), whose column names are
  lowercase after cleaning, and to standard output with `-O fasta`.
* `--names` now sets the column names of a fixed-width (`.fwf`) file. It was
  ignored, so the columns were always named `x1`, `x2`, and so on.
* `--output-indent` now sets the indentation of JSON. It was ignored, so
  JSON was always indented by 2 spaces.
* `rush sql` with a file it can't query (e.g. Excel or SQLite) now says so.
  It used to pass the file to DuckDB as CSV, which failed with a confusing
  error.
* Output templates with `l` (lowercase) or `u` (uppercase), such as
  `-o '%(table_name)l.csv'`, now work. They failed with "invalid format".
* A list of data frames is now written to Excel (`.xlsx`) and ODS files as
  one sheet per element, named after the list names. `rush convert` does the
  same for multiple inputs and databases, e.g.
  `rush convert -o book.xlsx a.csv b.csv` or
  `rush convert -o book.xlsx data.duckdb`.
* `--input-sheet` and `--output-sheet` now work for ODS files. They were
  ignored, so the first sheet was read and the sheet was named `Sheet1`.
* `rush plot` without a file is now an error, like the other commands. It
  used to read standard input; use `-` for that.
* `-o -` now writes to standard output, like omitting `-o`. It used to create
  a file named `-`, and `rush plot -o -` failed with "Unknown graphics device".
* The `<file>` description in `rush plot -h` now lists all input formats and
  explains where the plot goes without `--output`.
* Invalid option values (e.g. for `--aes`) now give their own error message,
  without purrr's "In index" wrapper.
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
