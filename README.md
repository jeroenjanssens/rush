
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rush

<!-- badges: start -->

[![R-CMD-check](https://github.com/jeroenjanssens/rush/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jeroenjanssens/rush/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**`rush` brings R to the command line.** Run an R expression, wrangle a
CSV, query a Parquet file with SQL, or draw a plot right in your
terminal — all without opening an R session or writing a script.

``` sh
rush run 'mtcars |> dplyr::filter(mpg > 25) |> head()'
```

R has a wonderful ecosystem for working with data, but reaching for it
usually means launching R, loading packages, and reading files by hand.
`rush` collapses that into a single command you can pipe, redirect, and
drop into any shell pipeline — so R becomes just another Unix tool, at
home next to `grep`, `awk`, and `jq`.

Under the hood, each invocation assembles a small, self-contained R
script and runs it with [`ir`](https://r-lib.github.io/ir/), the R
interpreter launcher from r-lib. The script declares its own package
dependencies in `#|` frontmatter, and `ir` installs whatever it needs on
the fly. That means `rush` itself stays lightweight, and you never have
to manage a library of packages just to run a one-liner.

## Highlights

- **One-liners, not scripts.** Evaluate any R expression straight from
  the shell.
- **Pipeline-native.** Reads from standard input and writes to standard
  output, so it composes with every other command-line tool.
- **Reads what you have.** CSV and other delimited text,
  [Parquet](#parquet), and [DuckDB](#duckdb) databases — chosen
  automatically by file extension.
- **Query with SQL.** The [`sql`](#querying-with-sql) command runs
  DuckDB queries directly against your files, no import step required.
- **Plots in your terminal.** The [`plot`](#plotting) command renders
  ggplot2 graphics as ANSI/ASCII art, or saves them to PNG, PDF, and
  more.
- **Zero dependency management.** Packages are resolved on demand by
  `ir`.

## Installation

First install [`ir`](https://r-lib.github.io/ir/#installation). Then
install `rush` as an `ir` tool, which puts a `rush` launcher on your
`PATH`:

``` sh
ir tool install github::jeroenjanssens/rush
```

That’s it — there’s no separate R package to load. The first time a
command needs a package (say, ggplot2 for a plot), `ir` fetches and
caches it for you.

## A quick tour

Evaluate an expression — the value of the last expression is printed:

``` bash
rush run '6 * 7'
#> 42
```

Read from standard input with `-`. Input is read into a data frame
called `df`, so `seq 6` becomes a one-column table you can compute on:

``` bash
seq 6 | rush run -H '2 * sum(df$x1)' -
#> 42
```

Give `rush run` a file and it is read into `df` before your expression
runs. With no expression, the data frame is simply printed back out — a
handy way to peek at a file:

``` bash
rush run 'head(mtcars, 10)' | tee mtcars.csv
#> mpg,cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb
#> 21,6,160,110,3.9,2.62,16.46,0,1,4,4
#> 21,6,160,110,3.9,2.875,17.02,0,1,4,4
#> 22.8,4,108,93,3.85,2.32,18.61,1,1,4,1
#> 21.4,6,258,110,3.08,3.215,19.44,1,0,3,1
#> 18.7,8,360,175,3.15,3.44,17.02,0,0,3,2
#> 18.1,6,225,105,2.76,3.46,20.22,1,0,3,1
#> 14.3,8,360,245,3.21,3.57,15.84,0,0,3,4
#> 24.4,4,146.7,62,3.69,3.19,20,1,0,4,2
#> 22.8,4,140.8,95,3.92,3.15,22.9,1,0,4,2
#> 19.2,6,167.6,123,3.92,3.44,18.3,1,0,4,4
```

Because `rush` writes plain CSV to standard output, you can keep piping
into the next command — including another `rush`:

``` bash
rush run 'head(mtcars)' | rush run 'df |> dplyr::select(mpg, cyl, hp)' -
#> mpg,cyl,hp
#> 21,6,110
#> 21,6,110
#> 22.8,4,93
#> 21.4,6,110
#> 18.7,8,175
#> 18.1,6,105
```

Load extra packages with `-l`, or enter the whole Tidyverse with `-t`:

``` bash
rush run -t 'starwars |> count(species, sort = TRUE) |> head(3)'
#> species,n
#> Human,35
#> Droid,6
#> NA,4
```

## Working with data files

`rush` picks a reader based on the file extension, so the same commands
work whatever format your data is in. A single file is read into `df`;
pass several and each is read into a named element of a list called
`dfs`.

### Delimited text

CSV is the default. Use `--delimiter` (`-d`) for other separators and
`--no-header` (`-H`) for files without a header row:

``` bash
rush run 'df |> dplyr::filter(mpg > 22) |> dplyr::select(mpg, cyl, hp)' mtcars.csv
#> mpg,cyl,hp
#> 22.8,4,93
#> 24.4,4,62
#> 22.8,4,95
```

<a id="parquet"></a>

### Parquet

Files ending in `.parquet` or `.pq` are read with
[nanoparquet](https://nanoparquet.r-lib.org/) — no flags needed. Point
`--output` (`-o`) at a `.parquet` file to write one, which makes `rush`
a quick way to convert between formats:

``` bash
# Convert CSV to Parquet...
rush run --output mtcars.parquet 'df' mtcars.csv

# ...then read it straight back
rush run 'head(df, 3)' mtcars.parquet
#> mpg,cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb
#> 21,6,160,110,3.9,2.62,16.46,0,1,4,4
#> 21,6,160,110,3.9,2.875,17.02,0,1,4,4
#> 22.8,4,108,93,3.85,2.32,18.61,1,1,4,1
```

<a id="duckdb"></a>

### DuckDB

Point `rush` at a [DuckDB](https://duckdb.org/) database (`.duckdb` or
`.ddb`) and every table is read into the `dfs` list, keyed by table
name:

``` bash
rush run 'names(dfs)' shop.duckdb
#> customers
#> orders
```

If a database holds a single table, it is also bound to `df` for
convenience, so a one-table database behaves just like any other single
input.

## Querying with SQL

Sometimes SQL is simply the clearest way to express what you want —
especially joins and aggregations. The `sql` command runs a
[DuckDB](https://duckdb.org/) query directly against your files, with no
import step. Each file becomes a relation named after the file, so you
can reference it right in the query:

``` bash
rush sql "SELECT cyl, ROUND(AVG(mpg), 1) AS avg_mpg, COUNT(*) AS n
          FROM mtcars GROUP BY cyl ORDER BY cyl" mtcars.parquet
#> cyl,avg_mpg,n
#> 4,23.3,3
#> 6,20.1,5
#> 8,16.5,2
```

Because DuckDB reads the file itself, this works on datasets far larger
than memory — the filtering and aggregation happen inside DuckDB, and
only the result comes back to R.

CSV files are read with `read_csv_auto`, Parquet with `read_parquet`,
and a `.duckdb` database is attached so its tables are addressed as
`name.table`. That makes joins across a whole database natural:

``` bash
rush sql "SELECT c.name, SUM(o.amount) AS total
          FROM shop.customers c JOIN shop.orders o ON c.id = o.cust
          GROUP BY c.name ORDER BY total DESC" shop.duckdb
#> name,total
#> Carol,65
#> Alice,55
#> Bob,10
```

The result of a query is an ordinary data frame, so everything else
composes as usual — pipe it onward, or save it with `--output`,
including back to Parquet:

``` bash
rush run 'head(mtcars, 5)' | rush sql "SELECT cyl, mpg FROM stdin WHERE mpg > 21" -
#> cyl,mpg
#> 4,22.8
#> 6,21.4
```

<a id="plotting"></a>

## Plotting

The `plot` command builds a [ggplot2](https://ggplot2.tidyverse.org/)
graphic from your data. Choose columns with `--x`, `--y`, `--color`, and
friends, and a sensible geom is guessed for you (override it with
`--geom`).

Show the generated script with `--dry-run` to see exactly what will run:

``` bash
rush plot --dry-run --x mpg --geom density --fill 'factor(cyl)' mtcars.csv
#> #!/usr/bin/env -S ir run
#> #| packages:
#> #|   - rlang
#> #|   - cli
#> #|   - tibble
#> #|   - readr
#> #|   - ggplot2
#> #|   - fs
#> #|   - github::coolbutuseless/devout
#> #|   - github::jeroenjanssens/miniansi
#> #|   - github::coolbutuseless/devoutansi
#> #|   - janitor
#> 
#> .rush <- list(
#>   output = NULL,
#>   output_format = "delim",
#>   width = NULL,
#>   height = NULL,
#>   units = "in",
#>   dpi = 300,
#>   delimiter = ",",
#>   has_post = FALSE
#> )
#> 
#> library(ggplot2)
#> df <- janitor::clean_names(readr::read_delim("mtcars.csv", delim = ",", col_names = TRUE))
#> result <- ggplot(df, aes(x = mpg, fill = factor(cyl))) + geom_density()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") file("stdout", "wb", raw = TRUE)
#>   else file("/dev/stdout", "wb", raw = TRUE)
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) out <- if (.has_tty) "ansi" else "png"
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) w <- cli::console_width()
#>   devoutansi::ansi(width = w, height = h, plain_ascii = TRUE, char_lookup_table = 2)
#>   if (!.rush$has_post) {
#>     result <- result +
#>       ggplot2::theme_minimal() +
#>       ggplot2::theme(panel.grid = ggplot2::element_blank())
#>   }
#>   print(result)
#>   invisible(grDevices::dev.off())
#> } else {
#>   if (fs::path_ext(out) == "") {
#>     output_filename <- tempfile()
#>     device <- out
#>     cat_output <- TRUE
#>   } else {
#>     output_filename <- out
#>     device <- NULL
#>     cat_output <- FALSE
#>   }
#>   if (is.null(w)) w <- 6
#>   if (is.null(h)) h <- 4
#>   ggplot2::ggsave(output_filename, result, device = device,
#>                   width = w, height = h, units = .rush$units, dpi = .rush$dpi)
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

Run it in a terminal and the plot is drawn right there as ANSI/ASCII
art. Redirect the output to a file instead and `rush` writes an image,
picking the device from the extension:

``` bash
rush plot --x mpg --geom density --fill 'factor(cyl)' mtcars.csv > ../man/figures/mtcars.png
```

![](man/figures/mtcars.png)

Terminal plotting relies on three packages that are not on CRAN —
`devout`, `miniansi`, and `devoutansi` — but you do not need to install
them yourself. `rush` declares them in the generated script’s
frontmatter, and `ir` fetches them from GitHub the first time they are
needed.

## Saving and reusing scripts

The script that `--dry-run` prints is not a throwaway — it is a
complete, self-contained `ir` script. Redirect it to a file and you have
a reproducible artifact you can version, share, or run again later, long
after the one-liner has scrolled out of your shell history:

``` bash
rush run --dry-run 'df |> dplyr::filter(mpg > 21)' mtcars.csv > analysis.R
head -3 analysis.R
#> #!/usr/bin/env -S ir run
#> #| packages:
#> #|   - rlang
```

The `#!/usr/bin/env -S ir run` shebang on the first line makes the file
directly executable. `ir` reads the `#|` frontmatter, installs any
packages the script declares, and runs it — so the saved script carries
its own dependencies and needs nothing but `ir` on the target machine:

``` bash
chmod +x analysis.R
./analysis.R 2>/dev/null
#> mpg,cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb
#> 22.8,4,108,93,3.85,2.32,18.61,1,1,4,1
#> 21.4,6,258,110,3.08,3.215,19.44,1,0,3,1
#> 24.4,4,146.7,62,3.69,3.19,20,1,0,4,2
#> 22.8,4,140.8,95,3.92,3.15,22.9,1,0,4,2
```

This is also handy for tweaking: dump a `rush` one-liner to a file, edit
the generated R to do something `rush` does not express directly, and
run it with `ir run analysis.R`.

## Help

Every command has built-in help. Start with the top level:

``` bash
rush -h
#> rush: R Scripting at the Command Line
#> 
#> Usage:
#>   rush [options] <command> [<args>]
#> 
#> Options:
#>   -n, --dry-run            Only print generated script.
#>   -h, --help               Show this help.
#>   -q, --quiet              Be quiet.
#>       --seed <int>         Seed random number generator.
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
#> 
#> Commands:
#>   plot
#>   run
#>   sql
```

Then dig into a specific command:

``` bash
rush run -h
#> rush: Run an R expression
#> 
#> Usage:
#>   rush run [options] [<expression>] [--] [<file>...]
#> 
#> Arguments:
#>   <expression>             R expression to evaluate. The value of the last
#>                            expression is printed or written out.
#>   <file>                   Data file(s) to read into a data frame named 'df'
#>                            before the expression runs. The reader is chosen by
#>                            extension: '.parquet'/'.pq' via nanoparquet,
#>                            '.duckdb'/'.ddb' (each table becomes an element of
#>                            'dfs') via DuckDB, everything else as delimited
#>                            text. Use '-' to read delimited text from standard
#>                            input. With multiple files, each is read into a
#>                            named element of a list 'dfs'.
#> 
#> Reading options:
#>   -d, --delimiter <str>    Delimiter [default: ,].
#>   -C, --no-clean-names     No clean names.
#>   -H, --no-header          No header.
#> 
#> Setup options:
#>   -l, --library <name>     Libraries to load.
#>   -t, --tidyverse          Enter the Tidyverse.
#> 
#> Saving options:
#>       --dpi <str|int>      Plot resolution [default: 300].
#>       --height <int>       Plot height.
#>   -o, --output <str>       Output file.
#>       --units <str>        Plot size units [default: in].
#>   -w, --width <int>        Plot width.
#> 
#> General options:
#>   -n, --dry-run            Only print generated script.
#>   -h, --help               Show this help.
#>   -q, --quiet              Be quiet.
#>       --seed <int>         Seed random number generator.
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
```

``` bash
rush sql -h
#> rush: Query files with SQL (DuckDB)
#> 
#> Usage:
#>   rush sql [options] [<query>] [--] [<file>...]
#> 
#> Arguments:
#>   <query>                  DuckDB SQL query to run. Its result is printed or
#>                            written out.
#>   <file>                   File(s) to expose to the query, each as a relation
#>                            named after the file's base name. CSV files are
#>                            read with read_csv_auto, Parquet with read_parquet,
#>                            and a '.duckdb' database is attached so its tables
#>                            are addressed as name.table. Use '-' to read CSV
#>                            from standard input as a relation named 'stdin'.
#> 
#> Setup options:
#>   -l, --library <name>     Libraries to load.
#>   -t, --tidyverse          Enter the Tidyverse.
#> 
#> Saving options:
#>       --dpi <str|int>      Plot resolution [default: 300].
#>       --height <int>       Plot height.
#>   -o, --output <str>       Output file.
#>       --units <str>        Plot size units [default: in].
#>   -w, --width <int>        Plot width.
#> 
#> General options:
#>   -n, --dry-run            Only print generated script.
#>   -h, --help               Show this help.
#>   -q, --quiet              Be quiet.
#>       --seed <int>         Seed random number generator.
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
```

``` bash
rush plot -h
#> rush: Quick plot
#> 
#> Usage:
#>   rush plot [options] [--] [<file>...]
#> 
#> Arguments:
#>   <file>                   Data file(s) to read before plotting. The reader is
#>                            chosen by extension: '.parquet'/'.pq' via
#>                            nanoparquet, '.duckdb'/'.ddb' via DuckDB, everything
#>                            else as delimited text. A single file is read into a
#>                            data frame named 'df'; use '-' or omit to read
#>                            delimited text from standard input. Multiple files
#>                            are each read into a named element of a list 'dfs';
#>                            combine them into 'df' yourself with the --pre
#>                            option, e.g. 'df <- dplyr::bind_rows(dfs)'.
#> 
#> Reading options:
#>   -d, --delimiter <str>    Delimiter [default: ,].
#>   -C, --no-clean-names     No clean names.
#>   -H, --no-header          No header.
#> 
#> Setup options:
#>   -l, --library <name>     Libraries to load.
#>   -t, --tidyverse          Enter the Tidyverse.
#> 
#> Plotting options:
#>       --aes <key=value>    Additional aesthetics.
#>   -a, --alpha <name>       Alpha column.
#>   -c, --color <name>       Color column.
#>       --facets <formula>   Facet specification.
#>   -f, --fill <name>        Fill column.
#>   -g, --geom <geom>        Geometry [default: auto].
#>       --group <name>       Group column.
#>       --log <x|y|xy>       Variables to log transform.
#>       --margins            Display marginal facets.
#>       --post <code>        Code to run after plotting.
#>       --pre <code>         Code to run before plotting.
#>       --shape <name>       Shape column.
#>       --size <name>        Size column.
#>       --title <str>        Plot title.
#>   -x, --x <name>           X column.
#>       --xlab <str>         X axis label.
#>   -y, --y <name>           Y column.
#>       --ylab <str>         Y axis label.
#>   -z, --z <name>           Z column.
#> 
#> Saving options:
#>       --dpi <str|int>      Plot resolution [default: 300].
#>       --height <int>       Plot height.
#>   -o, --output <str>       Output file.
#>       --units <str>        Plot size units [default: in].
#>   -w, --width <int>        Plot width.
#> 
#> General options:
#>   -n, --dry-run            Only print generated script.
#>   -h, --help               Show this help.
#>   -q, --quiet              Be quiet.
#>       --seed <int>         Seed random number generator.
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
```

## Code of Conduct

Please note that the rush project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
