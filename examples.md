# rush examples

A comprehensive collection of examples showing what `rush` can do. Each
section demonstrates a specific argument or capability with real data
and realistic use cases.

------------------------------------------------------------------------

## rush run

### Simple expressions

At its core, `rush run` evaluates any R expression and prints the
result.

``` bash
rush run '6 * 7'
#> 42
```

Multiple semicolon-separated expressions work; the value of the last one
is printed:

``` bash
rush run 'x <- 1:10; mean(x)'
#> 5.5
```

Strings, vectors, and lists all print naturally:

``` bash
rush run 'paste("Hello", "from", "R", sep = ", ")'
#> Hello, from, R
```

``` bash
rush run 'rev(LETTERS[1:8])'
#> H
#> G
#> F
#> E
#> D
#> C
#> B
#> A
```

### Reading a single file

Pass a file path after the expression, and it’s read into a data frame
called `df`:

``` bash
rush run 'nrow(df)' mtcars.csv
#> 20
```

With just a file and no expression, the data is simply printed — a quick
way to peek:

``` bash
rush run 'head(df, 5)' mtcars.csv
#> mpg,cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb
#> 21,6,160,110,3.9,2.62,16.46,0,1,4,4
#> 21,6,160,110,3.9,2.875,17.02,0,1,4,4
#> 22.8,4,108,93,3.85,2.32,18.61,1,1,4,1
#> 21.4,6,258,110,3.08,3.215,19.44,1,0,3,1
#> 18.7,8,360,175,3.15,3.44,17.02,0,0,3,2
```

### Reading from standard input (`-`)

Use `-` to read from stdin. This makes `rush` composable in shell
pipelines:

``` bash
echo "name,score
Alice,92
Bob,85
Carol,97" | rush run 'df |> dplyr::arrange(dplyr::desc(score))' -
#> name,score
#> Carol,97
#> Alice,92
#> Bob,85
```

You can pipe the output of one `rush` into another:

``` bash
rush run 'head(mtcars, 5)' | rush run 'dplyr::select(df, mpg, hp, wt)' -
#> mpg,hp,wt
#> 21,110,2.62
#> 21,110,2.875
#> 22.8,93,2.32
#> 21.4,110,3.215
#> 18.7,175,3.44
```

### Reading multiple files

When you pass multiple files, each is read into a list called `dfs`,
keyed by the file’s base name:

``` bash
rush run 'names(dfs)' mtcars.csv cities.tsv
#> mtcars
#> cities
```

``` bash
rush run 'nrow(dfs[["mtcars"]])' mtcars.csv cities.tsv
#> 20
```

### `--delimiter` (`-d`)

Override the delimiter for both reading and writing. Here we read a
tab-separated file:

``` bash
rush run -d "\t" 'head(df, 3)' cities.tsv
#> city_pop_province
#> Amsterdam    921402  Noord-Holland
#> Rotterdam    655468  Zuid-Holland
#> Utrecht  361924  Utrecht
```

And write tab-separated output:

``` bash
rush run -d "\t" 'head(mtcars, 3)' | cat
#> mpg\cyl\disp\hp\drat\wt\qsec\vs\am\gear\carb
#> 21\6\160\110\3.9\2.62\16.46\0\1\4\4
#> 21\6\160\110\3.9\2.875\17.02\0\1\4\4
#> 22.8\4\108\93\3.85\2.32\18.61\1\1\4\1
```

### `--no-header` (`-H`)

Read a file that has no header row. Columns are auto-named `x1`, `x2`,
…:

``` bash
rush run -H 'sum(df$x1)' numbers.txt
#> 150
```

``` bash
seq 5 | rush run -H 'cumsum(df$x1)' -
#> 1
#> 3
#> 6
#> 10
#> 15
```

### `--no-clean-names` (`-C`)

By default, `rush` passes data frames through
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
to normalize column names (lowercase, underscores). Disable this with
`-C` to preserve the original names exactly:

``` bash
echo "First Name,Last Name,Score (%)
Alice,Smith,92" | rush run 'names(df)' -
#> first_name
#> last_name
#> score_percent
```

``` bash
echo "First Name,Last Name,Score (%)
Alice,Smith,92" | rush run -C 'names(df)' -
#> First Name
#> Last Name
#> Score (%)
```

### `--tidyverse` (`-t`)

Load the entire Tidyverse (plus glue) so you can use dplyr, tidyr,
stringr, etc. without qualifying names:

``` bash
rush run -t 'mtcars |> group_by(cyl) |> summarise(mean_mpg = mean(mpg)) |> arrange(cyl)' mtcars.csv
#> cyl,mean_mpg
#> 4,26.663636363636364
#> 6,19.742857142857144
#> 8,15.1
```

### `--library` (`-l`)

Load specific packages. Comma-separate multiple packages:

``` bash
rush run -l stringr 'stringr::str_to_title("hello world")'
#> Hello World
```

``` bash
rush run -l dplyr,tidyr 'df |>
  tidyr::pivot_longer(cols = c(mpg, hp), names_to = "metric") |>
  dplyr::slice_head(n = 4)' mtcars.csv
#> cyl,disp,drat,wt,qsec,vs,am,gear,carb,metric,value
#> 6,160,3.9,2.62,16.46,0,1,4,4,mpg,21
#> 6,160,3.9,2.62,16.46,0,1,4,4,hp,110
#> 6,160,3.9,2.875,17.02,0,1,4,4,mpg,21
#> 6,160,3.9,2.875,17.02,0,1,4,4,hp,110
```

### `--seed`

Set the random seed for reproducible results:

``` bash
rush run --seed 42 'sample(1:100, 5)'
#> 49
#> 65
#> 25
#> 74
#> 18
```

Run it again — same seed, same result:

``` bash
rush run --seed 42 'sample(1:100, 5)'
#> 49
#> 65
#> 25
#> 74
#> 18
```

### `--output` (`-o`)

Write the result to a file instead of stdout. The output format is
inferred from the extension:

``` bash
rush run -o summary.csv 'data.frame(stat = c("min", "mean", "max"),
                                     mpg = c(min(df$mpg), mean(df$mpg), max(df$mpg)))' mtcars.csv
cat summary.csv
#> stat,mpg
#> min,10.4
#> mean,20.13
#> max,33.9
```

Write to Parquet:

``` bash
rush run -o subset.parquet 'dplyr::filter(df, mpg > 25)' mtcars.csv
rush run 'df' subset.parquet
#> mpg,cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb
#> 32.4,4,78.7,66,4.08,2.2,19.47,1,1,4,1
#> 30.4,4,75.7,52,4.93,1.615,18.52,1,1,4,2
#> 33.9,4,71.1,65,4.22,1.835,19.9,1,1,4,1
```

### `--dry-run` (`-n`)

Print the generated script instead of running it. This is great for
learning what `rush` does under the hood, or for saving the script to a
file:

``` bash
rush run -n 'df |> dplyr::filter(mpg > 25)' mtcars.csv
#> #!/usr/bin/env -S ir run
#> #| packages:
#> #|   - rlang
#> #|   - cli
#> #|   - tibble
#> #|   - readr
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- dplyr::filter(df, mpg > 25)
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> if (.has_tty) {
#>   options(
#>     width = if (is.null(.rush$width)) cli::console_width() else .rush$width
#>   )
#> }
#> 
#> if (rlang::is_atomic(result)) {
#>   cli::cat_line(result)
#> } else if (rlang::is_bare_list(result)) {
#>   result <- tibble::enframe(result)
#> }
#> 
#> if (is.data.frame(result)) {
#>   if (.has_tty && is.null(.rush$output)) {
#>     options(
#>       tibble.width = if (is.null(.rush$width)) {
#>         cli::console_width()
#>       } else {
#>         .rush$width
#>       }
#>     )
#>     print(tibble::as_tibble(result), n = .rush$height)
#>   } else if (identical(.rush$output_format, "parquet")) {
#>     nanoparquet::write_parquet(result, .rush$output)
#>   } else {
#>     con <- if (is.null(.rush$output)) .stdout_binary() else .rush$output
#>     readr::write_delim(result, con, delim = .rush$delimiter)
#>   }
#> }
```

### `--no-ir` (`-I`)

Run the generated script with plain `Rscript` instead of `ir`. No
package resolution happens — everything must already be installed:

``` bash
rush run -I '2 + 2'
#> 4
```

### `--verbose` (`-v`)

Show the parsed arguments before running. Useful for debugging complex
invocations:

``` bash
rush run -v --seed 7 'sum(1:10)' 2>&1
#> ── Arguments ───────────────────────────────────────────────────────────────────
#> • command         <chr> "run"
#> • delimiter       <chr> ","
#> • dpi             <dbl> 300
#> • dry_run         <lgl> FALSE
#> • expression      <language> list(sum(1:10))
#> • file            <list> list()
#> • height          <NULL> 
#> • help            <lgl> FALSE
#> • library         <NULL> 
#> • no_clean_names  <lgl> FALSE
#> • no_header       <lgl> FALSE
#> • no_ir           <lgl> FALSE
#> • output          <NULL> 
#> • quiet           <lgl> FALSE
#> • run             <lgl> TRUE
#> • seed            <int> 7L
#> • tidyverse       <lgl> FALSE
#> • units           <chr> "in"
#> • verbose         <lgl> TRUE
#> • version         <lgl> FALSE
#> • width           <NULL> 
#> ────────────────────────────────────────────────────────────────────────────────
#> 55
```

### Reading Parquet files

Files ending in `.parquet` or `.pq` are read with nanoparquet
automatically:

``` bash
rush run 'df |> dplyr::filter(dep_delay > 10) |> dplyr::select(carrier, origin, dest, dep_delay)' flights.parquet
#> carrier,origin,dest,dep_delay
#> DL,LGA,ATL,12
#> AA,JFK,LAX,120
#> DL,LGA,ATL,22
#> WN,EWR,DEN,45
```

### Reading DuckDB databases

Point `rush` at a `.duckdb` file and all tables are loaded into `dfs`:

``` bash
rush run 'lapply(dfs, head, 3)' inventory.duckdb
#> name,value
#> products,
#> sales,
```

------------------------------------------------------------------------

## rush sql

### Basic queries

Run a SQL query against one or more files. Each file becomes a DuckDB
relation:

``` bash
rush sql "SELECT mpg, cyl, hp FROM mtcars WHERE mpg > 25 ORDER BY mpg DESC" mtcars.csv
#> mpg,cyl,hp
#> 33.9,4,65
#> 32.4,4,66
#> 30.4,4,52
```

### Aggregations

DuckDB’s full SQL is available — aggregations, window functions, CTEs:

``` bash
rush sql "SELECT cyl,
                 COUNT(*) AS n,
                 ROUND(AVG(mpg), 1) AS avg_mpg,
                 MIN(hp) AS min_hp,
                 MAX(hp) AS max_hp
          FROM mtcars
          GROUP BY cyl
          ORDER BY cyl" mtcars.csv
#> cyl,n,avg_mpg,min_hp,max_hp
#> 4,6,27.8,52,95
#> 6,6,19.8,105,123
#> 8,8,14.7,175,245
```

### Querying Parquet files

Parquet files are registered via DuckDB’s `read_parquet`, so large-file
queries run without loading everything into memory:

``` bash
rush sql "SELECT carrier, COUNT(*) AS flights, ROUND(AVG(arr_delay), 1) AS avg_delay
          FROM flights
          GROUP BY carrier
          ORDER BY avg_delay DESC" flights.parquet
#> carrier,flights,avg_delay
#> WN,1,50
#> AA,3,33.7
#> DL,3,7
#> UA,3,-4.3
```

### Querying DuckDB databases

A `.duckdb` file is attached as a schema, and its tables are accessible
as `schema.table`:

``` bash
rush sql "SELECT p.name, p.category, SUM(s.quantity) AS total_sold
          FROM inventory.products p
          JOIN inventory.sales s ON p.id = s.product_id
          GROUP BY p.name, p.category
          ORDER BY total_sold DESC" inventory.duckdb
#> name,category,total_sold
#> Doohickey,A,16
#> Widget,A,16
#> Whatsit,A,9
#> Gizmo,B,6
#> Thingamajig,B,5
#> Gadget,B,3
```

### Querying standard input

Use `-` to read CSV from stdin as a relation named `stdin`:

``` bash
echo "item,price,qty
apple,1.20,10
banana,0.80,25
cherry,3.50,5" | rush sql "SELECT item, price * qty AS total FROM stdin ORDER BY total DESC" -
#> item,total
#> banana,20
#> cherry,17.5
#> apple,12
```

### Joining multiple files

Pass multiple files and each becomes a relation you can join:

``` bash
echo "cyl,fuel_type
4,regular
6,premium
8,premium" > fuel.csv
rush sql "SELECT m.cyl, f.fuel_type, ROUND(AVG(m.mpg), 1) AS avg_mpg
          FROM mtcars m JOIN fuel f ON m.cyl = f.cyl
          GROUP BY m.cyl, f.fuel_type
          ORDER BY m.cyl" mtcars.csv fuel.csv
#> cyl,fuel_type,avg_mpg
#> 4,regular,27.8
#> 6,premium,19.8
#> 8,premium,14.7
```

### SQL with `--output`

Pipe a SQL result to Parquet or another file:

``` bash
rush sql -o efficient.parquet "SELECT * FROM mtcars WHERE mpg > 20" mtcars.csv
rush run 'nrow(df)' efficient.parquet
#> 9
```

### SQL with `--tidyverse`

Load tidyverse packages alongside the SQL query — useful when you want
to post-process the result with `--library` or pipe it into further R:

``` bash
rush sql -t "SELECT cyl, AVG(mpg) as avg_mpg FROM mtcars GROUP BY cyl" mtcars.csv
#> cyl,avg_mpg
#> 6,19.75
#> 8,14.675000000000002
#> 4,27.783333333333335
```

### SQL `--dry-run`

See the generated script that runs the query:

``` bash
rush sql -n "SELECT COUNT(*) FROM mtcars" mtcars.csv
#> #!/usr/bin/env -S ir run
#> #| packages:
#> #|   - rlang
#> #|   - cli
#> #|   - tibble
#> #|   - readr
#> #|   - duckdb
#> #|   - DBI
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
#> con <- DBI::dbConnect(duckdb::duckdb())
#> invisible(DBI::dbExecute(
#>   con,
#>   "CREATE VIEW mtcars AS SELECT * FROM read_csv_auto('mtcars.csv')"
#> ))
#> result <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM mtcars")
#> DBI::dbDisconnect(con, shutdown = TRUE)
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> if (.has_tty) {
#>   options(
#>     width = if (is.null(.rush$width)) cli::console_width() else .rush$width
#>   )
#> }
#> 
#> if (rlang::is_atomic(result)) {
#>   cli::cat_line(result)
#> } else if (rlang::is_bare_list(result)) {
#>   result <- tibble::enframe(result)
#> }
#> 
#> if (is.data.frame(result)) {
#>   if (.has_tty && is.null(.rush$output)) {
#>     options(
#>       tibble.width = if (is.null(.rush$width)) {
#>         cli::console_width()
#>       } else {
#>         .rush$width
#>       }
#>     )
#>     print(tibble::as_tibble(result), n = .rush$height)
#>   } else if (identical(.rush$output_format, "parquet")) {
#>     nanoparquet::write_parquet(result, .rush$output)
#>   } else {
#>     con <- if (is.null(.rush$output)) .stdout_binary() else .rush$output
#>     readr::write_delim(result, con, delim = .rush$delimiter)
#>   }
#> }
```

------------------------------------------------------------------------

## rush plot

### Scatter plot (default with x and y)

When both `--x` and `--y` are given, `rush` guesses `geom_point`:

``` bash
rush plot -n -x wt -y mpg mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg)) + geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### Histogram (default with x only)

When only `--x` is given, `rush` guesses `geom_histogram`:

``` bash
rush plot -n -x mpg mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = mpg)) + geom_histogram()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--geom` (`-g`)

Override the guessed geom. Any ggplot2 geom suffix works:

``` bash
rush plot -n -x mpg -g density mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = mpg)) + geom_density()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

``` bash
rush plot -n -x cyl -y mpg -g boxplot mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = cyl, y = mpg)) + geom_boxplot()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

``` bash
rush plot -n -x wt -y mpg -g smooth mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg)) + geom_smooth()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--color` (`-c`)

Map a column to the color aesthetic:

``` bash
rush plot -n -x wt -y mpg -c 'factor(cyl)' mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg, color = factor(cyl))) + geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--fill` (`-f`)

Map a column to fill (useful for histograms, bar charts, density plots):

``` bash
rush plot -n -x mpg -g density -f 'factor(cyl)' --alpha 0.5 mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = mpg, alpha = 0.5, fill = factor(cyl))) +
#>   geom_density()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--alpha` (`-a`)

Map a column to transparency, or use a constant for semi-transparent
geoms:

``` bash
rush plot -n -x wt -y mpg --size hp --alpha 0.6 mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg, alpha = 0.6, size = hp)) +
#>   geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--size`

Map a column to point size — great for bubble charts:

``` bash
rush plot -n -x wt -y mpg --size hp -c 'factor(cyl)' mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg, color = factor(cyl), size = hp)) +
#>   geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--shape`

Map a column to point shape:

``` bash
rush plot -n -x wt -y mpg --shape 'factor(vs)' mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg, shape = factor(vs))) + geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--group`

Group observations for geoms like `geom_line` that connect points:

``` bash
rush plot -n -x wt -y mpg --group 'factor(cyl)' -g line mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg, group = factor(cyl))) + geom_line()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--aes`

Pass additional aesthetics as `key=value` pairs for anything not covered
by the dedicated flags:

``` bash
rush plot -n -x wt -y mpg --aes "label=rownames(df)" -g text mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg, label = rownames(df))) + geom_text()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--log`

Log-transform axes. Accepts `x`, `y`, or `xy`:

``` bash
rush plot -n -x hp -y mpg --log xy mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = hp, y = mpg)) +
#>   geom_point() +
#>   scale_x_log10() +
#>   scale_y_log10()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

``` bash
rush plot -n -x hp -y mpg --log x mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = hp, y = mpg)) + geom_point() + scale_x_log10()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--facets` (one-sided: facet_wrap)

A one-sided formula uses `facet_wrap`:

``` bash
rush plot -n -x wt -y mpg --facets '~ cyl' mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--facets` (two-sided: facet_grid) + `--margins`

A two-sided formula uses `facet_grid`. Add `--margins` to show marginal
panels:

``` bash
rush plot -n -x wt -y mpg --facets 'vs ~ cyl' --margins mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg)) +
#>   geom_point() +
#>   facet_grid(vs ~ cyl, margins = TRUE)
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--title`, `--xlab`, `--ylab`

Add axis labels and a title:

``` bash
rush plot -n -x wt -y mpg --title "Fuel Efficiency vs Weight" --xlab "Weight (1000 lbs)" --ylab "Miles per Gallon" mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg)) +
#>   geom_point() +
#>   labs(
#>     x = "Weight (1000 lbs)",
#>     y = "Miles per Gallon",
#>     title = "Fuel Efficiency vs Weight"
#>   )
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--pre`

Run code before the plot is built. Useful for data transformations:

``` bash
rush plot -n -x cyl -y mean_mpg --pre 'df <- df |> dplyr::summarise(mean_mpg = mean(mpg), .by = cyl)' -g col mtcars.csv
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
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> df <- dplyr::summarise(df, mean_mpg = mean(mpg), .by = cyl)
#> result <- ggplot(df, aes(x = cyl, y = mean_mpg)) + geom_col()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--post`

Run code after the plot is built. The plot object is available as `p`,
and the value of the last `--post` expression becomes the result. Use
this to add custom layers, themes, or annotations:

``` bash
rush plot -n -x wt -y mpg --post 'p + ggplot2::theme_minimal() + ggplot2::geom_smooth(method = "lm")' mtcars.csv
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
#>   has_post = TRUE
#> )
#> 
#> library(ggplot2)
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> p <- ggplot(df, aes(x = wt, y = mpg)) + geom_point()
#> result <- p + ggplot2::theme_minimal() + ggplot2::geom_smooth(method = "lm")
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### Saving to a file (`--output`)

When `--output` points to a file, the plot is saved (device inferred
from extension):

``` bash
rush plot -x wt -y mpg -o scatter.png mtcars.csv 2>/dev/null
ls -la scatter.png
#> -rw-r--r-- 1 jeroen staff 37929 Jul 25 16:32 scatter.png
```

### `--width`, `--height`, `--units`, `--dpi`

Control the saved image dimensions and resolution:

``` bash
rush plot -x wt -y mpg -o hires.png --width 10 --height 6 --units in --dpi 150 mtcars.csv 2>/dev/null
ls -la hires.png
#> -rw-r--r-- 1 jeroen staff 22569 Jul 25 16:32 hires.png
```

### Reading from stdin for plots

Without a file argument, `plot` reads from standard input:

``` bash
rush run 'head(mtcars, 10)' | rush plot -n -x wt -y mpg -
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
#> df <- janitor::clean_names(readr::read_delim(
#>   file("stdin", "rb", raw = TRUE),
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = wt, y = mpg)) + geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### Plotting multiple files

With multiple files, each is read into `dfs`. Use `--pre` to combine
them into `df` before plotting:

``` bash
echo "src,x,y
A,1,2
A,2,4
A,3,5" > a.csv
echo "src,x,y
B,1,3
B,2,2
B,3,6" > b.csv
rush plot -n -x x -y y -c src --pre 'df <- dplyr::bind_rows(dfs)' a.csv b.csv
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
#> dfs <- list()
#> dfs$a <- janitor::clean_names(readr::read_delim(
#>   "a.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> dfs$b <- janitor::clean_names(readr::read_delim(
#>   "b.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> df <- dplyr::bind_rows(dfs)
#> result <- ggplot(df, aes(x = x, y = y, color = src)) + geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### Plotting Parquet data

Parquet files work seamlessly:

``` bash
rush plot -n -x distance -y air_time -c carrier flights.parquet
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
#> #|   - nanoparquet
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
#> df <- janitor::clean_names(nanoparquet::read_parquet("flights.parquet"))
#> result <- ggplot(df, aes(x = distance, y = air_time, color = carrier)) +
#>   geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--tidyverse` with plot

Load the tidyverse for richer transformations in `--pre` or `--post`:

``` bash
rush plot -n -t -x cyl -y n --pre 'df <- count(df, cyl)' -g col mtcars.csv
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
#> #|   - tidyverse
#> #|   - glue
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
#> library(tidyverse)
#> library(glue)
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> df <- count(df, cyl)
#> result <- ggplot(df, aes(x = cyl, y = n)) + geom_col()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

### `--seed` with plot

Set the random seed when your plot involves sampling or jitter:

``` bash
rush plot -n --seed 123 -x cyl -y mpg -g jitter mtcars.csv
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
#> set.seed(123)
#> library(ggplot2)
#> df <- janitor::clean_names(readr::read_delim(
#>   "mtcars.csv",
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = cyl, y = mpg)) + geom_jitter()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

------------------------------------------------------------------------

## Combining commands in pipelines

The real power of `rush` is combining commands in Unix pipelines:

``` bash
# Query with SQL, then plot the result
rush sql "SELECT cyl, COUNT(*) as n, AVG(mpg) as avg_mpg FROM mtcars GROUP BY cyl" mtcars.csv | \
  rush plot -n -x cyl -y avg_mpg --size n -g point -
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
#> df <- janitor::clean_names(readr::read_delim(
#>   file("stdin", "rb", raw = TRUE),
#>   delim = ",",
#>   col_names = TRUE
#> ))
#> result <- ggplot(df, aes(x = cyl, y = avg_mpg, size = n)) + geom_point()
#> 
#> #~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> .has_tty <- isatty(stdout())
#> .stdout_binary <- function() {
#>   if (.Platform$OS.type == "windows") {
#>     file("stdout", "wb", raw = TRUE)
#>   } else {
#>     file("/dev/stdout", "wb", raw = TRUE)
#>   }
#> }
#> 
#> out <- .rush$output
#> w <- .rush$width
#> h <- .rush$height
#> if (is.null(out)) {
#>   out <- if (.has_tty) "ansi" else "png"
#> }
#> 
#> if (out %in% c("ansi", "ascii")) {
#>   if (is.null(w)) {
#>     w <- cli::console_width()
#>   }
#>   devoutansi::ansi(
#>     width = w,
#>     height = h,
#>     plain_ascii = TRUE,
#>     char_lookup_table = 2
#>   )
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
#>   if (is.null(w)) {
#>     w <- 6
#>   }
#>   if (is.null(h)) {
#>     h <- 4
#>   }
#>   ggplot2::ggsave(
#>     output_filename,
#>     result,
#>     device = device,
#>     width = w,
#>     height = h,
#>     units = .rush$units,
#>     dpi = .rush$dpi
#>   )
#>   if (cat_output) {
#>     contents <- readBin(output_filename, raw(), n = 1e8)
#>     writeBin(contents, .stdout_binary())
#>   }
#> }
```

``` bash
# Filter with rush run, convert to Parquet, query the Parquet
rush run 'dplyr::filter(df, mpg > 15)' mtcars.csv | \
  rush sql "SELECT cyl, ROUND(AVG(mpg),1) as avg FROM stdin GROUP BY cyl ORDER BY cyl" -
#> cyl,avg
#> 4,27.8
#> 6,19.8
#> 8,16.9
```
