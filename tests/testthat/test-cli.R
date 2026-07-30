# The --dry-run flag prints the generated script instead of executing it,
# which makes it a convenient way to test script generation end to end. When
# the `air` formatter is on the PATH, `rush` runs the printed script through
# it, which reflows long lines and would make these assertions depend on
# whether air happens to be installed. Mask the PATH so generation tests
# always see the raw, unformatted script; air integration is covered
# separately below.
dry_run <- function(...) {
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = "")
  on.exit(Sys.setenv(PATH = old_path))
  utils::capture.output(rush(..., "-R"))
}

test_that("run without an expression or file errors gracefully", {
  expect_error(rush("run"), "No expression to run")
})

test_that("--no-ir parses to no_ir and keeps the frontmatter intact", {
  expect_true(parse_arguments("run", "-I", "1 + 1")$no_ir)
  expect_false(parse_arguments("run", "1 + 1")$no_ir)
  # The script is identical either way; only the executable that runs it
  # differs, so --no-ir still emits the ir shebang and package frontmatter,
  # keeping the saved script portable back to ir.
  script <- dry_run("run", "-n", "-I", "1 + 1")
  expect_equal(script[[1]], "#!/usr/bin/env -S ir run")
  expect_true(any(grepl("^#\\| packages:$", script)))
})

test_that("--dry-run runs the script through air when it is available", {
  skip_if(Sys.which("air") == "", "air is not installed")
  # A deliberately dense expression that air will reflow across lines.
  long <- paste0(
    "df |> dplyr::mutate(alpha = 1, beta = 2, gamma = 3, delta = 4, ",
    "epsilon = 5, zeta = 6, eta = 7, theta = 8)"
  )
  script <- utils::capture.output(rush("run", "-n", long, "data.csv"))
  # air keeps the script valid and preserves the ir shebang and frontmatter...
  expect_equal(script[[1]], "#!/usr/bin/env -S ir run")
  expect_true(any(grepl("^#\\| packages:$", script)))
  expect_silent(parse(text = script))
  # ...and reflows the long call onto multiple lines.
  expect_true(any(grepl("^\\s+alpha = 1,$", script)))
})

test_that("run generates an ir shebang, frontmatter, and reads the file", {
  script <- dry_run("run", "-n", "head(df)", "data.csv")
  expect_equal(script[[1]], "#!/usr/bin/env -S ir run")
  expect_true(any(grepl("^#\\| packages:$", script)))
  expect_true(any(grepl("^#\\|   - janitor$", script)))
  expect_true(any(grepl("read_delim", script)))
  expect_true(any(grepl("janitor::clean_names", script)))
  expect_true(any(grepl("result <- head\\(df\\)", script)))
})

test_that("--no-clean-names omits the janitor call", {
  script <- dry_run("run", "-n", "-C", "head(df)", "data.csv")
  expect_false(any(grepl("clean_names", script)))
})

test_that("--no-header reads without column names", {
  script <- dry_run("run", "-n", "-H", "head(df)", "data.csv")
  expect_true(any(grepl("col_names = FALSE", script)))
})

test_that("--no-header suppresses output header", {
  script <- dry_run("run", "-n", "-H", "head(df)", "data.csv")
  expect_true(any(grepl("output_header = FALSE", script)))
})

test_that("--no-input-header only affects reading", {
  script <- dry_run("run", "-n", "--no-input-header", "head(df)", "data.csv")
  expect_true(any(grepl("col_names = FALSE", script)))
  expect_true(any(grepl("output_header = TRUE", script)))
})

test_that("--no-output-header only affects writing", {
  script <- dry_run("run", "-n", "--no-output-header", "head(df)", "data.csv")
  expect_true(any(grepl("col_names = TRUE", script)))
  expect_true(any(grepl("output_header = FALSE", script)))
})

test_that("--names provides column names and implies no input header", {
  script <- dry_run("run", "-n", "--names", "a,b,c", "head(df)", "data.csv")
  expect_true(any(grepl('col_names = c\\("a", "b", "c"\\)', script)))
  expect_true(any(grepl("output_header = TRUE", script)))
})

test_that("--names with --no-output-header suppresses output header", {
  script <- dry_run(
    "run",
    "-n",
    "--names",
    "x,y",
    "--no-output-header",
    "df",
    "data.csv"
  )
  expect_true(any(grepl('col_names = c\\("x", "y"\\)', script)))
  expect_true(any(grepl("output_header = FALSE", script)))
})

test_that("--seed emits set.seed before other code", {
  script <- dry_run("run", "-n", "--seed", "7", "1 + 1", "data.csv")
  expect_true(any(grepl("set.seed\\(7\\)", script)))
  expect_lt(grep("set.seed", script), grep("read_delim", script))
})

test_that("libraries are loaded", {
  script <- dry_run("run", "-n", "-l", "stringr", "head(df)", "data.csv")
  expect_true(any(grepl("^library\\(stringr\\)$", script)))
})

test_that("--tidyverse loads tidyverse and glue", {
  script <- dry_run("run", "-n", "-t", "head(df)", "data.csv")
  expect_true(any(grepl("^library\\(tidyverse\\)$", script)))
  expect_true(any(grepl("^library\\(glue\\)$", script)))
})

test_that("stdin is read from a binary connection", {
  script <- dry_run("run", "-n", "head(df)", "-")
  expect_true(any(grepl("stdin", script)))
})

test_that("single file always creates both df and dfs", {
  script <- dry_run("run", "-n", "head(df)", "data.csv")
  expect_true(any(grepl("^dfs <- list\\(\\)$", script)))
  expect_true(any(grepl('dfs\\[\\["data"\\]\\] <-', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("stdin creates dfs$stdin entry", {
  script <- dry_run("run", "-n", "head(df)", "-")
  expect_true(any(grepl("^dfs <- list\\(\\)$", script)))
  expect_true(any(grepl('dfs\\[\\["stdin"\\]\\] <-', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("run reads multiple files into a dfs list", {
  script <- dry_run("run", "-n", "nrow(dfs$a)", "a.csv", "b.csv")
  expect_true(any(grepl("^dfs <- list\\(\\)$", script)))
  expect_true(any(grepl(
    'dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"',
    script
  )))
  expect_true(any(grepl(
    'dfs\\[\\["b"\\]\\] <- .*read_delim\\("b.csv"',
    script
  )))
})

test_that("run reads a Parquet file with nanoparquet", {
  script <- dry_run("run", "-n", "head(df)", "data.parquet")
  expect_true(any(grepl(
    "nanoparquet::read_parquet\\(\"data.parquet\"",
    script
  )))
  expect_true(any(grepl("^#\\|   - nanoparquet$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("run writes a Parquet file when --output ends in .parquet", {
  script <- dry_run("run", "-n", "-o", "out.parquet", "df", "data.csv")
  expect_true(any(grepl('output_format = "parquet"', script)))
  expect_true(any(grepl("nanoparquet::write_parquet", script)))
  expect_true(any(grepl("^#\\|   - nanoparquet$", script)))
})

test_that("run reads a DuckDB database into a dfs list", {
  script <- dry_run("run", "-n", "nrow(df)", "mydb.duckdb")
  expect_true(any(grepl(
    "dbConnect\\(duckdb::duckdb\\(\\), dbdir = \"mydb.duckdb\", read_only = TRUE\\)",
    script
  )))
  expect_true(any(grepl("dbListTables", script)))
  expect_true(any(grepl("dbReadTable", script)))
  expect_true(any(grepl('dfs\\[\\["mydb"\\]\\] <- list\\(\\)', script)))
  expect_true(any(grepl('dfs\\[\\["mydb"\\]\\]\\[\\[.t\\]\\]', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
  expect_true(any(grepl("^#\\|   - duckdb$", script)))
  expect_true(any(grepl("^#\\|   - DBI$", script)))
})

test_that("sql without a query errors gracefully", {
  expect_error(rush("sql"), "No query to run")
})

test_that("sql registers files as relations and runs the query", {
  script <- dry_run(
    "sql",
    "-n",
    "SELECT * FROM a JOIN b USING (x)",
    "a.csv",
    "b.parquet",
    "w.duckdb",
    "-"
  )
  expect_true(any(grepl(
    "con <- DBI::dbConnect\\(duckdb::duckdb\\(\\)\\)",
    script
  )))
  expect_true(any(grepl(
    "CREATE VIEW ..a.. AS SELECT .* FROM read_csv_auto",
    script
  )))
  expect_true(any(grepl(
    "CREATE VIEW ..b.. AS SELECT .* FROM read_parquet",
    script
  )))
  expect_true(any(grepl("ATTACH .* AS ..w.. .READ_ONLY.", script)))
  expect_true(any(grepl("\\.stdin_tmp <- tempfile", script)))
  expect_true(any(grepl(
    'CREATE VIEW .{1,2}stdin.{1,2} AS SELECT .* FROM read_csv_auto',
    script
  )))
  expect_true(any(grepl(
    "result <- DBI::dbGetQuery\\(con, \"SELECT \\* FROM a JOIN b USING \\(x\\)\"\\)",
    script
  )))
})

test_that("run with digit-prefixed file names generates parseable code", {
  script <- dry_run("run", "-n", "head(df)", "2024.csv")
  expect_silent(parse(text = script))
})

test_that("multi-file digit-prefixed names use valid dfs[[]] indexing", {
  script <- dry_run("run", "-n", "nrow(dfs)", "2024.csv", "2025.csv")
  expect_true(any(grepl('dfs\\[\\["x2024"\\]\\]', script)))
  expect_true(any(grepl('dfs\\[\\["x2025"\\]\\]', script)))
  expect_silent(parse(text = script))
})

test_that("sql with digit-prefixed file emits a quoted relation", {
  script <- dry_run("sql", "-n", "SELECT 1", "2024.csv")
  expect_true(any(grepl("CREATE VIEW ..x2024..", script)))
  expect_silent(parse(text = script))
})

test_that("sql emits on.exit disconnect", {
  script <- dry_run("sql", "-n", "SELECT 1", "a.csv")
  expect_true(any(grepl("on\\.exit\\(DBI::dbDisconnect", script)))
})

test_that("plot reads a single file into df", {
  script <- dry_run("plot", "-n", "-x", "wt", "a.csv")
  expect_true(any(grepl(
    'dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"',
    script
  )))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("plot defaults to reading df from stdin", {
  script <- dry_run("plot", "-n", "-x", "wt")
  expect_true(any(grepl('dfs\\[\\["stdin"\\]\\] <- .*stdin', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("plot reads multiple files into a dfs list", {
  script <- dry_run(
    "plot",
    "-n",
    "-x",
    "wt",
    "--pre",
    "df <- dplyr::bind_rows(dfs)",
    "a.csv",
    "b.csv"
  )
  expect_true(any(grepl("^dfs <- list\\(\\)$", script)))
  expect_true(any(grepl(
    'dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"',
    script
  )))
  expect_true(any(grepl(
    'dfs\\[\\["b"\\]\\] <- .*read_delim\\("b.csv"',
    script
  )))
  expect_true(any(grepl("df <- dplyr::bind_rows\\(dfs\\)", script)))
})

test_that("plot frontmatter injects the terminal-plotting GitHub packages", {
  script <- dry_run("plot", "-n", "-x", "wt", "mtcars.csv")
  expect_true(any(grepl("^#\\|   - github::coolbutuseless/devout$", script)))
  expect_true(any(grepl("^#\\|   - github::jeroenjanssens/miniansi$", script)))
  expect_true(any(grepl(
    "^#\\|   - github::coolbutuseless/devoutansi$",
    script
  )))
})

test_that("plot generates a ggplot call with aesthetics", {
  script <- dry_run("plot", "-n", "-x", "wt", "-y", "mpg", "mtcars.csv")
  expect_true(any(grepl("^library\\(ggplot2\\)$", script)))
  ggplot_line <- script[grepl("ggplot\\(", script)]
  expect_length(ggplot_line, 1)
  expect_match(ggplot_line, "ggplot\\(df, aes\\(")
  expect_match(ggplot_line, "x = wt")
  expect_match(ggplot_line, "y = mpg")
})

test_that("plot guesses geom_point when a y column is given", {
  script <- dry_run("plot", "-n", "-x", "wt", "-y", "mpg", "mtcars.csv")
  expect_true(any(grepl("geom_point\\(\\)", script)))
})

test_that("plot guesses geom_histogram when only x is given", {
  script <- dry_run("plot", "-n", "-x", "wt", "mtcars.csv")
  expect_true(any(grepl("geom_histogram\\(\\)", script)))
})

test_that("plot --geom overrides the guessed geom", {
  script <- dry_run(
    "plot",
    "-n",
    "-x",
    "wt",
    "-y",
    "mpg",
    "-g",
    "line",
    "mtcars.csv"
  )
  expect_true(any(grepl("geom_line\\(\\)", script)))
  expect_false(any(grepl("geom_point", script)))
})

test_that("plot --log rejects invalid values", {
  expect_error(
    rush("plot", "-n", "-x", "wt", "--log", "z", "mtcars.csv"),
    "--log.*must be one of"
  )
})

test_that("plot --log adds log scales for the requested axes", {
  script <- dry_run(
    "plot",
    "-n",
    "-x",
    "wt",
    "-y",
    "mpg",
    "--log",
    "xy",
    "mtcars.csv"
  )
  expect_true(any(grepl("scale_x_log10\\(\\)", script)))
  expect_true(any(grepl("scale_y_log10\\(\\)", script)))
})

test_that("plot facets: two-sided formula uses facet_grid, one-sided facet_wrap", {
  grid <- dry_run(
    "plot",
    "-n",
    "-x",
    "wt",
    "-y",
    "mpg",
    "--facets",
    "gear ~ cyl",
    "--margins",
    "mtcars.csv"
  )
  expect_true(any(grepl("facet_grid\\(gear ~ cyl, margins = TRUE\\)", grid)))

  wrap <- dry_run(
    "plot",
    "-n",
    "-x",
    "wt",
    "-y",
    "mpg",
    "--facets",
    "~ cyl",
    "mtcars.csv"
  )
  expect_true(any(grepl("facet_wrap\\(~cyl\\)", wrap)))
})

test_that("plot --title, --xlab, and --ylab become a labs() layer", {
  script <- dry_run(
    "plot",
    "-n",
    "-x",
    "wt",
    "--title",
    "Cars",
    "--xlab",
    "Weight",
    "--ylab",
    "MPG",
    "mtcars.csv"
  )
  labs_line <- script[grepl("labs\\(", script)]
  expect_length(labs_line, 1)
  expect_match(labs_line, 'title = "Cars"')
  expect_match(labs_line, 'x = "Weight"')
  expect_match(labs_line, 'y = "MPG"')
})

test_that("-d sets both input and output delimiters", {
  script <- dry_run("run", "-n", "-d", "\t", "head(df)", "data.csv")
  # Input side: read_delim uses tab
  expect_true(any(grepl('delim = "\\\\t"', script)))
  # Output side: .rush$delimiter is tab
  expect_true(any(grepl('delimiter = "\\\\t"', script)))
})

test_that("--output-delimiter overrides only the output delimiter", {
  script <- dry_run("run", "-n", "-D", "\t", "head(df)", "data.csv")
  # Input side still uses comma (default delimiter)
  read_line <- script[grepl("read_delim", script)]
  expect_true(any(grepl('delim = ","', read_line)))
  # Output side uses tab
  expect_true(any(grepl('delimiter = "\\\\t"', script)))
})

test_that("--input-delimiter overrides only the input delimiter", {
  script <- dry_run(
    "run",
    "-n",
    "--input-delimiter",
    "\t",
    "head(df)",
    "data.csv"
  )
  # Input side uses tab
  read_line <- script[grepl("read_delim", script)]
  expect_true(any(grepl('delim = "\\\\t"', read_line)))
  # Output side uses default comma
  expect_true(any(grepl('delimiter = ","', script)))
})

test_that("-F tsv implies tab as input delimiter", {
  script <- dry_run("run", "-n", "-F", "tsv", "head(df)", "data.tsv")
  read_line <- script[grepl("read_delim", script)]
  expect_true(any(grepl('delim = "\\\\t"', read_line)))
})

test_that("-O tsv implies tab as output delimiter", {
  script <- dry_run("run", "-n", "-O", "tsv", "head(df)", "data.csv")
  expect_true(any(grepl('delimiter = "\\\\t"', script)))
})

test_that("--head adds a head() call in the dispatch block", {
  script <- dry_run("run", "-n", "--head", "3", "head(df)", "data.csv")
  expect_true(any(grepl('\\.rush\\$head', script)))
  expect_true(any(grepl('head = 3L', script)))
})

test_that("-O json sets output_format to json in preamble", {
  script <- dry_run("run", "-n", "-O", "json", "df", "data.csv")
  expect_true(any(grepl('output_format = "json"', script)))
})

test_that("-O parquet works like --output ending in .parquet", {
  script <- dry_run(
    "run",
    "-n",
    "-O",
    "parquet",
    "-o",
    "out.parquet",
    "df",
    "data.csv"
  )
  expect_true(any(grepl('output_format = "parquet"', script)))
  expect_true(any(grepl("nanoparquet::write_parquet", script)))
  expect_true(any(grepl("^#\\|   - nanoparquet$", script)))
})

test_that("sql reads JSON files via read_json_auto", {
  script <- dry_run("sql", "-n", "SELECT * FROM data", "data.json")
  expect_true(any(grepl("read_json_auto", script)))
})

test_that("sql reads JSONL files via read_json_auto", {
  script <- dry_run("sql", "-n", "SELECT * FROM data", "data.jsonl")
  expect_true(any(grepl("read_json_auto", script)))
})

test_that("convert without input errors", {
  expect_error(rush("convert", "-o", "out.parquet"), "No input file to convert")
})

test_that("convert without output or format errors", {
  expect_error(rush("convert", "data.csv"), "No output format specified")
})

test_that("convert to stdout with -O works", {
  script <- dry_run("convert", "-n", "-O", "json", "data.csv")
  expect_true(any(grepl("read_delim", script)))
  expect_true(any(grepl("result <- df", script)))
  expect_true(any(grepl('output_format = "json"', script)))
  expect_true(any(grepl("jsonlite::toJSON", script)))
})

test_that("convert csv to parquet generates correct script", {
  script <- dry_run("convert", "-n", "-o", "out.parquet", "data.csv")
  expect_true(any(grepl("read_delim\\(\"data.csv\"", script)))
  expect_true(any(grepl("result <- df", script)))
  expect_true(any(grepl('output_format = "parquet"', script)))
  expect_true(any(grepl("nanoparquet::write_parquet", script)))
})

test_that("convert json to csv generates correct script", {
  script <- dry_run("convert", "-n", "-o", "out.csv", "data.json")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
  expect_true(any(grepl("result <- df", script)))
  expect_true(any(grepl("readr::write_delim", script)))
})

test_that("convert with multiple input files requires a template", {
  expect_error(
    rush("convert", "-n", "-o", "out.parquet", "a.csv", "b.csv"),
    "template"
  )
})

test_that("convert with output template emits result <- dfs", {
  script <- dry_run(
    "convert",
    "-n",
    "-o",
    "%(file_name)s.parquet",
    "a.csv",
    "b.csv"
  )
  expect_true(any(grepl("dfs <- list", script)))
  expect_true(any(grepl("result <- dfs", script)))
  expect_true(any(grepl("output_template", script)))
})

test_that("convert respects -F for input format override", {
  script <- dry_run("convert", "-n", "-F", "json", "-o", "out.csv", "data.txt")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
})

test_that("convert respects -O for output format override", {
  script <- dry_run("convert", "-n", "-O", "jsonl", "-o", "out.txt", "data.csv")
  expect_true(any(grepl('output_format = "jsonl"', script)))
  expect_true(any(grepl("jsonlite::stream_out", script)))
})

test_that("convert parquet to xlsx", {
  script <- dry_run("convert", "-n", "-o", "out.xlsx", "data.parquet")
  expect_true(any(grepl("nanoparquet::read_parquet", script)))
  expect_true(any(grepl("writexl::write_xlsx", script)))
})

test_that("convert applies --head to limit rows", {
  script <- dry_run(
    "convert",
    "-n",
    "--head",
    "10",
    "-o",
    "out.csv",
    "data.parquet"
  )
  expect_true(any(grepl("head = 10L", script)))
})

test_that("plot --pre and --post wrap the plot call", {
  script <- dry_run(
    "plot",
    "-n",
    "-x",
    "wt",
    "--pre",
    "df <- head(df)",
    "--post",
    "p + theme_bw()",
    "mtcars.csv"
  )
  expect_true(any(grepl("p <- ggplot", script)))
  expect_true(any(grepl("theme_bw", script)))
  # pre runs before the plot, post after
  ggplot_line <- grep("p <- ggplot", script)
  expect_lt(grep("head\\(df\\)", script)[[1]], ggplot_line)
  expect_gt(grep("theme_bw", script), ggplot_line)
})

# Output templates ------------------------------------------------------------

test_that("output template is detected and stored in preamble", {
  script <- dry_run("convert", "-n", "-o", "%(file_name)s.parquet", "a.csv")
  expect_true(any(grepl(
    'output_template = "%\\(file_name\\)s.parquet"',
    script
  )))
  expect_true(any(grepl("output = NULL", script)))
})

test_that("template dispatch includes .expand_template helper", {
  script <- dry_run("convert", "-n", "-o", "%(file_name)s.csv", "a.parquet")
  expect_true(any(grepl("\\.expand_template", script)))
  expect_true(any(grepl("\\.expand_field", script)))
  expect_true(any(grepl("\\.write_result", script)))
})

test_that("convert multi-file with template sets result <- dfs", {
  script <- dry_run(
    "convert",
    "-n",
    "-o",
    "out_%(file_index)d.csv",
    "a.csv",
    "b.csv"
  )
  expect_true(any(grepl("result <- dfs", script)))
})

test_that("convert multi-file without template errors", {
  expect_error(
    rush("convert", "-n", "-o", "out.csv", "a.csv", "b.csv"),
    "template"
  )
})

test_that("convert single database without template errors", {
  expect_error(
    rush("convert", "-n", "-o", "out.csv", "data.duckdb"),
    "template"
  )
})

# DuckDB write -----------------------------------------------------------------

test_that("-O duckdb emits DuckDB write via DBI", {
  script <- dry_run(
    "run",
    "-n",
    "-O",
    "duckdb",
    "-o",
    "out.duckdb",
    "df",
    "data.csv"
  )
  expect_true(any(grepl('output_format = "duckdb"', script)))
  expect_true(any(grepl("duckdb::duckdb\\(\\)", script)))
  expect_true(any(grepl("dbWriteTable", script)))
  expect_true(any(grepl("^#\\|   - duckdb$", script)))
  expect_true(any(grepl("^#\\|   - DBI$", script)))
})

test_that("convert csv to duckdb", {
  script <- dry_run("convert", "-n", "-o", "out.duckdb", "data.csv")
  expect_true(any(grepl('output_format = "duckdb"', script)))
  expect_true(any(grepl("duckdb::duckdb\\(\\)", script)))
  expect_true(any(grepl("dbWriteTable", script)))
})

test_that("convert .ddb extension resolves to duckdb format", {
  script <- dry_run("convert", "-n", "-o", "out.ddb", "data.csv")
  expect_true(any(grepl('output_format = "duckdb"', script)))
})

test_that("convert multiple csvs to duckdb writes multi-table", {
  script <- dry_run("convert", "-n", "-o", "combined.duckdb", "a.csv", "b.csv")
  expect_true(any(grepl('output_format = "duckdb"', script)))
  expect_true(any(grepl("result <- dfs", script)))
  expect_true(any(grepl("for \\(.tbl_name in names\\(result\\)\\)", script)))
  expect_true(any(grepl("dbWriteTable\\(.con, .tbl_name", script)))
})

test_that("convert multiple csvs to sqlite writes multi-table", {
  script <- dry_run("convert", "-n", "-o", "combined.sqlite", "a.csv", "b.csv")
  expect_true(any(grepl('output_format = "sqlite"', script)))
  expect_true(any(grepl("result <- dfs", script)))
  expect_true(any(grepl("for \\(.tbl_name in names\\(result\\)\\)", script)))
})

test_that("convert single csv to duckdb uses file name as table name", {
  script <- dry_run("convert", "-n", "-o", "out.duckdb", "sales.csv")
  expect_true(any(grepl("result <- dfs", script)))
  expect_true(any(grepl("for \\(.tbl_name in names\\(result\\)\\)", script)))
})

test_that("rush run with -O duckdb writes data frame as 'data' table", {
  script <- dry_run(
    "run",
    "-n",
    "-O",
    "duckdb",
    "-o",
    "out.duckdb",
    "df",
    "data.csv"
  )
  expect_true(any(grepl("result <- df", script)))
  expect_true(any(grepl('dbWriteTable\\(.con, "data", result\\)', script)))
})

# Plot with database input ----------------------------------------------------

test_that("plot with database input auto-selects first table", {
  script <- dry_run("plot", "-n", "-x", "wt", "data.duckdb")
  expect_true(any(grepl("if \\(is.list\\(df\\)", script)))
  expect_true(any(grepl("df <- df\\[\\[1L\\]\\]", script)))
})

# JSON/JSONL stdin -------------------------------------------------------------

test_that("run -F json with stdin uses file('stdin') connection", {
  script <- dry_run("run", "-n", "-F", "json", "head(df)", "-")
  expect_true(any(grepl('fromJSON\\(file\\("stdin"\\)\\)', script)))
})

test_that("run -F jsonl with stdin uses file('stdin') connection", {
  script <- dry_run("run", "-n", "-F", "jsonl", "head(df)", "-")
  expect_true(any(grepl('stream_in\\(file\\("stdin"\\)', script)))
})

# SQL with JSON extension ------------------------------------------------------

test_that("sql with .json file emits INSTALL/LOAD json", {
  script <- dry_run("sql", "-n", "SELECT * FROM data", "data.json")
  expect_true(any(grepl("INSTALL json; LOAD json", script)))
  expect_true(any(grepl("read_json_auto", script)))
})

test_that("sql with .jsonl file emits INSTALL/LOAD json", {
  script <- dry_run("sql", "-n", "SELECT * FROM events", "events.jsonl")
  expect_true(any(grepl("INSTALL json; LOAD json", script)))
  expect_true(any(grepl("read_json_auto", script)))
})

test_that("sql without json files does not emit INSTALL json", {
  script <- dry_run("sql", "-n", "SELECT * FROM data", "data.csv")
  expect_false(any(grepl("INSTALL json", script)))
})
