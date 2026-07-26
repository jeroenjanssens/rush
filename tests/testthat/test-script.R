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
  utils::capture.output(rush(...))
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

test_that("run reads multiple files into a dfs list", {
  script <- dry_run("run", "-n", "nrow(dfs$a)", "a.csv", "b.csv")
  expect_true(any(grepl("^dfs <- list\\(\\)$", script)))
  expect_true(any(grepl('dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"', script)))
  expect_true(any(grepl('dfs\\[\\["b"\\]\\] <- .*read_delim\\("b.csv"', script)))
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
  expect_true(any(grepl(
    "if \\(length\\(dfs\\) == 1\\) df <- dfs\\[\\[1\\]\\]",
    script
  )))
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
    "CREATE VIEW .stdin. AS SELECT .* FROM read_csv_auto",
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
  expect_true(any(grepl("^df <- .*read_delim\\(\"a.csv\"", script)))
})

test_that("plot defaults to reading df from stdin", {
  script <- dry_run("plot", "-n", "-x", "wt")
  expect_true(any(grepl("^df <- .*stdin", script)))
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
  expect_true(any(grepl('dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"', script)))
  expect_true(any(grepl('dfs\\[\\["b"\\]\\] <- .*read_delim\\("b.csv"', script)))
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
  script <- dry_run("run", "-n", "--input-delimiter", "\t", "head(df)", "data.csv")
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
  script <- dry_run("run", "-n", "-O", "parquet", "-o", "out.parquet", "df", "data.csv")
  expect_true(any(grepl('output_format = "parquet"', script)))
  expect_true(any(grepl("nanoparquet::write_parquet", script)))
  expect_true(any(grepl("^#\\|   - nanoparquet$", script)))
})

test_that("run reads a JSON file with jsonlite", {
  script <- dry_run("run", "-n", "head(df)", "data.json")
  expect_true(any(grepl("jsonlite::fromJSON\\(\"data.json\"\\)", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("run reads a JSONL file with jsonlite stream_in", {
  script <- dry_run("run", "-n", "head(df)", "data.jsonl")
  expect_true(any(grepl("jsonlite::stream_in\\(file\\(\"data.jsonl\"\\)", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
})

test_that("run reads an .ndjson file as JSONL", {
  script <- dry_run("run", "-n", "head(df)", "data.ndjson")
  expect_true(any(grepl("jsonlite::stream_in\\(file\\(\"data.ndjson\"\\)", script)))
})

test_that("-F json forces JSON reading regardless of extension", {
  script <- dry_run("run", "-n", "-F", "json", "head(df)", "data.csv")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("-F jsonl forces JSONL reading", {
  script <- dry_run("run", "-n", "-F", "jsonl", "head(df)", "data.csv")
  expect_true(any(grepl("jsonlite::stream_in", script)))
})

test_that("-O json emits JSON output via jsonlite", {
  script <- dry_run("run", "-n", "-O", "json", "df", "data.csv")
  expect_true(any(grepl('output_format = "json"', script)))
  expect_true(any(grepl("jsonlite::toJSON", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
})

test_that("-O jsonl emits JSONL streaming output", {
  script <- dry_run("run", "-n", "-O", "jsonl", "df", "data.csv")
  expect_true(any(grepl('output_format = "jsonl"', script)))
  expect_true(any(grepl("jsonlite::stream_out", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
})

test_that("--output data.json infers JSON format", {
  script <- dry_run("run", "-n", "-o", "data.json", "df", "data.csv")
  expect_true(any(grepl('output_format = "json"', script)))
  expect_true(any(grepl("jsonlite::toJSON", script)))
})

test_that("--output data.jsonl infers JSONL format", {
  script <- dry_run("run", "-n", "-o", "data.jsonl", "df", "data.csv")
  expect_true(any(grepl('output_format = "jsonl"', script)))
  expect_true(any(grepl("jsonlite::stream_out", script)))
})

test_that("run reads an Excel file with readxl", {
  script <- dry_run("run", "-n", "head(df)", "data.xlsx")
  expect_true(any(grepl("readxl::read_excel\\(\"data.xlsx\"\\)", script)))
  expect_true(any(grepl("^#\\|   - readxl$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("run reads .xls files with readxl", {
  script <- dry_run("run", "-n", "head(df)", "data.xls")
  expect_true(any(grepl("readxl::read_excel\\(\"data.xls\"\\)", script)))
  expect_true(any(grepl("^#\\|   - readxl$", script)))
})

test_that("--sheet selects a specific Excel sheet", {
  script <- dry_run("run", "-n", "--sheet", "Sales", "head(df)", "data.xlsx")
  expect_true(any(grepl('readxl::read_excel\\("data.xlsx", sheet = "Sales"\\)', script)))
})

test_that("--sheet with numeric index works", {
  script <- dry_run("run", "-n", "--sheet", "2", "head(df)", "data.xlsx")
  expect_true(any(grepl('readxl::read_excel\\("data.xlsx", sheet = 2\\)', script)))
})

test_that("-F xlsx forces Excel reading", {
  script <- dry_run("run", "-n", "-F", "xlsx", "head(df)", "data.csv")
  expect_true(any(grepl("readxl::read_excel", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("-O xlsx emits Excel output via writexl", {
  script <- dry_run("run", "-n", "-O", "xlsx", "-o", "out.xlsx", "df", "data.csv")
  expect_true(any(grepl('output_format = "xlsx"', script)))
  expect_true(any(grepl("writexl::write_xlsx", script)))
  expect_true(any(grepl("^#\\|   - writexl$", script)))
})

test_that("--output out.xlsx infers Excel format", {
  script <- dry_run("run", "-n", "-o", "out.xlsx", "df", "data.csv")
  expect_true(any(grepl('output_format = "xlsx"', script)))
  expect_true(any(grepl("writexl::write_xlsx", script)))
})

test_that("run reads Arrow IPC file with arrow package", {
  script <- dry_run("run", "-n", "head(df)", "data.arrow")
  expect_true(any(grepl("arrow::read_ipc_file\\(\"data.arrow\"\\)", script)))
  expect_true(any(grepl("^#\\|   - arrow$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("run reads .feather files as Arrow IPC", {
  script <- dry_run("run", "-n", "head(df)", "data.feather")
  expect_true(any(grepl("arrow::read_ipc_file\\(\"data.feather\"\\)", script)))
})

test_that("run reads .ipc files as Arrow IPC", {
  script <- dry_run("run", "-n", "head(df)", "data.ipc")
  expect_true(any(grepl("arrow::read_ipc_file\\(\"data.ipc\"\\)", script)))
})

test_that("-F arrow forces Arrow reading", {
  script <- dry_run("run", "-n", "-F", "arrow", "head(df)", "data.csv")
  expect_true(any(grepl("arrow::read_ipc_file", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("-O arrow emits Arrow IPC output", {
  script <- dry_run("run", "-n", "-O", "arrow", "-o", "out.arrow", "df", "data.csv")
  expect_true(any(grepl('output_format = "arrow"', script)))
  expect_true(any(grepl("arrow::write_ipc_file", script)))
  expect_true(any(grepl("^#\\|   - arrow$", script)))
})

test_that("--output out.feather infers Arrow format", {
  script <- dry_run("run", "-n", "-o", "out.feather", "df", "data.csv")
  expect_true(any(grepl('output_format = "arrow"', script)))
  expect_true(any(grepl("arrow::write_ipc_file", script)))
})

test_that("sql reads JSON files via read_json_auto", {
  script <- dry_run("sql", "-n", "SELECT * FROM data", "data.json")
  expect_true(any(grepl("read_json_auto", script)))
})

test_that("sql reads JSONL files via read_json_auto", {
  script <- dry_run("sql", "-n", "SELECT * FROM data", "data.jsonl")
  expect_true(any(grepl("read_json_auto", script)))
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
