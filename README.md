
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rush

<!-- badges: start -->

[![R-CMD-check](https://github.com/jeroenjanssens/rush/workflows/R-CMD-check/badge.svg)](https://github.com/jeroenjanssens/rush/actions)
<!-- badges: end -->

`rush` is an R package that allows you to run expressions, create plots,
and install packages directly from the shell.

## Installation

You can install the development version of `rush` with:

``` r
remotes::install_github("jeroenjanssens/rush")
```

## Examples

`rush` should be invoked from the command line. The executable is
located in the *exec* sub-directory of the package directory.

``` bash
./rush run 6*7
#> 42
```

Read from standard input:

``` bash
seq 6 | ./rush run -H '2 * sum(df$x1)' -
#> 42
```

Write to standard output:

``` bash
./rush run 'head(mtcars, 10)' | tee mtcars.csv
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

Show generated script with the `--dry-run` option:

``` bash
< mtcars.csv ./rush qplot --dry-run --x mpg --geom density --fill 'factor(cyl)'
#> #!/usr/bin/env Rscript
#> library(ggplot2)
#> df <- janitor::clean_names(readr::read_delim(file("stdin", "rb", raw = TRUE), delim = ",", col_names = TRUE))
#> qplot(x = mpg, margins = FALSE, geom = "density", fill = factor(cyl), data = df)
```

Create plots with the `qplot command`:

``` bash
< mtcars.csv ./rush qplot --x mpg --geom density --fill 'factor(cyl)' > ../man/figures/mtcars.png 
```

![](man/figures/mtcars.png)

## Help

``` bash
./rush -h
#> rush: R Scripting at the Command Line
#> 
#> Usage:
#>   rush [options] <command> [<args>]
#> 
#> Options:
#>   -n, --dry-run            Only print generated script.
#>   -h, --help               Show this help.
#>   -q, --quiet              Be quiet.
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
#> 
#> Commands:
#>   qplot
#>   run
#>   install
```

``` bash
./rush run -h
#> rush: Run an R expression
#> 
#> Usage:
#>   rush run [options] <expression> [--] [<file>...]
#> 
#> Reading options:
#>   -d, --delimiter <str>    Delimiter [default: ,].
#>   -C, --no-clean-names     No clean names.
#>   -H, --no-header          No header.
#> 
#> Run options:
#>   -l, --library <name>     Libraries to load.
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
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
```

``` bash
./rush qplot -h
#> rush: Quick plot
#> 
#> Usage:
#>   rush qplot [options] [--] [<file>|-]
#> 
#> Reading options:
#>   -d, --delimiter <str>    Delimiter [default: ,].
#>   -C, --no-clean-names     No clean names.
#>   -H, --no-header          No header.
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
#>       --main <str>         Plot title.
#>       --margins            Display marginal facets.
#>       --post <code>        Code to run after plotting.
#>       --pre <code>         Code to run before plotting.
#>       --shape <name>       Shape column.
#>       --size <name>        Size column.
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
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
```

``` bash
./rush install -h
#> rush: Install a package
#> 
#> Usage:
#>   rush install [options] <package>...
#> 
#> Install options:
#>   -u, --upgrade            Upgrade packages.
#> 
#> General options:
#>   -n, --dry-run            Only print generated script.
#>   -h, --help               Show this help.
#>   -q, --quiet              Be quiet.
#>   -v, --verbose            Be verbose.
#>       --version            Show version.
```

## Code of Conduct

Please note that the rush project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
