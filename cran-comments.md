## Test environments
* macOS, local, R 4.5.1
* macOS, GitHub Actions, R release
* Windows, GitHub Actions, R release
* Ubuntu, GitHub Actions, R release
* Ubuntu, GitHub Actions, R devel
* Ubuntu, GitHub Actions, R oldrel-1

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

This NOTE is a transient network issue (DNS/NTP) and does not indicate
a problem with the package.

## Notes for reviewers

* This package requires the `ir` command-line tool (SystemRequirements) to
  execute generated scripts. All examples and tests use `dry_run = TRUE`
  which only prints the generated script, so they work without `ir` installed.

* Packages listed in Suggests (readr, nanoparquet, jsonlite, etc.) are
  runtime dependencies of the *generated scripts*, resolved by `ir` at
  execution time. They are not needed to install or test this package.

* This is a new submission.
