
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{mappingEditr}`

<!-- badges: start -->
<!-- badges: end -->

This app is meant to create a friendly interface to edit master lookup
tables in JSON format. The JSON format is standardized so it can be
leveraged to automate the mapping process in data pipelines that reads
data from non SDMX sources and format them into SDMX

## Installation

You can install the development version of `{mappingEditr}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
mappingEditr::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2024-11-18 11:50:03 CET"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading mappingEditr
#> ── R CMD check results ──────────────────────────── mappingEditr 0.0.0.9000 ────
#> Duration: 57.7s
#> 
#> ❯ checking tests ...
#>   See below...
#> 
#> ❯ checking dependencies in R code ... WARNING
#>   '::' or ':::' imports not declared from:
#>     'DT' 'bslib' 'httr' 'jsonlite'
#> 
#> ❯ checking for unstated dependencies in 'tests' ... WARNING
#>   Warning: unable to access index for repository https://CRAN.R-project.org/src/contrib:
#>     cannot open URL 'https://CRAN.R-project.org/src/contrib/PACKAGES'
#>   Warning: unable to access index for repository https://bioconductor.org/packages/3.19/bioc/src/contrib:
#>     cannot open URL 'https://bioconductor.org/packages/3.19/bioc/src/contrib/PACKAGES'
#>   Warning: unable to access index for repository https://bioconductor.org/packages/3.19/data/annotation/src/contrib:
#>     cannot open URL 'https://bioconductor.org/packages/3.19/data/annotation/src/contrib/PACKAGES'
#>   Warning: unable to access index for repository https://bioconductor.org/packages/3.19/data/experiment/src/contrib:
#>     cannot open URL 'https://bioconductor.org/packages/3.19/data/experiment/src/contrib/PACKAGES'
#>   '::' or ':::' imports not declared from:
#>     'jsonlite' 'testthat'
#>   'library' or 'require' calls not declared from:
#>     'jsonlite' 'testthat'
#> 
#> ❯ checking DESCRIPTION meta-information ... NOTE
#>   Malformed Description field: should contain one or more complete sentences.
#> 
#> ❯ checking R code for possible problems ... [10s] NOTE
#>   fetch_json: no visible global function definition for 'fromJSON'
#>   Undefined global functions or variables:
#>     fromJSON
#> 
#> ── Test failures ───────────────────────────────────────────────── testthat ────
#> 
#> > # This file is part of the standard setup for testthat.
#> > # It is recommended that you do not modify it.
#> > #
#> > # Where should you do additional test configuration?
#> > # Learn more about the roles of various files in:
#> > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#> > # * https://testthat.r-lib.org/articles/special-files.html
#> > 
#> > library(testthat)
#> Error in library(testthat) : there is no package called 'testthat'
#> Execution halted
#> 
#> 1 error ✖ | 2 warnings ✖ | 2 notes ✖
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error: Failure in `C:/Users/WB499754/AppData/Local/Temp/RtmpAfKWcY/R_LIBS670044ae3d85/mappingEditr/mappingEditr-tests/testthat.Rout.fail`
```
