
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.timegpt

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fable.timegpt)](https://CRAN.R-project.org/package=fable.timegpt)
<!-- badges: end -->

The fable.timegpt package provides a fable compatible SDK for the
[Nixtla](https://www.nixtla.io/)’s [TimeGPT
API](https://docs.nixtla.io/).

## Installation

You can install the development version of fable.timegpt from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mitchelloharawild/fable.timegpt")
```

Using the TimeGPT API requires a token, you can request one at
<https://dashboard.nixtla.io/>. It is recommended that you store the
token as an R environment variable, which can be done by adding the line
`TIMEGPT_TOKEN=<your_token_here>` to the `.Renviron` file (which can be
opened with `usethis::edit_r_environ()`). Alternatively you can specify
the token directly in the `forecast()` function.

## Example

``` r
library(fable.timegpt)
#> Loading required package: fabletools
uad <- as_tsibble(USAccDeaths)
fc <- uad |> 
  model(TimeGPT(value)) |> 
  forecast(h = 10, level = c(80, 95))
fc
#> # A fable: 10 x 4 [1M]
#> # Key:     .model [1]
#>    .model            index         value .mean
#>    <chr>             <mth>        <dist> <dbl>
#>  1 TimeGPT(value) 1979 Jan percentile[5]    NA
#>  2 TimeGPT(value) 1979 Feb percentile[5]    NA
#>  3 TimeGPT(value) 1979 Mar percentile[5]    NA
#>  4 TimeGPT(value) 1979 Apr percentile[5]    NA
#>  5 TimeGPT(value) 1979 May percentile[5]    NA
#>  6 TimeGPT(value) 1979 Jun percentile[5]    NA
#>  7 TimeGPT(value) 1979 Jul percentile[5]    NA
#>  8 TimeGPT(value) 1979 Aug percentile[5]    NA
#>  9 TimeGPT(value) 1979 Sep percentile[5]    NA
#> 10 TimeGPT(value) 1979 Oct percentile[5]    NA
fc |> 
  autoplot(uad, point_forecast = list(median = median))
```

<img src="man/figures/README-example-1.png" width="100%" />
