
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cst: climate scenarios toolkit

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/earthlab/cst.svg?branch=master)](https://travis-ci.com/earthlab/cst)
[![codecov](https://codecov.io/gh/earthlab/cst/branch/master/graph/badge.svg)](https://codecov.io/gh/earthlab/cst)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of cst is to provide data access utilities that facilitate
climate scenario planning

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("earthlab/cst")
```

## Example

To acquire precipitation data for Acadia National Park for a subset of
climate models, you can use the `cstdata` function:

``` r
library(ggplot2)
library(cst)

d <- cstdata(park = "Acadia National Park", parameters = "pr", 
             years = c(2020, 2021), models = "CCSM4", scenarios = "rcp85")
#> [1] "Retrieving Area of Interest Boundaries"
#> [1] "Retrieving climate data for acadia_national_park"
#> [1] "Saving local files to /tmp/RtmpDEQX87/acadia_national_park"
```

This gives you a data frame with paths to local climate data files:

``` r
str(d)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    1 obs. of  13 variables:
#>  $ local_file    : chr "pr_acadia_national_park_CCSM4_r6i1p1_rcp85_macav2metdata_2020_2021_daily.nc"
#>  $ local_path    : chr "/tmp/RtmpDEQX87/acadia_national_park/pr_acadia_national_park_CCSM4_r6i1p1_rcp85_macav2metdata_2020_2021_daily.nc"
#>  $ aws_url       : logi NA
#>  $ model         : chr "CCSM4"
#>  $ parameter     : chr "pr"
#>  $ rcp           : chr "rcp85"
#>  $ ensemble      : chr "r6i1p1"
#>  $ year1         : num 2020
#>  $ year2         : num 2021
#>  $ area_name     : chr "acadia_national_park"
#>  $ units         : chr "mm"
#>  $ full_var_name : chr "Precipitation"
#>  $ parameter_long: chr "precipitation"
```

And, you can also summarize the daily data by computing a spatial
average over the region of interest:

``` r
df <- cst_df(d, cores = 2)
#> Computing spatial averages...
#> Generating climate data.frame...
str(df)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    731 obs. of  6 variables:
#>  $ rcp      : chr  "rcp85" "rcp85" "rcp85" "rcp85" ...
#>  $ date     : Date, format: "2020-01-01" "2020-01-02" ...
#>  $ model    : chr  "CCSM4" "CCSM4" "CCSM4" "CCSM4" ...
#>  $ ensemble : chr  "r6i1p1" "r6i1p1" "r6i1p1" "r6i1p1" ...
#>  $ area_name: chr  "acadia_national_park" "acadia_national_park" "acadia_national_park" "acadia_national_park" ...
#>  $ pr       : num  0 6.4 0 16.8 16.4 ...
```

Because this is a data.frame, you can use all of the normal data
visualization and processing functionality in base R\!

``` r
plot(x = df$date, 
     y = df$pr, 
     type = "b", 
     ylab = "Precipitation (mm)", 
     xlab = "Date", 
     main = "Acadia National Park, CCSM4, RCP 8.5")
```

<img src="man/figures/README-base-plot-1.png" width="100%" />

Or, if you prefer, you can use the tidyverse:

``` r
df %>%
  ggplot(aes(date, pr)) + 
  geom_point() + 
  geom_line() + 
  xlab("Date") + 
  ylab("Precipitation (mm)") + 
  ggtitle("Acadia National Park, CCSM4, RCP 8.5")
```

<img src="man/figures/README-ggplot-precip-1.png" width="100%" />

This is just a small glimpse at what you can do with the cst package.
For more, see [Getting started with the Climate Scenarios
Toolkit](vignettes/cst-intro.Rmd)

## Meta

  - Please [report any issues or
    bugs](https://github.com/earthlab/cst/issues), after reading our
    contribution [guidelines](CONTRIBUTING.md), and the [Contributor
    Code of Conduct](CONDUCT.md).
  - License: GPL-3
  - See `citation("smapr")` in R to cite this package in publications.
