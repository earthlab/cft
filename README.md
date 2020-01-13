
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cstdata: climate scenarios toolkit data utilities

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/earthlab/cstdata.svg?branch=master)](https://travis-ci.com/earthlab/cstdata)
[![Codecov test
coverage](https://codecov.io/gh/earthlab/cstdata/branch/master/graph/badge.svg)](https://codecov.io/gh/earthlab/cstdata?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of cstdata is to provide data access utilities that facilitate
climate scenario planning

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("earthlab/cstdata")
```

## Example

To acquire precipitation data for Acadia National Park for a subset of
climate models, you can use the `cstdata` function:

``` r
library(cstdata)
library(raster)

d <- cstdata(park = "Acadia National Park", parameters = "pr", 
             years = c(2020, 2021), models = "CCSM4", scenarios = "rcp85")
```
