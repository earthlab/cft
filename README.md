---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# cst: climate scenarios toolbox

<!-- badges: start -->
[![Build Status](https://travis-ci.com/earthlab/cst.svg?branch=master)](https://travis-ci.com/earthlab/cst)
[![codecov](https://codecov.io/gh/earthlab/cst/branch/master/graph/badge.svg)](https://codecov.io/gh/earthlab/cst)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build status](https://github.com/earthlab/cst/workflows/R-CMD-check/badge.svg)](https://github.com/earthlab/cst/actions)
[![R build status](https://github.com/earthlab/cst/workflows/pkgdown/badge.svg)](https://github.com/earthlab/cst/actions)
<!-- badges: end -->

The goal of cst is to provide easy climate data access 
([MACA v2](http://www.climatologylab.org/maca.html)) to support 
climate scenario planning.
This package allows you to: 

1. Quickly acquire climate data subsets for a spatial region of interest, with
first class support for US National Parks
2. Summarize climate data at daily timesteps, and compute derived quantities
3. Contrast reference and target time periods to understand differences in 
climate over time, and
4. Easily work with climate data, without having to worry about the details of 
how it is stored or formatted

## Installation

Install the development version of cst from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("earthlab/cst")
```

This package also has some python dependencies. 
We recommend using conda (preferably [miniconda](https://docs.conda.io/en/latest/miniconda.html)) to simplify the installation process. 
Python dependencies can be installed as follows: 

``` r
cst::install_py_deps()
```

### Using Docker instead of a local installation

If you are having trouble installing this package locally, or simply do not want
to install the package locally, we also provide a 
[Docker](https://www.docker.com/) image that has the 
package and its dependencies pre-installed, along with
RStudio server which can run in a web browser. 

To use the Docker image, you'll need to have Docker installed (see 
[Docker installation instructions here](https://docs.docker.com/install/)), then 
run the following command from a terminal, replacing <yourpassword> with 
a password of your choosing.

```bash
docker run -e PASSWORD=<yourpassword> -d -p 8787:8787 earthlab/cst
```

Then, in a web browser, navigate to localhost:8787. 
Log in with username: `rstudio`, and the password you provided. 

## Quickstart guide

To get daily precipitation data for Acadia National Park for a subset of 
climate models, you can use the `cstdata()` function:


```r
library(ggplot2)
library(cst)
library(reticulate)

use_condaenv("cst")
```

Then, download some data using the `cstdata()` function. 





```r
d <- cstdata(park = "Acadia National Park", parameters = "pr", 
             years = c(2020, 2021), models = "CCSM4", scenarios = "rcp85")
```

This gives you a data frame with paths to local climate data files: 


```r
str(d)
#> Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  13 variables:
#>  $ local_file      : chr "pr_acadia_national_park_CCSM4_r6i1p1_rcp85_macav2metdata_2020_2021_daily.nc"
#>  $ local_path      : chr "/home/mjoseph/acadia_national_park/pr_acadia_national_park_CCSM4_r6i1p1_rcp85_macav2metdata_2020_2021_daily.nc"
#>  $ model           : chr "CCSM4"
#>  $ parameter       : chr "pr"
#>  $ rcp             : chr "rcp85"
#>  $ ensemble        : chr "r6i1p1"
#>  $ year1           : num 2020
#>  $ year2           : num 2021
#>  $ area_name       : chr "acadia_national_park"
#>  $ units           : chr "mm"
#>  $ full_varname    : chr "Precipitation"
#>  $ internal_varname: chr "precipitation"
#>  $ parameter_long  : chr "precipitation"
```

And, you can also summarize the daily data by computing a spatial average over the region of interest: 


```r
df <- cst_df(d, ncores = 2)
#> Computing spatial averages...
#> Generating climate data.frame...
str(df)
#> Classes 'tbl_df', 'tbl' and 'data.frame':	731 obs. of  6 variables:
#>  $ rcp      : chr  "rcp85" "rcp85" "rcp85" "rcp85" ...
#>  $ date     : Date, format: "2020-01-01" "2020-01-02" ...
#>  $ model    : chr  "CCSM4" "CCSM4" "CCSM4" "CCSM4" ...
#>  $ ensemble : chr  "r6i1p1" "r6i1p1" "r6i1p1" "r6i1p1" ...
#>  $ area_name: chr  "acadia_national_park" "acadia_national_park" "acadia_national_park" "acadia_national_park" ...
#>  $ pr       : num  0 4.7804 0.0497 16.9338 18.5549 ...
```

Because this is a data.frame, you can use all of the normal data visualization and processing functionality in R, e.g.,


```r
df %>%
  ggplot(aes(date, pr)) + 
  geom_point() + 
  geom_line(alpha = .1) + 
  xlab("Date") + 
  ylab("Precipitation (mm)") + 
  ggtitle("Acadia National Park, CCSM4, RCP 8.5")
```

<img src="man/figures/README-ggplot-precip-1.png" title="plot of chunk ggplot-precip" alt="plot of chunk ggplot-precip" width="100%" />

### Dive deeper

This is just a small glimpse at what you can do with the cst package.
For more, see [Getting started with the Climate Scenarios Toolbox](https://www.earthdatascience.org/cst/articles/cst-intro.html).

## Development instructions

### Building documentation

The vignettes in this package are pre-rendered because they take a while to 
execute. 
The `Makefile` contains instructions for rendering the vignettes, as well as
the manual and README.md file. 
To build the documentation, execute the following command from the terminal: 

```bash
make
```

## Meta

* Please [report any issues or bugs](https://github.com/earthlab/cst/issues),
after reading our contribution [guidelines](CONTRIBUTING.md), and the 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
* License: GPL-3
* See `citation("cst")` in R to cite this package in publications. 

