---
output: github_document
editor_options: 
  chunk_output_type: console
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# cft: climate futures toolbox

<!-- badges: start -->
[![Build Status](https://travis-ci.com/earthlab/cft.svg?branch=master)](https://travis-ci.com/earthlab/cft)
[![codecov](https://codecov.io/gh/earthlab/cft/branch/master/graph/badge.svg)](https://codecov.io/gh/earthlab/cft)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build status](https://github.com/earthlab/cft/workflows/R-CMD-check/badge.svg)](https://github.com/earthlab/cft/actions)
[![R build status](https://github.com/earthlab/cft/workflows/pkgdown/badge.svg)](https://github.com/earthlab/cft/actions)
[![DOI](https://zenodo.org/badge/205295577.svg)](https://zenodo.org/badge/latestdoi/205295577)
<!-- badges: end -->

The goal of cft is to provide easy climate data access 
([MACA v2](http://www.climatologylab.org/maca.html)) to support 
climate scenario planning.
This package allows you to: 

1. Quickly acquire climate data subsets for a spatial region of interest
2. Summarize climate data at daily timesteps, and compute derived quantities
3. Contrast reference and target time periods to understand differences in 
climate over time, and
4. Easily work with climate data, without having to worry about the details of 
how it is stored or formatted

## Installation

Install the development version of cft from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("earthlab/cft")
```

## Quickstart guide

To get daily maximum air temperature data for an area of interest, you can use 
the `cftdata()` function:


```r
library(ggplot2)
library(cft)
```

First define an area of interest. This should be a `Spatial*` object. 
In this case we'll load a file distributed with this package, but you 
could read a local shapefile etc. 


```r
aoi <- rgdal::readOGR(system.file("extdata", "windcave.geojson", package = "cft"))
#> OGR data source with driver: GeoJSON 
#> Source: "/home/max/R/x86_64-pc-linux-gnu-library/4.0/cft/extdata/windcave.geojson", layer: "windcave"
#> with 1 features
#> It has 19 fields
```

Then, download some data using the `cftdata()` function. 


```r
d <- cftdata(aoi = aoi, area_name = "windcave", parameters = "tasmax", 
             years = c(2003, 2007), models = "CCSM4", scenarios = "rcp85")
#> [1] "Building area of interest grid..."
#> [1] "Retrieving climate data for windcave"
#> [1] "Saving local files to /tmp/RtmpfGqX6E/windcave"
```

This gives you a data frame with paths to local climate data files: 


```r
d
#> # A tibble: 1 x 13
#>   local_file local_path model parameter rcp   ensemble year1 year2 area_name
#>   <chr>      <chr>      <chr> <chr>     <chr> <chr>    <dbl> <dbl> <chr>    
#> 1 tasmax_wi… /tmp/Rtmp… CCSM4 tasmax    rcp85 r6i1p1    2003  2007 windcave 
#> # … with 4 more variables: units <chr>, full_varname <chr>,
#> #   internal_varname <chr>, parameter_long <chr>
```

And, you can also summarize the daily data by computing a spatial average over the region of interest: 


```r
df <- cft_df(d, ncores = 2)
#> Computing spatial averages...
#> Generating climate data.frame...
df
#> # A tibble: 1,826 x 6
#>    rcp   date       model ensemble area_name tasmax
#>    <chr> <date>     <chr> <chr>    <chr>      <dbl>
#>  1 rcp85 2003-01-01 CCSM4 r6i1p1   windcave    265.
#>  2 rcp85 2003-01-02 CCSM4 r6i1p1   windcave    263.
#>  3 rcp85 2003-01-03 CCSM4 r6i1p1   windcave    268.
#>  4 rcp85 2003-01-04 CCSM4 r6i1p1   windcave    270.
#>  5 rcp85 2003-01-05 CCSM4 r6i1p1   windcave    269.
#>  6 rcp85 2003-01-06 CCSM4 r6i1p1   windcave    274.
#>  7 rcp85 2003-01-07 CCSM4 r6i1p1   windcave    271.
#>  8 rcp85 2003-01-08 CCSM4 r6i1p1   windcave    276.
#>  9 rcp85 2003-01-09 CCSM4 r6i1p1   windcave    271.
#> 10 rcp85 2003-01-10 CCSM4 r6i1p1   windcave    273.
#> # … with 1,816 more rows
```

Because this is a data.frame, you can use all of the normal data visualization and processing functionality in R, e.g.,


```r
df %>%
  ggplot(aes(date, tasmax)) + 
  geom_point() + 
  geom_line(alpha = .1) + 
  xlab("Date") + 
  ylab("Max. air temp. (K)") + 
  ggtitle("Wind Cave National Park, CCSM4, RCP 8.5")
```

<img src="man/figures/README-readme-1.png" title="plot of chunk readme" alt="plot of chunk readme" width="100%" />

### Dive deeper

This is just a small glimpse at what you can do with the cft package.
For more, see [Getting started with the Climate Futures Toolbox](https://www.earthdatascience.org/cft/articles/cft-intro.html).

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


### Using Docker instead of a local installation

If you are having trouble installing this package locally, or do not want
to install the package locally, we also provide a 
[Docker](https://www.docker.com/) image that has the 
package and its dependencies pre-installed, along with
RStudio server which can run in a web browser. 

To use the Docker image, you'll need to have Docker installed (see 
[Docker installation instructions here](https://docs.docker.com/install/)), then 
run the following command from a terminal, replacing <yourpassword> with 
a password of your choosing.

```bash
docker run -e PASSWORD=<yourpassword> -d -p 8787:8787 earthlab/cft
```

Then, in a web browser, navigate to localhost:8787. 
Log in with username: `rstudio`, and the password you provided. 

## Meta

* Please [report any issues or bugs](https://github.com/earthlab/cft/issues),
after reading our contribution [guidelines](CONTRIBUTING.md), and the 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
* License: GPL-3
* See `citation("cft")` in R to cite this package in publications. 

