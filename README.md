cstdata

[![Build Status](https://travis-ci.com/WilliamsTravis/cstdata.svg?branch=master)](https://travis-ci.com/WilliamsTravis/cstdata)
[![Codecov test coverage](https://codecov.io/gh/WilliamsTravis/cstdata/branch/master/graph/badge.svg)](https://codecov.io/gh/WilliamsTravis/cstdata?branch=master)


This is a collection of methods for quickly downloading downscaled global climate model (GCM) data covering specified areas for 
use in the USGS NCCASC Climate Scenario Toolbox.

## Installation

Install the development version of the cstdata package with:


```r
remotes::install_github("WilliamsTravis/cstdata")
```

Then, install python dependencies via:

```r
cstdata::install_py_deps()
```

## Getting started

The Climate Scenario Toolbox is an R package that allows users to quickly query downscaled Global Climate Models for
a variety of climate variables for a specified area and future time period. 

The GCM data is downscaled to a 4km resolution via the Multivariate Adaptive Constructed Analogs method (MACA). 

## Example

TODO