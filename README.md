# metapp


<!-- README.md is generated from README.qmd. Please edit that file -->

**NOTE: This package should be used for internal purposes only at the
moment.**

## Overview

A Helper Package for Recalculating Effect Sizes in Meta-Analyses.

## Background

Meta-Analyses quantify and combine effects in the published scientific
literature. Different ways exist to calculate an effect size (and its
uncertainty) for a single scientific study. This packages provides
various effect size calculation methods for group differences.

## Installation

Install `metapp` from GitHub:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("smnnlt/metapp")
```

## Usage

``` r
library(metapp)

# calculate a SMD with Hedges correction for small sample bias
smd(x1 = 5, x2 = 7, sd1 = 2, sd2 = 3, n1 = 20, n2 = 18)
#>           es       var
#> 1 -0.7763268 0.1134856
```
