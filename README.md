Norwegian Spring Spawning Herring explorer
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/g-jus/NSSH/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/g-jus/NSSH/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Norwegian Spring-Spawning Herring Explorer

The **NSSH Explorer** is an R package that combines a fully interactive
**Shiny application** for working with Norwegian Spring-Spawning Herring
(NSSH) data.

The app provides:

- Dynamic **growth model visualizations** (VBGM, Gompertz & logistic)
- Annual statistics such as **counts** and **weights** per year
- Detailed **age composition** plots
- Annual table of **mean age** and **weight** with total number of
  caught NSSH  
- An interactive **leaflet map** of catch locations

The goal is to offer a clean, transparent, and reproducible workflow for
herring data exploration.

## Installation

You can install the development version of NSSH from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("g-jus/NSSH")
```

## Use package

To launch the app use command:

``` r
NSSH::run_NSSH()
```

For further insight to different functions in the package see the help
file.

Here information about the underlying functions are provided. For
insight in package structure, pipeline and syntax see
[GitHub](https://github.com/g-jus/NSSH).

This package will **not** be regularly updated or maintained, but
potential debugs or adjustments can be suggested through pull request on
[GitHub](https://github.com/g-jus/NSSH).

*23.03.26 - Sundvor, G.*
