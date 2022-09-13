
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moostr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of moostr via:

``` r
# install.packages("remotes")
remotes::install_github("iris-yi-jiang/moostr")
```

## Overview

`moostr` generates a Moodle syntax string for a given list of possible
answers and their corresponding rewards. It allows the user to
conveniently allocate rewards for multiple correct and partially correct
answers. `moostr` also randomly shuffles the answer list for multichoice
questions.

The returned Moodle syntax string can be directly used as the solution
to a verbatim item in a cloze exercise with `exams`.

## Example

This is a basic example:

``` r
library(moostr)
make_moostr(type="mchoice", ans=c("A two-sample t-test",
"A paired t-test", "A one-sample t-test"), reward=c(50, 100, 0))
```
