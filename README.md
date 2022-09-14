
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moostr <a href="https://iris-yi-jiang.github.io/moostr/"><img src="man/figures/logo.png" align="right" height="139" /></a>

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
answers. For multichoice questions, `moostr` also randomly shuffles the
answer list. For numerical questions, `moostr` also allows the
flexibility to specify different tolerance levels for each possible
answer.

For `moostr` to function properly, it is necessary to set the question
type for both multichoice and numerical questions to vertatim type. The
returned Moodle syntax string can then be directly used as the solution
to a verbatim item in an Embedded Answers (Cloze) exercise with `exams`
or Moodle.

## Example

This is a basic example for mutichoice question type. Note that `moostr`
shuffles the answer list.

``` r
library(moostr)
make_moostr(type="mchoice", 
            ans=c("A two-sample t-test",
                  "A paired t-test", 
                  "A one-sample t-test"), 
            reward=c(50, 100, 0))
#> [1] ":MULTICHOICE:%0%A one-sample t-test~%50%A two-sample t-test~%100%A paired t-test"
```

This is a basic example for numerical question type. Note that `moostr`
allows the flexibly to specify different tolerance levels for each
possible answer. If the same tolerance level is preferred across all
possible answers, only a single tolerance level is needed for the tol
argument. The tol argument is by default set to 0.01, so if this is the
preferred tolerance level, then it is not necessary to set it.

The tol argument should be set carefully. If the difference between
correct and partially correct answers are within the specified tolerance
level, then the reward applied to the partially correct answer will be
ignored and the reward for the correct answer will be used to evaluate
both answers.

``` r
library(moostr)
make_moostr(type="num", 
            ans=c(2, 2.1, 2.01), 
            reward=c(30, 100, 50),
            tol = c(0, 0.1, 0.01))
#> [1] ":NUMERICAL:%30%2:0~%100%2.1:0.1~%50%2.01:0.01"
make_moostr(type="num", 
            ans=c(2, 2.1, 2.01), 
            reward=c(30, 100, 50),
            tol = 0.1)
#> [1] ":NUMERICAL:%30%2:0.1~%100%2.1:0.1~%50%2.01:0.1"
make_moostr(type="num", 
            ans=c(2, 2.1, 2.01), 
            reward=c(30, 100, 50))
#> [1] ":NUMERICAL:%30%2:0.01~%100%2.1:0.01~%50%2.01:0.01"
```
