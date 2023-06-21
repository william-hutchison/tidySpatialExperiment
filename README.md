tidySpatialExperiment - part of tidytranscriptomics
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

**Brings SpatialExperimet to the tidyverse**

<!-- badges: start -->

[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/william-hutchison/tidySpatialExperiment/actions/workflows/check-bioc.yml/badge.svg)](https://github.com/william-hutchison/tidySpatialExperiment/actions)\]
<!-- badges: end -->

You can find more packages from the tidytranscriptomics ecosystem here:

- [tidySingleCellExperiment](https://github.com/stemangiola/tidySingleCellExperiment)
  for tidy manipulation of SingleCellExperiment objects
- [tidySummarizedExperiment](https://github.com/stemangiola/tidySummarizedExperiment)
  for tidy manipulation of SummarizedExperiment objects
- [tidyseurat](https://stemangiola.github.io/tidyseurat/) for tidy
  manipulation of Seurat objects
- [tidybulk](https://stemangiola.github.io/tidybulk/) for tidy bulk
  RNA-seq data analysis
- [nanny](https://github.com/stemangiola/nanny) for tidy high-level data
  analysis and manipulation
- [tidygate](https://github.com/stemangiola/tidygate) for adding custom
  gate information to your tibble
- [tidyHeatmap](https://stemangiola.github.io/tidyHeatmap/) for heatmaps
  produced with tidy principles

# Introduction

tidySpatialExperiment provides a bridge between the
[SpatialExperiment](https://github.com/drighelli/SpatialExperiment)
\[@righelli2022spatialexperiment\] and
[Tidyverse](https://www.tidyverse.org) \[@wickham2019welcome\] packages.
It creates an invisible layer that allows you to interact with a
SpatialExperiment object as if it were a tibble, enabling the use of
*dplyr*, *tidyr*, *ggplot2* and *plotly* functions. But, underneath,
your data remains a SpatialExperiment object.

tidySpatialExperiment also provides an additional two utility functions.

## Functions and utilities

| Package             | Functions available                                                                                                                                                                                                |
|---------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `SpatialExperiment` | All                                                                                                                                                                                                                |
| `dplyr`             | `arrange`,`bind_rows`, `bind_cols`, `distinct`, `filter`, `group_by`, `summarise`, `select`, `mutate`, `rename`, `left_join`, `right_join`, `inner_join`, `slice`, `sample_n`, `sample_frac`, `count`, `add_count` |
| `tidyr`             | `nest`, `unnest`, `unite`, `separate`, `extract`, `pivot_longer`                                                                                                                                                   |
| `ggplot2`           | `ggplot`                                                                                                                                                                                                           |
| `plotly`            | `plot_ly`                                                                                                                                                                                                          |

| Utility           | Description                                               |
|-------------------|-----------------------------------------------------------|
| `as_tibble`       | Convert cell-wise information to a `tbl_df`               |
| `aggregate_cells` | Aggregate cell gene-transcription abundance as pseudobulk |

## Installation

You can install the development version of tidySpatialExperiment from
GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("william-hutchison/tidySpatialExperiment")
```

# Examples

This is a basic example which shows you how to solve a common problem:

``` r
# Load example data from SpatialExperiment
# library(tidySpatialExperiment)
# example(read10xVisium)
```
