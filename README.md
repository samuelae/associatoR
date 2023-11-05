# associatoR

Associations offer a window into the semantics of the mind, revealing how concepts and words are interconnected. This has interesting applications not only for psychological theory but also for practical uses in marketing research, sociology, education, and artificial intelligence.

`associatoR` is an R package to work with association data. It offers pipelines to import, normalize, process, analyze, and convey results.

## General Information

The `associatoR` package is developed by [Samuel Aeschbach](https://github.com/samuelae) and [Dirk U. Wulff](https://github.com/dwulff).

# Installation

The latest development version on GitHub can be installed via `devtools::install_github("samuelae/associatoR")`. Note that this requires prior installation of the `devtools` package which can be achieved running `install.packages("devtools")` in R.

# Usage

This is a prototypical workflow using functions from the `associatoR` package.

``` r
data %>% 
  ar_import(...) %>% 
  ar_set_target(...) %>% 
  ar_embed(...) %>% 
  ar_plot(...)
```

Detailed descriptions can be found on the [package website](https://samuelaeschbach.com/associatoR). This includes the [function reference](https://samuelaeschbach.com/associatoR/reference/index.html).

## Citation

Aeschbach, S., Wulff, D. U. (2023). associatoR. <https://github.com/samuelae/associatoR>
