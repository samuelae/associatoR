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
  ar_set_targets(...) %>% 
  ar_embed(...) %>% 
  ar_plot(...)
```

# Workflow

``` mermaid
flowchart TD
    DATA(free association data) --> IMPORT["ar_import()"]
    IMPORT --> OBJ("associatoR Object \n [cues, participants, responses]")
    IMPORT --> NORM["ar_normalize()"] --> OBJ
    IMPORT --> NORMM["ar_normalize_manual()"] --> OBJ

    OBJ <--> SUB["ar_subset()"]

    OBJ --> SET["ar_set_targets()"]

    SET --> OBJT("associatoR Object \n [cues, participants, responses, targets]")

    OBJT <--> COUNT["ar_count_targets()"]
    OBJT <--> CHAR["ar_characterize_targets()"]

    OBJT --> EMBED["ar_embed()"]
    EMBED --> OBJTE("associatoR Object \n [cues, participants, responses, targets, target_embedding]")

    OBJTE <--> PROJ["ar_project()"]

    COMP["ar_compare()"]
    OBJ <--> COMP
    OBJT <--> COMP
    OBJTE <--> COMP

    CORR["ar_correlate()"]
    OBJ <--> CORR
    OBJT <--> CORR
    OBJTE <--> CORR
```


Detailed descriptions can be found on the [package website](https://samuelaeschbach.com/associatoR). This includes the [function reference](https://samuelaeschbach.com/associatoR/reference/index.html).

## Citation

Aeschbach, S., Wulff, D. U. (2023). associatoR. <https://github.com/samuelae/associatoR>
