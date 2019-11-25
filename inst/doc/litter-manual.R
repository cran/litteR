## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)
options(dplyr.width = Inf)
library(dplyr)
library(readr)
library(fs)
library(kableExtra)

## ---- out.width=650, fig.cap="Functions to start a litteR session.", fig.align="center"----
knitr::include_graphics("./fig/r-console.png")

## ---- out.width=650, fig.cap="File open dialogue.", fig.align="center"--------
knitr::include_graphics("./fig/file-open-dialogue.png")

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017-wide.csv") %>%
    read_csv %>%
    select(1:8) %>%
    slice(1:10)

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017-long.csv") %>%
    read_csv %>%
    slice(1:10)

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017-ospar.csv") %>%
    read_csv %>%
    select(1:10) %>%
    slice(1:10)

## ---- out.width="500px", fig.cap="Settings file in YAML-format.", fig.align='center', comment=NA----
path_package("litteR", "extdata", "settings.yaml") %>%
    read_lines %>%
    cat(sep = "\n")

## ----message=FALSE------------------------------------------------------------
"./tbl/settings.csv" %>%
    read_csv(
        col_types = cols(
          entry = col_character(),
          description = col_character(),
          value = col_character()
        )) %>%
    kable %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(2, width = "15em") %>%
    column_spec(3, width = "15em")

## ---- out.width=650, fig.cap="First 10 records of the litter-groups.csv file.", fig.align="center"----
knitr::include_graphics("./fig/litter-groups.png")

## ---- out.width=650, fig.cap="Example of a trend plot for total abundance (TA) at a beach near Bergen (The Netherlands). In this plot, the black dots are the observations, the thin gray line segments connect the dots and guide the eye, the blue line is a loess-smoother, and the red line is the Theil-Sen slope.", fig.align="center"----
knitr::include_graphics("./fig/trend.png")

## ---- out.width=650, fig.cap="Example of a baseline plot. Each dot is the average abundance of a specific litter type or the total abundance (TA) within a moving window of the size given on the x-axis.", fig.align="center"----
knitr::include_graphics("./fig/baseline-plot.png")

## ---- out.width=700, fig.cap="Snapshot of the baseline table in the report. For an explanation, see main text.", fig.align="center"----
knitr::include_graphics("./fig/baseline-table.png")

## ---- out.width=650, fig.cap="Example of a power analysis plot. It gives the power (y-axis) as function of the number of surveys (x-axis) for different effect sizes (see legend).", fig.align="center"----
knitr::include_graphics("./fig/power.png")

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "litter-stats-meta.csv") %>%
    read_csv %>% 
    kable(align = "lll")

