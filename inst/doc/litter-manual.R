## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)
options(knitr.kable.NA = "",  dplyr.width = Inf)
library(dplyr)
library(readr)
library(fs)
library(kableExtra)

## ---- out.width=650, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/beach-litter-cover-photo.jpg")

## ---- out.width=450, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/histogram-litter.png")

## ---- out.width=650, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/library-litter.png")

## ---- out.width=650, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/file-open-dialogue.png")

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "types-ospar-tc-sup-fish-plastic.csv") %>%
    read_csv %>%
    slice(1:10) %>%
    kable(align = c("l", "r", "c", "c", "c", "c"))

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017.csv") %>%
    read_csv %>%
    select(spatial_code, date, country_code, `Plastic: Bags [2]`, `Plastic: Small_bags [3]`) %>%
    slice(1:10) %>%
    kable

## ---- out.width="500px", fig.cap="Settings file in YAML-format.", fig.align='center', comment=NA----
path_package("litteR", "extdata", "settings.yaml") %>%
    read_lines %>%
    cat(sep = "\n")

## ---- out.width=650, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/outlier.png")

## ---- out.width=450, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/bar-plot-bergen.png")

## ---- out.width=600, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/terschelling-tc.png")

## ---- out.width=600, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/terschelling-sup.png")

## ---- out.width=600, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/terschelling-bags.png")

## ---- message=FALSE, comment=NA-----------------------------------------------
path("tab", "litteR-results-20200602T141829.csv") %>%
    read_csv %>%
    slice(1:10) %>%
    kable()

## ---- message=FALSE, comment=NA-----------------------------------------------
path("tab", "litteR-log-20200602T141829.log") %>%
    read_lines %>%
    head(20) %>%
    paste0(collapse = "\n") %>%
    cat("\n")

