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

## ---- out.width=450, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/histogram-litter.png")

## ---- out.width=650, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/library-litter.png")

## ---- out.width=650, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/file-open-dialogue.png")

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "types-ospar-sup-fish-other.csv") %>%
    read_csv(col_types = cols(
        type_name = col_character(),
        included = col_character(),
        SUP = col_character(),
        FISH = col_character(),
        PLASTIC = col_character()
    )) %>%
    slice(1:10) %>%
    kable(align = c("l", "r", "c", "c", "c", "c"))

## ---- echo=FALSE--------------------------------------------------------------
add_dots <- function(x) {
    x_bot = x[1,]
    x_bot[1,] = ":"
    x <- bind_rows(x, x_bot)
    x_right <- x[,1]
    x_right[,1] <- "..."
    names(x_right) <- "Plastic..."
    x_right[nrow(x_right), 1] <- ":"
    bind_cols(x, x_right)
}

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017.csv") %>%
    read_csv(col_types = cols(.default = col_character())) %>%
    select(location_code, date, `Plastic: Yokes [1]`, `Plastic: Bags [2]`) %>%
    slice(1:5) %>%
    add_dots %>%
    kable(align = "llrrr")

## ---- message=FALSE, comment=NA-----------------------------------------------
x_top <- path_package("litteR", "extdata", "beach-litter-nl-2012-2017.csv") %>%
    read_csv(col_types = cols(.default = col_character())) %>%
    select(region_code, location_code, date, `Plastic: Yokes [1]`, `Plastic: Bags [2]`) %>%
    slice_head(n = 3) 
x_bot <- path_package("litteR", "extdata", "beach-litter-nl-2012-2017.csv") %>%
    read_csv(col_types = cols(.default = col_character())) %>%
    select(region_code, location_code, date, `Plastic: Yokes [1]`, `Plastic: Bags [2]`) %>%
    slice_tail(n = 3) 
x_mid <- x_top[1,]
x_mid[1,] <- ":"
bind_rows(x_top, x_mid, x_bot) %>%
    add_dots %>%
    slice_head(n = nrow(.)-1) %>%
    {
        .[4,ncol(.)] <- ":"
        .
    } %>%
    kable(align = "lllrrr")

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017.csv") %>%
    read_csv(col_types = cols(.default = col_character())) %>%
    mutate(Country = "Netherlands") %>%
    transmute(
        `Beach ID` = location_code,
        `Beach name` = location_name,
        Country,
        `Survey date` = date,
        `Plastic: Yokes [1]`,
        `Plastic: Bags [2]`) %>%
    slice(1:3) %>%
    add_dots %>%
    kable(align = "llllrrr")

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017.csv") %>%
    read_csv(col_types = cols(.default = col_character())) %>%
    mutate(Country = "Netherlands") %>%
    transmute(
        location_code = location_code,
        `Beach name` = location_name,
        region_code = Country,
        date = date,
        `Plastic: Yokes [1]`,
        `Plastic: Bags [2]`) %>%
    slice(1:3) %>%
    add_dots %>%
    kable(align = "llllrrr")

## ---- message=FALSE, comment=NA-----------------------------------------------
path_package("litteR", "extdata", "beach-litter-nl-2012-2017.csv") %>%
    read_csv(col_types = cols(.default = col_character())) %>%
    mutate(Country = "Netherlands") %>%
    transmute(
        lc = location_code,
        region_code = Country,
        location_code = location_name,
        date = format(as.Date(date), "%d/%m/%Y"),
        `Beach ID` = lc,
        `Beach name` = location_name,
        Country,
        `Survey date` = date) %>%
    select(-lc) %>%
    slice(1:3) %>%
    add_dots %>%
    kable(align = "l")

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

## ---- out.width=600, fig.align="center"---------------------------------------
knitr::include_graphics("./fig/regional-trend-sup.png")

## ---- message=FALSE, comment=NA-----------------------------------------------
path("tab", "litteR-results-20210904T221809.csv") %>%
    read_csv(col_types = cols(.default = col_character())) %>%
    slice(1:10) %>%
    kable()

## ---- message=FALSE, comment=NA-----------------------------------------------
path("tab", "litteR-log-20210904T221809.log") %>%
    read_lines %>%
    head(20) %>%
    paste0(collapse = "\n") %>%
    cat("\n")

## ---- message=FALSE, comment=NA-----------------------------------------------
path("tab", "litteR-log-20210904T221809.log") %>%
    read_lines %>%
    paste0(collapse = "\n") %>%
    cat("\n")

