test_that("litter readers produce error/warning/informative messages", {
    d <- dplyr::tribble(
        ~location_code,        ~date,  ~`p: a [1]`, ~dummy,
                    1, "15/01/2020",            3,      4,
                    1, "16/01/2020",            3,      4,
                    1, "17/01/2020",            3,      4
    )
    type_names <- c("p: a [1]", "p: b [2]")
    expect_warning(
        litteR:::validate.litter(d, type_names),
        regexp = "The following types in the type file are missing in the data file")
    expect_error(
        litteR::read_litter("test.xyz")
    )
})



test_that("litter readers handle NA properly", {
    d <- dplyr::tribble(
        ~location_code,        ~date,  ~`p: a [1]`, ~`p: b [2]`,
                    1, "15/01/2020",            3,           4,
                    1, "16/01/2020",     NA_real_,           4,
                    1, "17/01/2020",            3,           4
    )
    type_names <- c("p: a [1]", "p: b [2]")
    expect_error(
        litteR:::validate.litter(d, type_names),
        "The following data column\\(s\\) contain\\(s\\) empty cells"
    )
})



test_that("duplicates in data file are correctly detected", {
    d <- dplyr::tribble(
       ~location_code,    ~date,      ~`p: a [1]`, ~`p: b [2]`,
                   1, "15/01/2020",            3,           4,
                   1, "16/01/2020",            3,           4,
                   1, "16/01/2020",            3,           4,
                   1, "17/01/2020",            3,           4
    )
    class(d) <- c("litter", class(d))
    type_names <- c("p: a [1]", "p: b [2]")
    expect_warning(
        litteR:::validate.litter(d, type_names),
        regexp = str_c("The following records are duplicated: \n2 and 3."))
    d$`p: a [1]`[2] <- 0
    expect_warning(
        litteR:::validate.litter(d, type_names),
        regexp = "`location_code` and `date`:\n2 and 3")
})


test_that("all columns with litter counts are found", {
    d <- tibble(
        location_code = c(1, 1, 1, 1),
        date = as.Date("2020-01-24") + 0:3)
    type_names <- c("Plastic: Yokes [1]")
    expect_warning(
        expect_error(
            litteR:::validate.litter(d, type_names),
            regexp = "No columns with litter data found"),
        regexp = "The following types in the type file are missing in the data file"
    )
    d$`Plastic: Yokes [1]` <- 3
    expect_output(litteR:::validate.litter(d, type_names))
    d$`Plastic: Yokes [1]` <- c(3.1, 3.5, 4.2, 5.4)
    expect_warning(
        litteR:::validate.litter(d, type_names),
        regexp = "Non-natural numbers")
    d$`Plastic: Yokes [1]` <- c(3, "dummy", 3, 3)
    expect_error(
        litteR:::validate.litter(d, type_names),
        regexp = "The following data column\\(s\\) contain\\(s\\) text")
    d$`Plastic: Yokes [1]` <- c(3, NA, 3, 3)
    expect_error(
        litteR:::validate.litter(d, type_names),
        regexp = "The following data column\\(s\\) contain\\(s\\) empty cells")
})