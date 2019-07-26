test_that("tests that natural numbers work", {
    expect_true(all(is_natural_number(0:9)))
    expect_true(is_natural_number(1))
    expect_false(is_natural_number(-1))
    expect_false(is_natural_number(1.23))
    expect_false(is_natural_number(1.99999))
    expect_false(all(is_natural_number(letters)))
})


test_that("litter types are detected", {

    # valid litter type (e-mail WvL 2019-05-14 22:01)
    expect_true(is_type_name("[56] spoon plastic"))
    expect_true(is_type_name("Plastic: spoon [56]")) # OSPAR
    expect_true(is_type_name("Spoon plastic [56]"))
    expect_true(is_type_name("Spoon [56] plastic"))
    expect_true(is_type_name("Spoon [56]"))
    expect_true(is_type_name("Spoon [G56]"))  # TSG ML
    expect_true(is_type_name("Spoon [AB56]")) # UNEP

    # invalid litter type: as TSG-ML general code but with space
    expect_false(is_type_name("Spoon [A 56]"))

    # invalid litter type: as UNEP-code but with space
    expect_false(is_type_name("Spoon [AB 56]"))

    # invalid litter type: additional punctuation : and + are not allowed
    expect_false(is_type_name("All cartons/tetrapaks [302:204+62+118]"))

    # invalid litter type: numeric litter code is missing
    expect_false(is_type_name("no litter here"))

    # invalid litter type: number greater than 989
    expect_false(is_type_name("Survey: Remarks [999]"))

    # invalid litter type: number greater than 989
    is_type_name("[Spoon 56 plastic]")

    # multiple litter types
    expect_identical(
        is_type_name(c("Survey: Remarks [999]", "[TA]", "Plastic: Food [6]")),
        c(FALSE, FALSE, TRUE))
})


#' @importFrom rlang is_na
test_that("litter codes are correctly extracted", {
    expect_identical(get_type_code("[56] spoon plastic"), "[56]")
    expect_identical(get_type_code("Plastic: spoon [56]"), "[56]") # OSPAR
    expect_identical(get_type_code("Spoon plastic [56]"), "[56]")
    expect_identical(get_type_code("Spoon [56] plastic"), "[56]")
    expect_identical(get_type_code("Spoon [56]"), "[56]")
    expect_identical(get_type_code("Spoon [G56]"), "[G56]")  # TSG ML
    expect_identical(get_type_code("Spoon [AB56]"), "[AB56]") # UNEP
    expect_identical(get_type_code("Plastic: Food [6]"), "[6]")
    expect_identical(get_type_code(c("Plastic: Food [6]",
                                     "Plastic: Shoes [44]")),
                     c("[6]", "[44]"))
    expect_true(is_na(get_type_code("[TA]")))
    expect_true(is_na(get_type_code("All cartons/tetrapaks [302:204+62+118]")))
})


test_that("litter readers produce messages", {
    filename <- path_package("litteR", "extdata",
                             "beach-litter-nl-2012-2017-ospar.csv")
    expect_message(read_litter(filename),
                   "The following fields will be excluded from analysis:")
    filename <- path_package("litteR", "extdata",
                             "beach-litter-nl-2012-2017-long.csv")
    expect_message(read_litter(filename), NA)
})


test_that("litter readers give identical results", {
    d_ospar <- path_package("litteR", "extdata",
                            "beach-litter-nl-2012-2017-ospar.csv") %>%
        read_litter %>%
        select(-region_name) %>%
        arrange(location_name, date, type_name)
    d_wide <- path_package("litteR", "extdata",
                           "beach-litter-nl-2012-2017-wide.csv") %>%
        read_litter %>%
        select(-region_name) %>%
        arrange(location_name, date, type_name)
    d_long <- path_package("litteR", "extdata",
                           "beach-litter-nl-2012-2017-long.csv") %>%
        read_litter %>%
        select(-region_name) %>%
        arrange(location_name, date, type_name)
    expect_identical(d_ospar, d_wide)
    expect_identical(d_ospar, d_long)
})

test_that("litter readers handle NA properly", {
    expect_warning(
        path_package("litteR", "extdata",
                     "beach-litter-nl-2012-2017-ospar.csv") %>%
            read_csv %>%
            mutate("Plastic: Bags [2]" = NA_integer_) %>%
            litteR:::validate.ospar(),
        "do not contain natural numbers"
    )
    expect_warning(
        path_package("litteR", "extdata",
                     "beach-litter-nl-2012-2017-long.csv") %>%
            read_csv %>%
            mutate(abundance = NA_integer_) %>%
            litteR:::validate.long(),
        "do not contain natural numbers"
    )
})


test_that("date formats are tested correctly", {
    expect_true(is_date_format("2019-05-14", "%Y-%m-%d"))
    expect_false(is_date_format("2019-14-05", "%Y-%m-%d"))
})

test_that("date formats are tested correctly", {
    expect_identical(get_signature(path_package("litteR", "extdata",
                    "beach-litter-nl-2012-2017-ospar.csv")), "OSPAR")
    expect_identical(get_signature(path_package("litteR", "extdata",
                    "beach-litter-nl-2012-2017-long.csv")), "long")
})


test_that("tests on litter codes work", {
    expect_identical(get_type_code("Plastic: Food [6]"), "[6]")
    expect_identical(get_type_code(c("Plastic: Food [6]",
                                     "Plastic: Shoes [44]")),
                     c("[6]", "[44]"))
    expect_true(is_na(get_type_code("All cartons/tetrapaks [302:204+62+118]")))
    expect_true(is_na(get_type_code("[TA]")))
    expect_true(is_na(get_type_code("total abundance [TA]")))
})