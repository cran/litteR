test_that("date formats are tested correctly", {
    expect_true(is_date_format("2019-05-14", "%Y-%m-%d"))
    expect_false(is_date_format("2019-14-05", "%Y-%m-%d"))
})



test_that("tests that natural numbers work", {
    expect_true(all(is_natural_number(0:9)))
    expect_true(is_natural_number(1))
    expect_false(is_natural_number(-1))
    expect_false(is_natural_number(1.23))
    expect_false(is_natural_number(1.99999))
    expect_false(all(is_natural_number(letters)))
})



test_that("enumeration of things work properly", {
    expect_identical(
        enumerate(1),
        "1")
    expect_identical(
        enumerate(1:2),
        "1 and 2")
    expect_identical(
        enumerate(1:3/3),
        "0.3333333, 0.6666667 and 1.0000000")
    expect_identical(
        enumerate(1:3/3, digits = 2),
        "0.33, 0.67 and 1.00")
    expect_identical(
        enumerate(1:3),
        "1, 2 and 3")
    expect_identical(
        enumerate("apples"),
        "apples")
    expect_identical(
        enumerate(c("apples", "oranges")),
        "apples and oranges")
    expect_identical(
        enumerate(c("apples", "oranges", "pears")),
        "apples, oranges and pears")
})



test_that("enumeration of things work properly", {
    expect_error(sequenize(as.integer(c(1, 5, 4, 2))))
    expect_error(sequenize(as.integer(c(1, 1, 1, 1))))
    expect_equal(enumerate(sequenize(c(1:4, 8:9))), "1-4, 8-9")
    expect_equal(enumerate(sequenize(as.integer(c(1:4, 8)))), "1-4, 8")
})



test_that("duplicates are correctly listed", {
    
    # tibble/data.frame
    d <- data.frame(x = 1:4, y = letters[1:4])
    expect_equal(list_duplicates(d), list())
    d[3, ] <- d[1, ]
    expect_equal(list_duplicates(d), list(c(1, 3)))
    d[4, ] <- d[1, ]
    expect_equal(list_duplicates(d), list(c(1, 3, 4)))
    
    # corner cases
    expect_null(list_duplicates(tibble()))
    expect_null(list_duplicates(""))
})