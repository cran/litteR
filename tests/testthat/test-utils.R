test_that("enumeration of things work properly", {
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
    expect_equal(toString(sequenize(c(1:4, 8:9))), "1-4, 8-9")
    expect_equal(toString(sequenize(as.integer(c(1:4, 8)))), "1-4, 8")
})