test_that("Tukeys trimean is correct", {
    expect_equal(trimean(0:100), mean(0:100))
    expect_equal(trimean(0:100), median(0:100))
})



test_that("Mann Kendall is correct", {
    # single argument
    x <- c(12, 8, 9, 4, 7, 5, 3, 1)
    mk <- mann_kendall(x, type = "decreasing")
    ct <- cor.test(seq_along(x), x, alternative = "less", method = "kendall")
    expect_equivalent(test_statistic(mk), ct$estimate)
    expect_equal(p_value(mk), ct$p.value)

    # two arguments (random order)
    x <- c(12, 8, 9, 4, 7, 5, 3, 1)
    t <- seq(
        from = as.Date("2022-04-04"),
        to = as.Date("2022-04-11"),
        by = "day")
    o <- sample(seq_along(x))
    t <- t[o]
    x <- x[o]
    mk <- mann_kendall(x = x, t = t, type = "decreasing")
    expect_equivalent(test_statistic(mk), ct$estimate)
    expect_equal(p_value(mk), ct$p.value)
    
    # irregular time steps
    x <- c(12, 8, 9, 4, 7, 5, 3, 1)
    t <- seq(
        from = as.Date("2022-01-01"),
        to = as.Date("2022-12-31"),
        by = "day")
    t <- sort(sample(t, size = length(x), replace = FALSE))
    mk <- mann_kendall(x = x, t = t, type = "decreasing")
    expect_equivalent(test_statistic(mk), ct$estimate)
    expect_equal(p_value(mk), ct$p.value)
})



test_that("Theil-Sen is correct", {
    y <- c(12, 8, 9, 4, 7, 5, 3, 1)
    x <- seq_along(y)
    ts <- theil_sen(x, y)
    expect_equal(slope(ts), -1.5)
    expect_equal(intercept(ts), 13.5)
})



test_that("Wilcoxon is correct", {
    set.seed(314)
    x <- rpois(n = 25, lambda = 25)
    wl <- wilcoxon(x, mu = 26, type = "less")
    wr <- wilcox.test(x, mu = 26, alternative = "less",
                      exact = length(x) == length(unique(x)))
    expect_equal(p_value(wl), wr$p.value)
    expect_equivalent(test_statistic(wl), wr$statistic)
})



test_that("various statistics are correct", {
    set.seed(314)
    x <- runif(100)
    expect_equal(cv(x), 0.5883164, tolerance = 1.0e-6)
    expect_equal(rmad(x), 0.6503762, tolerance = 1.0e-6)
    expect_equal(iod(x), 0.1560304, tolerance = 1.0e-6)
})



test_that("medcouple is correct", {

    # examples robustbase::mc
    expect_equal(medcouple(1:5), 0)
    expect_equal(medcouple(c(1, 2, 7, 9, 10)), -1 / 3)
    cushny <- c(0.0, 0.8, 1.0, 1.2, 1.3, 1.3, 1.4, 1.8, 2.4, 4.6)
    expect_equal(medcouple(cushny), 0)

    # properties
    a <- c(1, 2, 7, 9, 10)
    expect_equal(-medcouple(a), medcouple(-a))

    # tested against robustbase::mc
    expect_equal(medcouple(c(1:100, 1000)), 0)
    set.seed(314)
    a <- runif(100)
    expect_equal(medcouple(a), 0.06063277, tolerance = 1.0e-6)
    a <- rnorm(100)
    expect_equal(medcouple(a), -0.09275587, tolerance = 1.0e-6)
    a <- rlnorm(100)
    expect_equal(medcouple(a), 0.2972825, tolerance = 1.0e-6)
    a <- rnbinom(1000, size = 2, prob = 0.5)
    expect_equal(medcouple(a), 1 / 3, tolerance = 1.0e-6)
})




test_that("Kendall test works as expected", {
    
    # Gilbert, 1987, example p. 211
    # example without ties
    expect_equal(kendall_s(c(10, 15, 14, 20)), 4)
    
    # Van Belle & Hughes, 1984, p.129
    # example with ties
    x <- c(10, 14, 14, 14, 17, 22, 22, 23, 27, 27)
    expect_equal(kendall_var_s(x), 119.3, tolerance = 0.1)
                 
    # Gilbert, 1987, example 16.3, p. 211
    # example with ties
    expect_equal(
        kendall_s(c(10, 22, 21, 30, 22, 30, 40, 40),
                  c( 1,  1,  1,  2,  3,  3,  4,  5)), 19)

    # Gilbert, 1987, example 17.1, p.229
    # example with ties and multiple seasons
    d <- dplyr::tibble(
        location_code = c(1,  1,  2,  1,  2,  2,  1,  2),
        date          = c(1,  1,  1,  2,  2,  2,  3,  3),
        count         = c(8, 10, 15, 12, 20, 18, 15, 20)
    )
    sel <- d$location_code == 1
    x <- d$count[sel]
    t <- d$date[sel]
    s <- kendall_s(x, t)
    v <- kendall_var_s(x, t)
    expect_equal(s, 5)
    expect_equal(v, 7.667, tolerance = 1.0e-4)
    ct <- cor.test(t, x, method = "kendall", exact = FALSE)
    z <- as.numeric(ct$statistic)
    expect_equal(v, (s / z)^2)
    sel <- d$location_code == 2
    x <- d$count[sel]
    t <- d$date[sel]
    s <- kendall_s(x, t)
    v <- kendall_var_s(x, t)
    expect_equal(s, 4)
    expect_equal(v, 6.834, tolerance = 1.0e-4)
    ct <- cor.test(t, x, method = "kendall", exact = FALSE)
    z <- as.numeric(ct$statistic)
    expect_equal(v, (s / z)^2)

    # Gilbert, 1987, p.211    
    x <- c(23, 24, 0, 6, 0, 24, 24, 0, 23)
    expect_equal(kendall_var_s(x), 83.66667, tolerance = 1.e-3)
    
    # Van Belle & Hughes, 1984, p. 136
    x <- c(1.0, 1.0, 1.5, 1.7, 1.6, 1.7, 1.8, 1.7, 2.2, 2.3)
    t <- c(  1,   1,   1,   1,   2,   2,   2,   3,   3,   3)
    expect_equal(kendall_s(x, t), 26)
    expect_equal(kendall_var_s(x, t), 105, tolerance = 0.01)

    # check with stats::cor.test
    s <- kendall_s(x, t)
    ct <- cor.test(t, x, method = "kendall", exact = FALSE)
    z <- as.numeric(ct$statistic)
    expect_equal(kendall_var_s(x, t), (s/z)^2)
})
