test_that("Tukeys trimean is correct", {
    expect_equal(trimean(0:100), mean(0:100))
    expect_equal(trimean(0:100), median(0:100))
})



test_that("Mann Kendall is correct", {
    x <- c(12, 8, 9, 4, 7, 5, 3, 1)
    mk <- mann_kendall(x, type = "decreasing")
    ct <- cor.test(1:length(x), x, alternative = "less", method = "kendall")
    expect_equivalent(test_statistic(mk), ct$estimate)
    expect_equal(p_value(mk), ct$p.value)
})



test_that("Theil-Sen is correct", {
    y <- c(12, 8, 9, 4, 7, 5, 3, 1)
    x <- 1:length(y)
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
    expect_equal(power(wl, n = 10, alpha = 0.05, nsim = 100),
                 0.655, tolerance = 1.0e-3)
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
