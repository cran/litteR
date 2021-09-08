# prevent notes by R CMD check
globalVariables(c("s", "v", "Z"))


#' p-value
#'
#' Extract p-value.
#'
#' @param x object
#' @param \dots further arguments passed to or from other methods.
#' @return p-value of a test (numeric vector of length 1).
#'
#' @export
p_value <- function(x, ...) {
    UseMethod("p_value", x)
}



#' Test Statistic
#'
#' Extract test_statistic.
#'
#' @param x object
#' @param \dots further arguments passed to or from other methods.
#' @return test statistic of a test (numeric vector of length 1).
#'
#' @seealso \code{\link{test_statistic.wilcoxon}},
#'   \code{\link{test_statistic.mann_kendall}}
#'
#' @export
test_statistic <- function(x, ...) {
    UseMethod("test_statistic", x)
}



#' Slope
#'
#' Extract slope.
#'
#' @param x object
#' @param \dots further arguments passed to or from other methods.
#' @return estimate of the slope (numeric vector of length 1).
#'
#' @export
slope <- function(x, ...) {
    UseMethod("slope", x)
}



#' Intercept
#'
#' Extract the intercept from object \code{x}.
#'
#' @param x object
#' @param \dots further arguments passed to or from other methods.
#' @return estimate of the intercept (numeric vector of length 1).
#'
#' @export
intercept <- function(x, ...) {
    UseMethod("intercept", x)
}



#' Mann Kendall
#'
#' Performs Mann-Kendall non-parametric test for trend.
#'
#' @param x numeric vector representing a time-series.
#' @param t time index
#' @param type direction to test (both, increasing, or decreasing).
#'
#' @return object of class \code{Mann-Kendall}.
#' @importFrom stats cor.test
#' @export
#'
#' @seealso \code{\link{test_statistic}}, \code{\link{p_value}},
#'   \code{\link{cor.test}}, \code{\link{regional_kendall}}
#'
#' @examples
#'
#' # create mann_kendall object
#' mk <- mann_kendall(c(9, 4, 7, 5, 3), type = "decreasing")
mann_kendall <- function(x, t = seq_along(x),
                         type = c("both", "increasing", "decreasing")) {
    n <- length(x)
    stopifnot(length(t) == n)
    result <- list(
        p_value = NA_real_,
        tau = NA_real_
    )
    class(result) <- "mann_kendall"
    if (n >= 2L) {
        type <- match.arg(type)
        alternative <- switch(type,
            both = "two.sided",
            increasing = "greater",
            decreasing = "less"
        )
        mk <- suppressWarnings(
            cor.test(
                x = t,
                y = x,
                alternative = alternative,
                method = "kendall"
            )
        )
        result$p_value <- mk$p.value
        result$tau <- as.double(mk$estimate)
    }
    result
}

#' @export
print.mann_kendall <- function(x, ...) {
    cat("Object of class", sQuote(class(x)), "\n")
    cat("tau:", test_statistic(x), "\n")
    cat("p-value:", p_value(x), "\n")
}


#' @describeIn mann_kendall Extracts Mann Kendall tau
#'
#' @export
#'
#' @examples
#'
#' # get test statistic tau
#' test_statistic(mk)
test_statistic.mann_kendall <- function(x, ...) {
    x$tau
}


#' @describeIn mann_kendall Extract p-value
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @export
#'
#' @examples
#'
#' # get p-value
#' p_value(mk)
p_value.mann_kendall <- function(x, ...) {
    x$p_value
}







#' Theil Sen Slope Estimator
#'
#' @param x time vector (\code{numeric}, or \code{Date}).
#' @param y numeric value.
#' @param \dots further arguments passed to or from other methods.
#'
#' @references \url{https://en.wikipedia.org/wiki/Theil-Sen_estimator}
#' @return object of class \code{Theil_Sen}.
#' @importFrom stats median
#'
#' @export
#' @examples
#'
#' # create theil_sen object
#' ts <- theil_sen(1:5, c(1, 2, 3, 5, 9))
theil_sen <- function(x, y, ...) {
    n <- length(x)
    if (length(y) != n) {
        stop("x and y should have the same length", call. = FALSE)
    }
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
    result <- list(
        x = x,
        y = y,
        b1 = NA_real_
    )
    class(result) <- "theil_sen"
    if (n >= 2L) {
        x <- as.numeric(x)
        N <- 0.5 * n * (n - 1L)
        slope <- rep.int(NA_real_, times = N)
        k <- 0L
        for (i in seq_len(n - 1)) {
            for (j in (i + 1):n) {
                dx <- x[j] - x[i]
                if (dx > .Machine$double.eps) {
                    dy <- y[j] - y[i]
                    k <- k + 1L
                    slope[k] <- dy / dx
                }
            }
        }
        slope <- slope[seq_len(k)]
        result$b1 <- median(slope, ...)
    }
    result
}



#' @export
print.theil_sen <- function(x, ...) {
    cat("Object of class", class(x), "\n")
    cat("intercept:", intercept(x), "\n")
    cat("slope:", slope(x), "\n")
}



#' @describeIn theil_sen Extract slope.
#'
#' @export
#'
#' @examples
#'
#' # get slope
#' slope(ts)
slope.theil_sen <- function(x, ...) {
    x$b1
}


#' @describeIn theil_sen Extract intercept.
#'
#' @export
#'
#' @examples
#'
#' # get intercept
#' intercept(ts)
intercept.theil_sen <- function(x, ...) {
    median(x$y - x$b1 * as.numeric(x$x), ...)
}



#' Coefficient of Variation
#'
#' @param x a numeric vector
#' @param na.rm logical. Should missing values be removed?
#' @return coefficient of variation (numeric vector of length 1).
#'
#' @references \url{https://en.wikipedia.org/wiki/Coefficient_of_variation}
#' @importFrom stats sd
#' @export
cv <- function(x, na.rm = FALSE) {
    d <- mean(x, na.rm = na.rm)
    if (d == 0) {
        return(NA_real_)
    }
    n <- sd(x, na.rm = na.rm)
    return(n / d)
    
}

#' Relative Median Absolute Deviation
#'
#' This is the Median Absolute Deviation divided by the median and
#' is similar to the coefficient of variation.
#'
#' @param x a numeric vector
#' @param na.rm logical. Should missing values be removed?
#' @return Relative median absolute deviation (numeric vector of length 1).
#'
#' @references \url{https://en.wikipedia.org/wiki/Median_absolute_deviation}
#' @importFrom stats mad median
#' @export
rmad <- function(x, na.rm = FALSE) {
    d <- median(x, na.rm = na.rm)
    if (d == 0) {
        return(NA_real_)
    }
    n <- mad(x, na.rm = na.rm)
    return(n / d)
}


#' Index of Dispersion
#'
#' A normalized measure of the dispersion of a probability distribution.
#'
#' @param x a numeric vector
#' @param na.rm logical. Should missing values be removed?
#' @return index of dispersion (numeric vector of length 1).
#'
#' @references \url{https://en.wikipedia.org/wiki/Index_of_dispersion}
#' @importFrom stats var
#' @export
iod <- function(x, na.rm = FALSE) {
    var(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}



#' Rolling Statistics
#'
#' Applies function \code{fun} within a rolling (moving) window
#' of size \code{w} to vector numeric vector \code{x}.
#'
#' @param x numeric vector (time-series)
#' @param w width of moving window
#' @param fun function to be applied
#'
#' @return vector of length length(x)-w
#' @export
roll <- function(x, w = 3, fun = mean) {
    n <- length(x)
    stopifnot(n >= w)
    w <- w - 1L
    r <- numeric(n - w)
    for (i in seq_along(r)) {
        r[i] <- fun(x[i:(i + w)])
    }
    r
}






#' Sample From an ECDF
#'
#' Type stable implementation of an Empirical Cumulative Distribution
#' Function (ECDF) sampler.
#'
#' @param x numeric vector
#' @param n number of draws
#'
#' @return vector of \code{n} elements of the same type as \code{x}
#'
#' @importFrom stats ecdf quantile runif
#' @seealso \code{\link{ecdf}}
#'
#' @export
#'
#' @examples recdf(1:5, 10)
recdf <- function(x, n) {
    r <- x %>%
        ecdf %>%
        quantile(runif(n), names = FALSE)
    if (is.integer(x)) {
        r <- r %>%
            round %>%
            as.integer
    }
    r
}


#' Wilcoxon Test
#'
#' Constructor for a Wilcoxon test (simple wrapper for \code{\link{wilcox.test}}).
#'
#' @param x numeric vector representing a time-series.
#' @param type direction to test (both, increasing, or decreasing).
#' @param mu baseline value (null hypothesis)
#'
#' @return object of class \code{wilcoxon}.
#' @importFrom stats wilcox.test
#' @export
#'
#' @seealso \code{\link{wilcox.test}}, \code{\link{p_value}},
#'   \code{\link{test_statistic}}
#'
#' @examples
#'
#' # create wilcoxon object
#' w <- wilcoxon(c(9, 4, 7, 5, 3), type = "less")
wilcoxon <- function(x, type = c("both", "greater", "less"), mu = 0) {
    result <- list(
        p_value = NA_real_,
        V = NA_real_,
        x = x,
        type = type,
        mu = mu
    )
    class(result) <- "wilcoxon"
    if (length(x) >= 2L) {
        type <- match.arg(type)
        type[type == "both"] <- "two.sided"
        w <- suppressWarnings(
            wilcox.test(
                x = x,
                alternative = type,
                mu = mu)
        )
        result$p_value <- w$p.value
        result$V <- as.double(w$statistic)
    }
    result
}


#' @export
print.wilcoxon <- function(x, ...) {
    cat("Object of class", class(x), "\n")
    cat("V:", test_statistic(x), "\n")
    cat("p-value:", p_value(x), "\n")
}


#' @describeIn wilcoxon Extract test statistic V
#'
#' @export
#'
#' @examples
#'
#' # get test statistic V
#' test_statistic(w)
test_statistic.wilcoxon <- function(x, ...) {
    x$V
}


#' @describeIn wilcoxon Extract p-value
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @export
#'
#' @examples
#'
#' # get p-value
#' p_value(w)
p_value.wilcoxon <- function(x, ...) {
    x$p_value
}


#' Tukey's Trimean
#'
#' Robust centrality measure estimated as the weighted average of
#' the three quartiles: \eqn{(Q_1 + 2Q_2 + Q_3) / 4}, where
#' \eqn{Q_1, Q_2} and \eqn{Q_3} are the first, second and third
#' quartiles respectively.
#'
#' @param x numeric vector
#' @param \dots further arguments passed to or from other methods.
#' @return trimean (numeric value of length 1).
#'
#' @references \url{https://en.wikipedia.org/wiki/Trimean}
#'
#' @export
trimean <- function(x, ...) {
    UseMethod("trimean", x)
}

#' @describeIn trimean Tukey's trimean
#'
#' @examples
#' stopifnot(trimean(0:100) == mean(0:100))
#' stopifnot(trimean(0:100) == median(0:100))
#'
#' @export
trimean.default <- function(x, ...) {
    x %>%
        quantile(probs = c(0.25, 0.50, 0.50, 0.75)) %>%
        mean
}





#' Medcouple
#'
#' Robust statistic that quantifies the skewness of univariate distributions.
#'
#' @param x numeric vector
#' @param \dots further arguments passed to or from other methods.
#' @return medcouple (numeric vector of length 1).
#' @note This is a naive, but robust en simple implementation. For a more
#' efficient implementation see package
#' \href{https://CRAN.R-project.org/package=robustbase}{robustbase} and the
#' references section below.
#'
#' @references Brys, G., M. Hubert, A. Struyf, 2004.
#'   A Robust Measure of Skewness.
#'   Journal of Computational and Graphical Statistics 13: 996-1017.
#'   \doi{10.1198/106186004X12632}.
#'
#' @export
medcouple <- function(x, ...) {
    UseMethod("medcouple", x)
}



#' @describeIn medcouple default method
#'
#' @importFrom purrr %>% chuck
#' @export
medcouple.default <- function(x, ...) {

    # define absolute tolerance
    eps <- 10 * .Machine %>%
        chuck("double.eps")

    # improve efficiency by subtracting the median a priori
    # do not scale to prevent artefacts
    Z <- x - median(x)
    Z_m <- Z[Z <= 0]
    Z_p <- Z[Z >= 0]

    # apply kernel
    k <- 0L
    h <- numeric(length = length(Z_m) * length(Z_p))
    for (z_p in Z_p) {
        for (z_m in Z_m) {
            k <- k + 1L
            z_pt <- abs(z_p) < eps
            z_mt <- abs(z_m) < eps
            if (z_pt && z_mt) {
                h[k] <- 0
                next
            }
            if (z_pt) {
                h[k] <- -1
                next
            }
            if (z_mt) {
                h[k] <- 1
                next
            }
            h[k] <- (z_p + z_m) /
                    (z_p - z_m)
        }
    }

    # return medcouple
    median(h)
}



#' Adjusted Boxplot Statistics
#'
#' Adjusted boxplot statistics according to Hubert & Vandervieren (2008).
#' The upper whisker extends from the hinge to the largest value no further
#' than the upper fence. Similarly, the lower whisker extends from the hinge to
#' the smallest value no further than the lower fence. See Hubert &
#' Vandervieren (2008, p.5191, Eq.5).
#'
#' @param x numeric vector
#' @param \dots further arguments passed to or from other methods.
#'
#' @return Numeric vector consisting of respectively the lower
#'   whisker/fence, the first quartile/hinge, the median, the third
#'   quartile/hinge, and the upper whisker/fence.
#'
#' @references Hubert, M., and E. Vandervieren, 2008.
#' An adjusted boxplot for skewed distributions.
#' Computational Statistics and Data Analysis 52:5186-5201
#' \doi{10.1016/j.csda.2007.11.008}
#'
#' @export
adj_boxplot_stats <- function(x, ...) {
    UseMethod("adj_boxplot_stats", x)
}



#' @describeIn adj_boxplot_stats Adjusted Boxplot Statistics
#'
#' @importFrom purrr %>%
#' @importFrom stats quantile
#' @seealso \code{\link{stat_adj_boxplot}}
#'
#' @examples
#' adj_boxplot_stats(rlnorm(100))
#' @export
adj_boxplot_stats.default <- function(x, ...) {
    Q <- x %>%
        quantile(probs = c(0.25, 0.50, 0.75), names = FALSE)
    IQR <- Q[3] - Q[1]
    MC <- x %>%
        medcouple
    if (MC < 0) {
        result <- c(
            max(min(x), Q[1] - 1.5 * exp(-3 * MC) * IQR),
            Q,
            min(max(x), Q[3] + 1.5 * exp( 4 * MC) * IQR)
        )
    } else {
        result <- c(
            max(min(x), Q[1] - 1.5 * exp(-4 * MC) * IQR),
            Q,
            min(max(x), Q[3] + 1.5 * exp( 3 * MC) * IQR)
        )
    }
    names(result) <- c("ymin", "lower", "middle", "upper", "ymax")
    result
}



#' Adjusted Boxplot Statistics for ggplot2
#'
#' Computes adjusted boxplot statistics to be used by \code{ggplot2}.
#' See Hubert & Vandervieren (2008, p.5191, Eq.5).
#'
#' @examples
#' library(ggplot2)
#'
#' d <- data.frame(x = gl(2, 50), y = rnorm(100))
#' ggplot(data = d, mapping = aes(x = x, y = y)) +
#'    stat_adj_boxplot()
#'
#' @references Hubert, M., and E. Vandervieren, 2008.
#' An adjusted boxplot for skewed distributions.
#' Computational Statistics and Data Analysis 52:5186-5201
#' \doi{10.1016/j.csda.2007.11.008}
#'
#' @seealso \code{\link{adj_boxplot_stats}}, \code{\link{stat_adj_boxplot_outlier}}
#'
#' @export
stat_adj_boxplot <- function() {
    stat_summary(fun.data = adj_boxplot_stats, geom = "boxplot")
}



#' @describeIn stat_adj_boxplot add outliers to adjusted boxplot
#' @export
stat_adj_boxplot_outlier <- function() {
    outlier <- function(x) {
        if (length(x) == 1) {
            return(NA_real_)
        }
        s <- adj_boxplot_stats(x)
        x[x < s["ymin"] | x > s["ymax"]]
    }
    stat_summary(fun = outlier, geom = "point", na.rm = TRUE)
}



#' Mann-Kendall S Statistic
#' 
#' @param x observations
#' @param t time index
#'
#' @references Gilbert, R.O., 1987. Statistical methods for environmental
#' pollution monitoring.
#' 
#' @seealso \code{\link{kendall_var_s}}
#'
#' @export
kendall_s <- function(x, t = seq_along(x)) {
    stopifnot(length(x) == length(t))
    o <- order(t)
    x <- x[o]
    t <- t[o]
    n <- length(x)
    if (n == 1L) {
        # corner case (same return type as 'sign')
        return(NA_real_)
    }
    s <- 0
    for (i in seq_len(n - 1)) {
        for (j in (i + 1):n) {
            if (t[i] != t[j]) {
                s <- s + sign(x[j] - x[i])
            }
        }
    }
    return(s)
}



#' Mann-Kendall Variance of S Statistic
#' 
#' @param x observations
#' @param t time index
#'
#' @references Gilbert, R.O., 1987. Statistical Methods for Environmental
#' Pollution Monitoring.
#' @references Van Belle and Hughes, 1984, Nonparametric Tests for Trend in 
#' Water Quality. Water Resources Research 20:127-136
#'
#' @export
kendall_var_s <- function(x, t = seq_along(x)) {

    # check arguments
    n <- length(x)
    stopifnot(length(t) == n)

    # handle corner cases
    if (n < 3L) {
        return(NA_real_)
    }

    # make sure that vectors are ordered
    o <- order(t)
    x <- x[o]
    t <- t[o]

    # utility functions
    d <- function(x) {
        x * (x - 1)
    }
    e <- function(x) {
        d(x) * (x - 2)
    }
    f <- function(x) {
        d(x) * (2 * x + 5)
    }

    # initialize var S
    s <- f(n)

    # tied values correction
    tp <- tapply(x, x, length)
    tp <- tp[tp > 1L]
    g <- length(tp)
    for (i in seq_len(g)) {
        s <- s - f(tp[i])
    }
    
    # multiple data correction
    uq <- tapply(t, t, length)
    uq <- uq[uq > 1L]
    h <- length(uq)
    for (i in seq_len(h)) {
        s <- s - f(uq[i])
    }

    s <- as.double(s) / 18 +
        (sum(e(tp)) * sum(e(uq))) /
            (9 * e(n)) +
        (sum(d(tp)) * sum(d(uq))) /
            (2 * d(n))

    return(as.double(s))
}



#' Regional Kendall Test for Trend
#' 
#' Performs Regional Kendall non-parametric test for trend.
#' 
#' @param x observations
#' @param t time index
#' @param r region index
#' @param type direction to test (both, increasing, or decreasing).
#' 
#' @importFrom dplyr tibble "%>%" summarise mutate if_else
#' @importFrom stats pnorm
#' 
#' @seealso \code{\link{mann_kendall}}
#'
#' @references Gilbert, R.O., 1987. Statistical methods for environmental
#' pollution monitoring.
#' 
#' @export
regional_kendall <- function(x, t = seq_along(x), r = rep.int(1, length(x)),
                             type = c("both", "increasing", "decreasing")) {
    type <- match.arg(type)
    n <- length(x)
    stopifnot(length(t) == n)
    stopifnot(length(r) == n)
    result <- tibble(x, t, r) %>%
        group_by(r) %>%
        summarise(
            s = kendall_s(x, t),
            v = kendall_var_s(x, t),
            .groups = "drop") %>%
        summarise(
            s = sum(s),
            v = sum(v)) %>%
        mutate(
            Z = if_else(s == 0, 0, (s - sign(s)) / sqrt(v)),
            p_value = switch(
                type,
                    "decreasing" = pnorm(Z),
                    "increasing" = pnorm(Z, lower.tail = FALSE),
                    2 * pnorm(-abs(Z)))) %>%
        as.list
    class(result) <- "regional_kendall"
    return(result)
}


#' @export
print.regional_kendall <- function(x, ...) {
    cat("Object of class", sQuote(class(x)), "\n")
    cat("Z:", test_statistic(x), "\n")
    cat("p-value:", p_value(x), "\n")
}


#' @describeIn regional_kendall Extracts Regional Kendall Z
#'
#' @export
test_statistic.regional_kendall <- function(x, ...) {
    x$Z
}


#' @describeIn regional_kendall Extract Regional Kendall p-value
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @export
p_value.regional_kendall <- function(x, ...) {
    x$p_value
}