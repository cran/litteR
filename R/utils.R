#' Sequenize Objects
#'
#' Generic function for sequenizing objects
#'
#' @param x object to sequenize
#' @param \dots further arguments passed to or from other methods.
#'
#' @seealso \code{\link{sequenize.integer}}
#' @export
sequenize <- function(x, ...) {
    UseMethod("sequenize", x)
}


#' Sequenize Integer Sequence
#'
#' Compression of integer sequences to 'start-end' notation.
#' For instance \code{c(1:5, 8:9)} becomes \code{"1-5, 8-9"}.
#'
#' @param x vector of integers.
#' @param \dots further arguments passed to or from other methods.
#'
#' @importFrom dplyr lag
#'
#' @return object of class \code{sequenized}
#'
#' @export
#'
#' @note The elements of \code{x} should be unique and in ascending order.
#'
#' @examples
#' sequenize(c(1:4, 8:9))
sequenize.integer <- function(x, ...) {
    n <- length(x)
    if (!all(order(x) == seq_len(n))) {
        stop(
            "the elements in x should be in ascending order",
            call. = FALSE
        )
    }
    if (length(unique(x)) != n) {
        stop(
            "the elements in x should be unique",
            call. = FALSE
        )
    }
    delta <- diff(x)
    i2 <- c(which(delta > 1L), n)
    i1 <- lag(i2, default = 0L) + 1L
    result <- list(from = x[i1], to = x[i2])
    class(result) <- "sequenized"
    result
}

#' Convert Sequenized Output to Character String
#'
#' @param x object of class \code{sequenized}.
#' @param \dots further arguments passed to or from other methods.
#' @return string representation (character vector of length 1) of
#' a sequenized object
#' @seealso \code{\link{sequenize.integer}}
#' @export
#'
#' @importFrom dplyr if_else
#' @importFrom stringr str_c
enumerate.sequenized <- function(x, ...) {
    str_c(
        if_else(
            x$from == x$to,
            str_c(x$from),
            str_c(x$from, x$to, sep = "-")
        ),
        collapse = ", "
    )
}





#' Enumerate Objects
#'
#' Generic function for enumerating objects
#'
#' @param x object to enumerate
#' @param \dots further arguments passed to or from other methods.
#' @seealso \code{\link{enumerate.character}}
#' @export
enumerate <- function(x, ...) {
    UseMethod("enumerate", x)
}


#' Enumerate Character Vector
#'
#' Collapsing a \code{character} vector of length n, to a
#' \code{character} vector of length 1.
#'
#' @param x character vector
#' @param \dots further arguments passed to or from other methods.
#'
#' @return \code{character} vector of length 1, with elements
#' separated by a comma except for the last element which
#' is prepended by "and".
#'
#' @examples
#' enumerate("apples")
#' enumerate(c("apples", "oranges"))
#' enumerate(c("apples", "oranges", "pears"))
#' @export
enumerate.character <- function(x, ...) {
    n <- length(x)
    if (n <= 1L) {
        return(x)
    }
    if (n == 2L) {
        return(str_c(x[1], " and ", x[2]))
    }
    return(str_c(str_c(x[-n], collapse = ", "), " and ", x[n]))
}



#' @describeIn enumerate enumerate \code{numeric} vector.
#' @export
enumerate.numeric <- function(x, ...) {
    enumerate(format(x, ...))
}



#' Test for Natural Numbers
#'
#' Test for natural numbers according to ISO 80000-2, that is the set  {0, 1, 2, ...}
#'
#' @param x \code{numeric} vector
#'
#' @return \code{TRUE} in case \code{x} is a natural number,
#'   \code{FALSE} otherwise.
#'
#' @examples
#' stopifnot(!is_natural_number(3.1))
#' stopifnot(!is_natural_number(2.99))
#' stopifnot(is_natural_number(3))
#' stopifnot(all(is_natural_number(0:9)))
#' stopifnot(sum(is_natural_number(c(1, 2.5, 3))) == 2)
#'
#' @export
is_natural_number <- function(x) {
    if (!is.numeric(x)) {
        return(rep.int(FALSE, length(x)))
    }
    epsilon <- .Machine$double.eps
    !is.na(x) & (x > -epsilon) & (abs(x - round(x)) < epsilon)
}



#' Check Date Format
#'
#' Checks if the data format \code{x} complies with \code{format}.
#'
#' @param x object of class \code{character} or \code{Date}
#' @param format required date format (see \code{\link{strptime}})
#'
#' @return \code{TRUE} if \code{x} complies with \code{format},
#' and \code{FALSE} otherwise.
#' @examples
#' is_date_format("2019-05-14", "%Y-%m-%d")
#' @export
is_date_format <- function(x, format = "%Y-%m-%d") {
    suppressWarnings(
        x %>%
            as.character %>%
            parse_date(format) %>%
            is.na %>% {
                !.
            }
    )
}



#' List Duplicates
#'
#' Lists all duplicates as a list of tuples.
#'
#' @param x object of class \code{\link{character}}, \code{\link{tibble}} 
#'   or \code{\link{data.frame}})
#' @param \dots further arguments passed to or from other methods.
#'
#' @return \code{\link{list}} of row numbers with duplicates
#' @export
list_duplicates <- function(x, ...) {
    UseMethod("list_duplicates", x)
}


#' @importFrom purrr compact
#' @describeIn list_duplicates list duplicates for a \code{\link{character}} vector.
#' @examples
#' list_duplicates(c("a", "b", "c")) # list()
#' list_duplicates(c("a", "b", "a", "c")) # list(c(1, 3))
#' @export
list_duplicates.character <- function(x, ...) {
    n <- length(x)
    active <- rep.int(x = TRUE, times = n)
    result <- vector(mode = "list", length = n)
    if (n <= 1L) {
        return(NULL)
    }
    for (i in 1:(n-1)) {
        r <- i
        for (j in (i+1):n) {
            if (active[j] && (x[i] == x[j])) {
                active[j] <- FALSE
                r <- c(r, j)
            }
        }
        if (length(r) > 1L) {
            result[[i]] <- r
        }
    }
    compact(result)
}

#' @describeIn list_duplicates lists duplicates for a \code{\link{tibble}}.
#' @importFrom purrr map map_chr
#' @importFrom stringr str_c str_replace_na
#' @export
list_duplicates.tbl <- function(x, ...) {
    x %>%
        split(seq_len(nrow(.))) %>%
        map(str_replace_na) %>%
        map_chr(str_c, collapse = "") %>%
        list_duplicates
}

#' @describeIn list_duplicates lists duplicates for a \code{\link{data.frame}}.
#' @importFrom dplyr as_tibble
#' @export
list_duplicates.data.frame <- function(x, ...) {
    x %>%
        as_tibble %>%
        list_duplicates
}
