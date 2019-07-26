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
#' @seealso \code{\link{toString}}
#' @export
#' 
#' @note The elements of \code{x} should be unique and in ascending order.
#' 
#' @examples
#' toString(sequenize(c(1:4, 8:9)))
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
toString.sequenized <- function(x, ...) {
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
    if (n == 1L) {
        return(x)
    }
    if (n == 2L) {
        return(str_c(x[1], " and ", x[2]))
    }
    return(str_c(str_c(x[-n], collapse = ", "), " and ", x[n]))
}



#' Test Litter Data by Name
#'
#' Checks if litter names are OSPAR compliant.
#' The OSPAR format consists of a litter category, a specification,
#' and an integer code in the range 000-989 in square brackets. In
#' addition the special code [TA] is allowed to specifiy total abundance.
#' 
#' @param x \code{character} vector to check.
#'
#' @return \code{TRUE} if \code{x} complies with OSPAR, \code{FALSE} if not.
#' 
#' @importFrom stringr str_detect
#' 
#' @export
#'
#' @examples
#' # valid litter type
#' stopifnot(is_type_name("Plastic: Food [6]"))
#' 
#' # invalid litter type: additional punctuation : and + are not allowed 
#' stopifnot(!is_type_name("All cartons/tetrapaks [302:204+62+118]"))
#' 
#' # invalid litter type: numeric litter code is missing
#' stopifnot(!(is_type_name("no litter here")))
#' 
#' # invalid litter type: number greater than 989
#' stopifnot(!(is_type_name("Survey: Remarks [999]")))
#' stopifnot(!is_type_name("[TA]"))
is_type_name <- function(x) {
    str_detect(x, "[a-zA-Z ]*\\[[a-zA-Z]*[0-9]+\\][a-zA-Z ]*") &
        !str_detect(x, "\\[99[0-9]{1}\\]")
}


#' Test For Type Code
#' 
#' Test if \code{x} contains a valid litter type code.
#'
#' @param x \code{character} vector to test
#' @return \code{TRUE} if test passes, \code{FALSE} otherwise.
#' @export
is_type_code <- function(x) {
    str_detect(x, "\\[[a-zA-Z]*[0-9]+\\]") &
        !str_detect(x, "\\[99[0-9]{1}\\]")
}

#' Test For Group Code
#' 
#' Test if \code{x} contains a valid litter group code.
#'
#' @param x \code{character} vector to test
#' @return \code{TRUE} if test passes, \code{FALSE} otherwise.
#' @export
is_group_code <- function(x) {
    str_detect(x, "\\[[A-Z]+\\]")
}


#' Extract Litter Code
#' 
#' Extract litter codes (ASCII characters in square brackets) from character
#' vector \code{x}.
#'
#' @param x \code{character} vector containg litter codes
#'
#' @return litter code (\code{character} vector).
#' 
#' @importFrom dplyr if_else
#' @importFrom stringr str_replace
#' 
#' @export
#'
#' @examples
#' # valid litter type
#' stopifnot(get_type_code("Plastic: Food [6]") == "[6]")
#' stopifnot(get_type_code(c("Plastic: Food [6]", "Plastic: Shoes [44]")) == c("[6]", "[44]"))
#' 
#' # invalid litter type: additional punctuation : and + are not allowed 
#' stopifnot(is.na(get_type_code("All cartons/tetrapaks [302:204+62+118]")))
#' stopifnot(is.na(get_type_code("[TA]")))
get_type_code <- function(x) {
    if_else(
        is_type_name(x),
        str_replace(x, pattern = ".*(\\[[a-zA-Z]*[0-9]+\\]).*",
                    replacement = "\\1"),
        NA_character_
    )
}



#' Extract Litter Group Code
#' 
#' Extracts litter group code (i.e., a code in square brackets),
#' from a \code{character} vector.
#'
#' @param x \code{character} vector containg litter group codes
#'
#' @return \code{character} vector of litter group codes
#' 
#' @importFrom stringr str_replace
#' 
#' @export
#'
#' @examples
#' # valid litter type
#' stopifnot(get_group_code("[TA]") == "[TA]")
#' stopifnot(get_group_code("all kinds of plastic [PLASTIC]") == "[PLASTIC]")
#' stopifnot(is.na(get_group_code("all kinds of plastic [Plastic]")))
get_group_code <- function(x) {
    if_else(
        str_detect(x, pattern = ".*\\[[A-Z]+\\].*"),
        str_replace(x, pattern = ".*(\\[[A-Z]+\\]).*", replacement = "\\1"),
        NA_character_
    )
}



#' Test for Natural Numbers
#'
#' Test for natural numbers according to ISO 80000-2, that is the set  {0, 1, 2, ...}
#' 
#' @param x \code{numeric} vector
#'
#' @return \code{TRUE} in case \code{x} is a natural number, \code{FALSE} otherwise.
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
is_date_format <- function(x, format) {
    suppressWarnings(
        x %>%
            as.character %>%
            parse_date(format) %>%
            is.na %>% {
                !.
            }
    )
}