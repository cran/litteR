#' Simple Logger
#'
#' Logger, in the spirit of loggers like log4j. Implemented logging
#' levels are DEBUG, INFO, WARN, ERROR (in increasing order of
#' specificity. Logging events can be filtered to show only
#' events with a minimum specificity.
#' 
#' @param con \link{connection} to write logging data to
#' @param level log only events of this level and those 
#'   that are more specific (see details)
#' 
#' @return Anonymous logging functions
#' 
#' @importFrom stringr str_c
#' @export
#' 
#' @examples
#'
#' logger <- create_logger(level = "INFO")
#' logger$info("starting specific computation")
#' logger$info("Today is {Sys.Date()}")
create_logger <- function(con = stdout(),
    level = c("DEBUG", "INFO", "WARN", "ERROR")) {
    
    # least specific logging level
    level <- match.arg(level) %>%
        factor(c("DEBUG", "INFO", "WARN", "ERROR"), ordered = TRUE)
    
    # generic logging function
    to_logger <- function(level, ...) {
        cat(
            format(Sys.time()), 
            " [", as.character(level), "] ",
            str_c(...),
            "\n", 
            sep = "",
            file = con, 
            append = TRUE
        )
        flush(con)
    }
    
    # specific logging functions (in order of specificity)
    list(
        debug = function(...) {
            if ("DEBUG" >= level) {
                to_logger("DEBUG", ...)
            }
        }, 
        info = function(...) {
            if ("INFO" >= level) {
                to_logger("INFO", ...)
            }
        },
        warn = function(...) {
            if ("WARN" >= level) {
                to_logger("WARN", ...)
                warning(str_c(..., sep = "\n"), call. = FALSE)
            }
        },
        error = function(...) {
            if ("ERROR" >= level) {
                to_logger("ERROR", ...)
                stop(str_c(..., sep = "\n"), call. = FALSE)
            }
        }
    )
}