#' Graphical User Interface to the litteR-package
#'
#' Starts a graphical user interface for analysing litter data.
#' A Tcl/Tk-dialogue will be started if one or more arguments
#' are missing.
#'
#' @param filename name of file containing settings (see vignette for details)
#'
#' @details For details, see our vignette by typing: vignette("litter-manual")
#'
#' @return An HTML-document in which all the litter analysis results
#' (tables, figures, explanatory text) are reported.
#'
#' @importFrom tcltk tk_choose.files tk_choose.dir
#' @importFrom rmarkdown render html_document
#' @importFrom purrr "%>%" chuck pluck flatten_chr map_chr
#' @importFrom readr read_lines
#' @importFrom rlang is_null is_na
#' @importFrom stringr str_c str_sub str_subset str_to_upper str_glue
#' @importFrom fs path path_norm path_package path_dir path_ext path_ext_set dir_create
#'   file_temp dir_copy file_copy
#'
#' @export
litter <- function(filename = NULL) {

    # read settings file
    if (is_null(filename)) {
        if (!capabilities("tcltk")) {
            stop(
                "The 'tcltk'-package is not supported on this machine.\n",
                "Please provide a valid filename as function argument.",
                call. = FALSE
            )
        }
        message("Note: A file dialogue should appear right now.\n",
                "If not, it is likely to be hidden behind other programs.")
        filename <- tk_choose.files(
            default = "",
            caption = "Select settings file",
            multi = FALSE,
            filters = matrix(data = c("settings file", ".yaml"), nrow = 1)
        )
        if (length(filename) == 0L) {
            message("Selection of the settings file has been cancelled.")
            return(invisible(NULL))
        }
    } else {
        extension <- filename %>%
            path_ext %>%
            str_to_lower
        if (extension != "yaml") {
            stop(
                "file should have yaml as extension",
                call. = FALSE
            )
        }
    }

    # create output directory
    timestamp <- Sys.time() %>%
        format("%Y%m%dT%H%M%S")
    dir_input <- filename %>%
        path_dir
    dir_output <- dir_input %>%
        path(str_glue("litteR-results-{timestamp}")) %>%
        dir_create

    # create, initialize, and finalize logger
    con <- dir_output %>%
        path(str_glue("litteR-log-{timestamp}.log")) %>%
        file(open = "wt")
    logger <- create_logger(con)
    logger$info("Starting a new litteR session")
    logger$info(str_glue("litteR version: {packageVersion('litteR')}"))
    logger$info(str_glue("litteR release date: {packageDate('litteR')}"))
    on.exit(setwd(dir_output), add = TRUE)
    on.exit(logger$info("litteR session terminated"), add = TRUE)
    on.exit(close(con), add = TRUE)

    # read settings
    logger$info(str_glue("Reading settings file {sQuote(path_file(filename))}")) 
    pars <- filename %>%
        read_settings(logger) %>%
        c(file_settings = path_norm(filename))
    logger$info(str_glue("Settings file has been read"))
    
    # add path to data file
    pars$file_data <- pars %>%
        chuck("file_data") %>%
        path(dir_input, .)
    if (!file_exists(pars$file_data)) {
        logger$error(str_glue("Data file {sQuote(pars %>% chuck('file_data'))} not found"))
    }

    # add path to type file
    pars$file_types <- pars %>%
        chuck("file_types") %>%
        path(dir_input, .)
    if (!file_exists(pars$file_data)) {
        logger$error(str_glue("Type file {sQuote(pars %>% chuck('file_types'))} not found"))
    }

    logger$info("Constructing filename for report")
    file_report <- dir_output %>%
        path(str_glue("litteR-results-{timestamp}.html"))
    logger$info(str_glue("Filename {sQuote(path_file(file_report))} created"))
    
    # construct filename for statistics
    logger$info("Construct filename for storing statistics")
    pars$file_stats <- dir_output %>%
        path(sprintf("litteR-results-%s.csv", timestamp))
    logger$info(str_glue("Filename {sQuote(path_file(pars$file_stats))} created"))

    # create HTML report
    logger$info("Starting litter analysis")
    temp_dir <- file_temp("litteR-")
    path_package("litteR", "app") %>%
        dir_copy(temp_dir)
    setwd(temp_dir)
    message("litteR is currently processing your data. ",
            "This may take a few minutes...")
    render(
        input = "litter-main.Rmd",
        output_format = html_document(
            toc = TRUE,
            theme = "default",
            css = "litter.css"),
        output_file = file_report,
        params = pars,
        quiet = TRUE)
    logger$info("Report completed")
    pars$file_data %>% 
        file_copy(dir_output)
    pars$file_types %>% 
        file_copy(dir_output)
    pars$file_settings %>% 
        file_copy(dir_output)
    logger$info(str_glue("All results have been written to {sQuote(dir_output)}"))
    message(str_glue("Finished! All results have been written to {sQuote(dir_output)}"))
    message(str_glue("See also 'litteR-log-{timestamp}.log' for detailed runtime information."))
}



#' Create Project Directory
#'
#' Fills an empty directory (\code{path}) with example files. If the
#' \code{path}' argument is missing or \code{NULL},
#' a Tcl/Tk dialogue will be started.
#'
#' @param path (Existing) directory name
#'
#' @importFrom tcltk tk_choose.files tk_choose.dir
#' @importFrom fs path_package dir_exists dir_ls file_copy path_dir
#' @importFrom rlang is_null
#'
#' @export
create_litter_project <- function(path = NULL) {

    # check if Tcl/Tk is available
    if (is_null(path) && !capabilities("tcltk")) {
        stop(
            "The 'tcltk'-package is not supported on this machine.\n",
            "Please provide a valid filename as function argument\n",
            call. = FALSE
        )
    }

    # select project directory
    if (is_null(path)) {
        message("Note: A file dialogue should appear right now.\n",
                "If not, it is likely to be hidden behind other programs.")
        path <- tk_choose.dir(caption = "Select project directory")
        if (length(path) == 0L) {
            message("Project directory not found.")
            return(invisible(NULL))
        }
        if (is_na(path)) {
            message("Selection of project directory has been cancelled.")
            return(invisible(NULL))
        }
    } else {
        !dir_exists(path) &&
            stop(
                "Project directory ", sQuote(path), " not found",
                call. = FALSE)
    }

    # check if project directory is empty
    path %>%
        dir_ls %>%
        length &&
        stop("Project directory ", sQuote(path),
             " should be empty", call. = FALSE)

    # populate project directory with example files
    path_package("litteR", "extdata") %>%
        dir_ls %>%
        file_copy(path)
    message("Project directory ", sQuote(path), " created")
}