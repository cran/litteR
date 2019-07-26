#' Graphical User Interface to the litteR-package
#'
#' Starts a graphical user interface for analysing litter data. 
#' A Tcl/Tk-dialogue will be started if one or more arguments 
#' are missing.
#' 
#' @param file_settings file containing settings
#' @param file_input file containing litter data
#' @param dir_output directory to store output
#' 
#' @return An HTML-document in which all the litter analysis results 
#' (tables, figures, explanotory text) are reported.
#' 
#' @importFrom tcltk tk_choose.files tk_choose.dir
#' @importFrom rmarkdown render html_document
#' @importFrom purrr "%>%" chuck flatten_chr map_chr
#' @importFrom readr read_lines
#' @importFrom yaml yaml.load
#' @importFrom rlang is_null is_na
#' @importFrom stringr str_c str_sub str_to_upper
#' @importFrom fs path path_norm path_package file_temp dir_copy path_dir
#' 
#' @export
litter <- function(file_settings = NULL, file_input = NULL, dir_output = NULL) {

    # check if Tcl/Tk is available
    if (is_null(file_input) | is_null(file_settings) | is_null(dir_output)) {
        if (!capabilities("tcltk")) {
            stop(
                "The 'tcltk'-package is not supported on this machine.\n",
                "Please provide a valid filename as function argument\n",
                call. = FALSE
            )
        }
    }

    if (is_null(file_settings)) {
        file_settings <- tk_choose.files(
            default = "",
            caption = "Select settings file",
            multi = FALSE,
            filters = matrix(data = c("settings file", ".yaml"), nrow = 1)
        )
        if (length(file_settings) == 0L) {
            message("Selection of the settings file has been cancelled.")
            return(invisible(NULL))
        }
    }
    pars <- file_settings %>%
        read_lines %>%
        yaml.load

    if (is_null(file_input)) {
        file_input <- tk_choose.files(
            default = "",
            caption = "Select input file",
            multi = FALSE,
            filters = matrix(data = c("input file", ".csv"), nrow = 1)
        )
        if (length(file_input) == 0L) {
            message("Selection of the input file has been cancelled.")
            return(invisible(NULL))
        }
    }
    pars <- c(pars, file_input = path_norm(file_input))
    if (is_null(dir_output)) {
        dir_output <- tk_choose.dir(
            default = path_dir(file_settings),
            caption = "Select output directory"
        )
        if (length(dir_output) == 0L) {
            message("Output directory not found.")
            return(invisible(NULL))
        }
        if (is_na(dir_output)) {
            message("Selection of the output directory has been cancelled.")
            return(invisible(NULL))
        }
    }

    pars$litter_type <- pars %>%
        chuck("litter_type") %>%
        str_to_upper %>%
        map_chr(function(x) {
            if_else(
                is_type_code(x) | is_group_code(x),
                x,
                str_c("[", x, "]")
            )
        })

    # concatenate first character of selected modules
    selected_modules <- c("stats", "assessment", "trend",
                          "baseline", "power") %>%
        map_chr(function(x) {
            ret <- ""
            if (pars %>% chuck(str_c("module_", x))) {
                ret <- x %>%
                    str_sub(1, 1) %>%
                    str_to_upper
            }
            ret
        }) %>%
        str_c(collapse = "")

    # construct output file names
    file_report <- path(
        dir_output,
        "litter-report-%s-%s-%s.html" %>%
            sprintf(
                pars %>%
                    chuck("litter_type") %>%
                    str_c(collapse = ""),
                selected_modules %>%
                    str_c(collapse = ""),
                format(Sys.time(), "%Y%m%d-%H%M%S")))
    pars$file_stats <- file_report %>%
        str_replace("report", "stats") %>%
        str_replace("html$", "csv")

    # create HTML report
    temp_dir <- file_temp("litteR-")
    path_package("litteR", "app") %>%
        dir_copy(temp_dir)
    owd <- setwd(temp_dir)
    on.exit(setwd(owd))
    render(
        input = "litter-main.Rmd",
        output_format = html_document(
            toc = TRUE,
            theme = "default",
            css = "litter.css"),
        output_file = file_report,
        params = pars,
        quiet = FALSE)
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
        dir_ls(regexp = "litter-stats-meta", invert = TRUE) %>%
        file_copy(path)
    message("Project directory ", sQuote(path), " created")
}