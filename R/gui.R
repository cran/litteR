#' Graphical User Interface to the litteR-package
#'
#' Starts a graphical user interface for analysing litter data.
#' A Tcl/Tk-dialogue will be started if one or more arguments
#' are missing.
#'
#' @param file file containing litter data (see vignette for details)
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
litter <- function(file = NULL) {

    # check if Tcl/Tk is available
    if (is_null(file)) {
        if (!capabilities("tcltk")) {
            stop(
                "The 'tcltk'-package is not supported on this machine.\n",
                "Please provide a valid filename as function argument\n",
                call. = FALSE
            )
        }
        message("Note: A file dialogue should appear right now.\n",
                "If not, it is likely to be hidden behind other programs.")
        file <- tk_choose.files(
            default = "",
            caption = "Select input file",
            multi = FALSE,
            filters = matrix(data = c("input file", ".csv"), nrow = 1)
        )
        if (length(file) == 0L) {
            message("Selection of the input file has been cancelled.")
            return(invisible(NULL))
        }
    }

    # extract work directory
    dir_output <- file %>%
        path_dir

    # read settings
    pars <- dir_output %>%
        path("settings.yaml") %>%
        read_lines %>%
        yaml.load %>%
        c(file_input = path_norm(file))

    # add path to groups file
    pars$file_groups <- pars %>%
        chuck("file_groups") %>%
        path(dir_output, .)

    # handle specified litter/group type(s)
    pars$litter_types <- pars %>%
        chuck("litter_types") %>%
        str_to_upper %>%
        map_chr(function(x) {
            if_else(
                is_type_code(x) | is_group_code(x),
                x,
                str_c("[", x, "]")
            )
        })

    # concatenate first character of selected modules
    selected_modules <- c("stats", "trend", "baseline", "power") %>%
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

    # construct filename report
    file_report <- path(
        dir_output,
        "litter-report-%s-%s-%s.html" %>%
            sprintf(
                pars %>%
                    chuck("litter_types") %>%
                    str_c(collapse = ""),
                selected_modules %>%
                    str_c(collapse = ""),
                format(Sys.time(), "%Y%m%d-%H%M%S")))

    # construct filename statistics
    pars$file_stats <- file_report %>%
        str_replace("report", "stats") %>%
        str_replace("html$", "csv")

    # create HTML report
    temp_dir <- file_temp("litteR-")
    path_package("litteR", "app") %>%
        dir_copy(temp_dir)
    owd <- setwd(temp_dir)
    on.exit(setwd(owd))
    message("litteR is currently processing your data. ",
            "This may take several minutes...")
    render(
        input = "litter-main.Rmd",
        output_format = html_document(
            toc = TRUE,
            theme = "default",
            css = "litter.css"),
        output_file = file_report,
        params = pars,
        quiet = TRUE)
    message("Finished! All results have been written to:\n",
            sQuote(dir_output))
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
        dir_ls(regexp = "litter-stats-meta", invert = TRUE) %>%
        file_copy(path)
    message("Project directory ", sQuote(path), " created")
}