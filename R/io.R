# prevent notes by R CMD check
globalVariables(c(".", "group_code", "spatial_code", "type_name", "value", "included"))


#' Validation of LitteR File Formats
#'
#' Generic function for validation of file formats.
#'
#' @param x object to validate
#' @param logger optional logger object (see \code{\link{create_logger}})
#' @param \dots further arguments passed to or from other methods.
validate <- function(x, ...) {
    UseMethod("validate", x)
}


#' @describeIn validate validate litter data.
#' @param type_names character vector of permissible types
#'
#' @return validated object of class \code{wide}
#'
#' @importFrom dplyr "%>%" pull tibble group_by summarise filter n
#' @importFrom purrr discard
#' @importFrom tidyselect all_of
#' @importFrom stringr str_glue
validate.litter <- function(x, type_names, logger = create_logger(level = "INFO"), ...) {

    # create persistent record identifier
    RECORD_ID <- seq_len(nrow(x))

    # set column names and type_names to lower case
    x <- x %>%
        set_names(str_to_lower(names(.)))
    type_names <- type_names %>%
        str_to_lower

    # check required columns
    required_column_names <- c("spatial_code", "date")
    logger$info(str_glue("Check if required metadata columns {enumerate(sQuote(required_column_names))} exist."))
    missing_column_names <- required_column_names %>%
        setdiff(names(x))
    n_missing_column_names <- length(missing_column_names)
    if (n_missing_column_names == 1L) {
        logger$error(str_glue("Column {sQuote(missing_column_names)} is missing in the data file."))
    } 
    if (n_missing_column_names > 1L) {
        logger$error(str_glue("Columns {enumerate(sQuote(missing_column_names))} are missing in the data file."))
    }
    logger$info("All required columns are available")


    logger$info("Checking date format")
    is_valid_ymd <- x %>%
        pull("date") %>%
        is_date_format("%Y-%m-%d")
    is_valid_dmy <- x %>%
        pull("date") %>%
        is_date_format("%d/%m/%Y")
    is_valid <- is_valid_ymd | is_valid_dmy
    if (any(!is_valid)) {
        logger$error(
            "Invalid date or date format found in records: ",
            str_c(enumerate(sequenize(which(!is_valid))), ". "),
            "Please correct the date or use YYYY-mm-dd or dd/mm/YYYY."
        )
    }
    if (all(is_valid_ymd)) {
        logger$info("All dates are ISO 8601 compliant (YYYY-mm-dd)")
        x$date <- x %>% pull("date") %>% as.Date("%Y-%m-%d")
    }
    if (all(is_valid_dmy)) {
        logger$info("All dates are OSPAR compliant (dd/mm/YYYY)")
        x$date <- x %>% pull("date") %>% as.Date("%d/%m/%Y")
    }
    logger$info("Checking consistency of dates")
    if (!(all(is_valid_ymd) | all(is_valid_dmy))) {
        logger$error("Dates should be either YYYY-mm-dd or dd/mm/YYYY but not both")
    }
    if (all(is_valid_ymd)) {
        date_format <- "YYYY-mm-dd"
    } else {
        date_format <- "dd/mm/YYYY"
    }
    logger$info(str_glue("Dates are consistent. All dates are {date_format}"))


    logger$info("Check if all litter types in the type file are present in the data file")
    missing_type_names <- type_names %>% setdiff(names(x))
    if (length(missing_type_names) == 0L) {
        logger$info("All litter types are present")
    } else {
        logger$warn(
            "The following types in the type file are missing in the data file: ",
            enumerate(sQuote(missing_type_names))
        )
    }
    

    logger$info("Select only litter data")
    type_names <- x %>%
        names %>%
        intersect(type_names)
    if (length(type_names) == 0L) {
        logger$error("No columns with litter data found. See the type file for valid litter types.")
    }
    sel <- c(required_column_names, type_names)
    redundant_column_names <- x %>%
        names %>%
        setdiff(sel)
    if (length(redundant_column_names) > 0L) {
        logger$warn(
            "The following columns will be excluded from analysis:",
            redundant_column_names %>% sQuote %>% enumerate)
    }
    x <- x %>%
        select(all_of(sel))
    
    logger$info("Check for empty cells")
    has_empty_cell <- x %>%
        select(all_of(type_names)) %>%
        map_lgl(function(x) {
            any(is.na(x))
        })
    if (any(has_empty_cell)) {
        logger$error(
            "The following data columns contain empty cells:",
            enumerate(names(has_empty_cell)[has_empty_cell])
        )
    }
    logger$info("No empty cells found")

    logger$info("Check that litter counts are numbers")
    no_number <- x %>%
        select(all_of(type_names)) %>%
        select_if(function(x) {
            !all(is.numeric(x))
        }) %>%
        names
    if (length(no_number) != 0L) {
        logger$error(
            "The following data column(s) contain(s) text: ",
            str_c(enumerate(sQuote(no_number)), ". "),
            "Please correct your data file."
        )
    } else {
        logger$info("Only numbers found")
    }
    type_names <- type_names %>%
        setdiff(no_number)
    x <- x %>%
        select(all_of(c(required_column_names, type_names)))

    
    logger$info("Check that litter counts are nonnegative numbers")
    is_negative_number <- x %>%
        select(all_of(type_names)) %>%
        select_if(function(x) {
            any(x < 0)
        }) %>%
        names
    if (length(is_negative_number) != 0L) {
        logger$error(
            "Negative litter counts found in column(s):",
            str_c(enumerate(sQuote(is_negative_number)), ". ")
        )
    } else {
        logger$info("No negative numbers found")
    }

    
    logger$info("Check that litter counts are natural numbers")
    no_natural_number <- x %>%
        select(all_of(type_names)) %>%
        select_if(function(x) {
            !all(is_natural_number(x))
        }) %>%
        names
    if (length(no_natural_number) != 0L) {
        logger$warn(
            "Non-natural numbers (i.e., numbers that are not in 0, 1, 2, ...) ",
            "found in columns:",
            str_c(enumerate(sQuote(no_natural_number)), ". "),
            "Please, verify if these numbers are correct.",
            "These columns will be included in the analysis."
        )
    } else {
        logger$info("Only natural numbers found")
    }


    logger$info("Check if not all litter counts in a single survey (record) are zero")
    all_zero <- x %>%
        select(all_of(type_names)) %>%
        split(1:nrow(.)) %>%
        map_lgl(function(x) {
            all(abs(as.numeric(x)) < .Machine$double.eps)
        })
    if (any(all_zero)) {
        logger$warn(
            "The following record(s) contain(s) only zeroes:",
            str_c(enumerate(RECORD_ID[all_zero]), ". "),
            "Please, verify if this correct.",
            "These record(s) will be included in the analysis."
        )
    } else {
        logger$info("No records found with only zero-counts")
    }
    
    
    logger$info("Check for duplicated records")
    duplicates <- list_duplicates(x)
    if (length(duplicates) > 0) {
        logger$warn(
            "The following records are duplicated: ",
            duplicates %>% 
                map_chr(function(x){
                    enumerate(RECORD_ID[x])}) %>%
                str_c(collapse = "; ") %>%
                str_c(". "),
            "Only the first record of each duplicate will be retained."
        )
        duplicates <- duplicated(x)
        x <- x %>%
            filter(!duplicates)
        RECORD_ID <- RECORD_ID %>%
            discard(duplicates)
    } else {
        logger$info("No duplicated records found")
    }

    # check for records with the same spatial_code/date
    duplicates <- x %>%
        select(all_of(required_column_names)) %>%
        list_duplicates
    if (length(duplicates) > 0) {
        logger$warn(
            "The following records have different counts for the same ",
            "`spatial_code` and `date`:",
            duplicates %>% 
                map_chr(function(x){
                    enumerate(RECORD_ID[x])}) %>%
                str_c(collapse = "; ") %>%
                str_c(". "),
            "All these records will be retained in the analysis."
        )
    } else {
        logger$info("No records with the same spatial_code/date found")
    }
    
    # return validated data
    x %>%
        mutate(.RECORD_ID = RECORD_ID)
}



#' @describeIn validate validate litter_types file
#'
#' @return validated object of class \code{litter_types}
#'
#' @importFrom dplyr "%>%" distinct one_of filter
#' @importFrom purrr map_lgl map_dfc
#' @importFrom readr parse_date
#' @importFrom rlang are_na
#' @importFrom stringr str_glue
validate.litter_types <- function(x, logger = create_logger(level = "INFO"), ...) {

    logger$info("Validating type file")
    names(x) <- str_to_lower(names(x))
    
    logger$info("Checking required columns in type file")
    x <- x %>%
        map_dfc(str_to_lower)
    required_column_names <- c("type_name", "included")
    missing_column_names <- required_column_names %>%
        setdiff(names(x))
    n_missing_column_names <- length(missing_column_names)
    if (n_missing_column_names == 1L) {
        logger$error(str_glue("Column {sQuote(missing_column_names)} is missing"))
    }
    if (n_missing_column_names  > 1L) {
        logger$error(str_glue("Columns {sQuote(enumerate(missing_column_names))} are missing"))
    }
    logger$info("Required columns are available")
    
    logger$info("Checking type names for duplicates")
    if (anyDuplicated(x$type_name)) {
        logger$error("Duplicated type names found. These are not allowed")
    }
    logger$info("No duplicates found")
    
    logger$info("Checking if table cells are either empty or 'x'")
    entry_ok <- x %>%
        select(-starts_with("type_")) %>%
        map_lgl(function(x) {
            all(are_na(x) | (x == "x"))
        })
    if (any(!entry_ok)) {
        logger$error(
            "In the litter type file, only character 'x' is allowed as group selector",
            str_glue("This is not correct for column(s) {enumerate(sQuote(names(entry_ok)[!entry_ok]))}")
        )
    }
    logger$info("All table cells are OK")
    
    x
}


#' @describeIn validate validate settings file
#'
#' @return validated settings (\code{list})
#'
#' @importFrom stringr str_to_lower str_glue
#' @importFrom rlang set_names
#' @importFrom purrr chuck
#' @importFrom dplyr between
#' @importFrom stringr str_glue
validate.settings <- function(x, logger = create_logger(level = "INFO"), ...) {

    # set settings names to lower case
    x <- x %>%
        set_names(str_to_lower(names(.)))

    logger$info("Check optional settings...")
    if (is_null(pluck(x, "date_min"))) {
        x$date_min <- "1900-01-01"
        logger$info(
            str_glue(str_c(
                "Optional setting 'date_min' is missing. ",
                "Its default ({x$date_min}) will be used")))
    }
    if (is_null(pluck(x, "date_max"))) {
        x$date_max <- "2100-12-31"
        logger$info(
            str_glue(str_c(
                "Optional setting 'date_max' is missing. ",
                "Its default ({x$date_max}) will be used.")))
    }
    if (is_null(pluck(x, "percentage_total_count"))) {
        x$percentage_total_count <- 80
        logger$info(
            str_glue(str_c(
                "Optional setting 'percentage_total_count' is missing. ",
                "Its default ({x$percentage_total_count}) will be used.")))
    }
    if (is_null(pluck(x, "figure_quality"))) {
        x$figure_quality <- "low"
        logger$info(
            str_glue(str_c(
                "Optional setting 'figure_quality' is missing. ",
                "Its default ({x$figure_quality}) will be used.")))
    }
    if (is_null(pluck(x, "spatial_code"))) {
        logger$info(
            str_glue(str_c(
                "Optional setting 'spatial_code' is missing. ",
                "Trend plots can't be created")))
    }
    if (is_null(pluck(x, "type_name"))) {
        logger$info(
            str_glue(str_c(
                "Optional setting 'type_name' is missing. ",
                "No trend plots will be created for litter types.")))
    } else {
        x$type_name <- str_to_lower(x$type_name)
    }
    if (is_null(pluck(x, "group_code"))) {
        logger$info(
            str_glue(str_c(
                "Optional setting 'group_code' is missing. ",
                "No trend plots will be created for litter groups.")))
    } else {
        x$group_code <- str_to_upper(x$group_code)
    }


    logger$info("Check existence of required settings...")
    required_column_names <- c("date_min", "date_max",
                              "percentage_total_count", "file_data",
                              "file_types", "figure_quality")
    missing_column_names <- required_column_names %>% setdiff(names(x))
    n_missing_column_names <- length(missing_column_names)
    if (n_missing_column_names == 1L) {
        logger$error(str_glue("Setting {sQuote(missing_column_names)} is missing"))
    } 
    if (n_missing_column_names  > 1L) {
        logger$error(str_glue("Settings {enumerate(sQuote(missing_column_names))} are missing"))
    }
    logger$info("All required settings are available")


    # validate dates
    logger$info("Checking settings 'date_min' and 'date_max'")
    is_valid <- x %>%
        chuck("date_min") %>%
        is_date_format("%Y-%m-%d")
    if (is_valid) {
        x$date_min <- x %>%
            chuck("date_min") %>%
            as.Date("%Y-%m-%d")
    } else {
        logger$error("Setting 'date_min' should be given as YYYY-mm-dd")
    }
    is_valid <- x %>%
        chuck("date_max") %>%
        is_date_format("%Y-%m-%d")
    if (is_valid) {
        x$date_max <- x %>%
            chuck("date_max") %>%
            as.Date("%Y-%m-%d")
    } else {
        logger$error("Setting 'date_max' should be given as YYYY-mm-dd")
    }
    if (x$date_min > x$date_max) {
        logger$error(str_glue("Setting 'date_min' ({x$date_min}) should be earlier than 'date_max' ({x$date_max})"))
    }
    logger$info("Settings 'date_min' and 'date_max' are valid")
    
    
    logger$info("Checking setting 'percentage_total_count'")
    if (!(x$percentage_total_count %>% between(1, 100))) {
        logger$error("Setting 'percentage_total_count' should be within 1-100")
    }
    logger$info("Setting 'percentage_total_count' is valid")

    
    logger$info("Checking setting 'figure_quality'")
    if (!(x$figure_quality %in% c("low", "high"))) {
        logger$error("'Setting 'figure_quality' should be 'low' or 'high'")
    }
    logger$info("Setting 'figure_quality' is valid")
    
    # return settings
    x
}



#' Read Litter Data
#'
#' Reads litter data from various formats. Currently only the
#' OSPAR data snapshot format, and a wide format
#' are supported. See the package vignette for more details.
#'
#' @param filename name of litter file
#' @param logger optional logger object (see \code{\link{create_logger}})
#' @param type_names character vector of allowed type_names
#'
#' @return \code{tibble} with litter data in long format
#' @importFrom readr read_csv parse_number count_fields tokenizer_csv
#' @importFrom dplyr select select_if rename mutate arrange matches
#' @importFrom tidyr pivot_longer
#' @importFrom purrr set_names negate
#' @importFrom stringr str_to_lower
#' @importFrom rlang has_name
#' @importFrom fs file_exists path_file
#' @export
read_litter <- function(filename, logger = create_logger(level = "INFO"), type_names) {

    logger$info(str_glue("Checking existence of {sQuote(filename)}"))
    if (!file_exists(filename)) {
        logger$error(str_glue("file {sQuote(filename)} not found."))
    }
    logger$info(str_glue("{sQuote(filename)} exists"))
    
    logger$info("Checking if CSV-file is comma delimited")
    n <- filename %>%
        count_fields(tokenizer_csv(), n_max = 1L)
    if (n == 1L) {
        logger$error("Incorrect column delimiter is used in the data file. Please use a comma.")
    }
    logger$info("CSV-file is comma delimited")
    
    logger$info("Reading litter data file")
    d <- suppressMessages(read_csv(filename, guess_max = 10000))
    class(d) <- c("litter", class(d))

    # validate and reformat file
    d %>%
        validate(type_names,logger) %>%
        pivot_longer(
            cols = intersect(names(.), type_names),
            names_to = "type_name", values_to = "count") %>%
        mutate(spatial_code = as.character(spatial_code)) %>%     
        arrange(spatial_code, date, type_name)
}



#' Read Type Names
#'
#' Read the file that links type names to group codes
#' See the package vignette for more details.
#'
#'
#' @param filename name of type file
#' @param logger optional logger object (see \code{\link{create_logger}})
#'
#' @return \code{tibble} with look-up-table of type names and group codes
#' @importFrom readr read_csv cols col_character
#' @importFrom fs file_exists path_package
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_glue
#' @export
read_litter_types <- function(filename, logger = create_logger(level = "INFO")) {
    logger$info(str_glue("Checking existence of {sQuote(filename)}"))
    if (!file_exists(filename)) {
        logger$error(str_glue("Type file {sQuote(filename)} not found."))
    }
    logger$info(str_glue("{sQuote(filename)} exists"))
    d <- filename %>%
        read_csv(col_types = cols(.default = col_character()))
    class(d) <- c("litter_types", class(d))
    d %>%
        validate(logger) %>%
        filter(!are_na(included)) %>%
        rename(tc = included) %>%
        pivot_longer(cols = !starts_with("type_"),
                     names_to = "group_code", values_to = "value") %>%
        filter(!are_na(value)) %>%
        select(-value) %>%
        mutate(group_code = str_to_upper(group_code))
}




#' Read Settings File
#'
#' Reads settings file. See tutorial for its format.
#'
#' @param filename name of litter file
#' @param logger optional logger object (see \code{\link{create_logger}})
#'
#' @return validated settings file
#' @importFrom readr read_lines
#' @importFrom yaml yaml.load
#' @export
read_settings <- function(filename, logger = create_logger(level = "INFO")) {
    d <- filename %>%
        read_lines %>%
        yaml.load
    class(d) <- c("settings", class(d))
    d %>%
        validate(logger)
}


