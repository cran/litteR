# prevent notes by R CMD check
globalVariables(c(".", "location_name", "date", "Beach name", "Survey date",
                  "type_name", "TYPE_NAME", "country_name", "country_code",
                  "country", "region", "region_name", "location_code", "refno",
                  "abundance", "value"))


#' Detect File Signature
#'
#' Generic function for detecting the file signature.
#'
#' @param x object of which the signature has to be determined
#' @param \dots further arguments passed to or from other methods.
#' @return Detected file signature. Currently "OSPAR", "long", and "wide"
#'         are recognized. In case of an unknown file signature, "unknown"
#'         is returned (character vector of length 1).
#' @export
get_signature <- function(x, ...) {
    UseMethod("get_signature", x)
}

#' @describeIn get_signature detect file signature from \code{character}.
#' @importFrom stringr str_to_lower
#' @export
get_signature.character <- function(x, ...) {
    x <- str_to_lower(x)
    ospar_column_names <- c("refno", "beach name", "country",
                            "region", "survey date")
    if (all(ospar_column_names %in% x)) {
        return("OSPAR")
    }
    long_column_names <- c("region_name", "country_code", "country_name",
                           "location_name",
                           "date", "type_name", "abundance")
    if (all(long_column_names %in% x)) {
        return("long")
    }
    wide_column_names <- c("region_name", "country_code", "country_name",
                           "location_name", "date")
    if (all(wide_column_names %in% x)) {
        return("wide")
    }
    return("unknown")
}

#' @describeIn get_signature detect file signature from a tibble (\code{tbl}).
#' @importFrom stringr str_to_lower
#' @export
get_signature.tbl <- function(x, ...) {
    x %>%
        names %>%
        get_signature
}

#' @describeIn get_signature detect file signature from \code{fs_path}.
#' @importFrom stringr str_to_lower
#' @export
get_signature.fs_path <- function(x, ...) {
    suppressMessages(
        x %>%
            read_csv %>%
            get_signature
    )
}


#' Signature from Files
#'
#' Generic function for retrieving the file signature
#'
#' @param x object of which the signature has to be determined
#' @param \dots further arguments passed to or from other methods.
as_internal_format <- function(x, ...) {
    UseMethod("as_internal_format", x)
}


#' @describeIn as_internal_format convert OSPAR data to internal representation.
#'
#' @importFrom dplyr "%>%" select arrange mutate if_else
#' @importFrom tidyr gather
as_internal_format.ospar <- function(x, ...) {
    x %>%
        gather(key = "type_name", value = "abundance",
               names(.)[is_type_name(names(.))]) %>%
        rename(
            region_name = region,
            country_name = country,
            location_name = "beach name",
            date = "survey date") %>%
        mutate(
            date = if_else(
                is_date_format(date, "%d/%m/%Y"),
                date %>% as.Date("%d/%m/%Y"), as.Date(date)),
            country_code = str_replace(refno, "[0-9]+", replacement = ""),
            location_code = refno) %>%
        select(region_name, country_code, country_name, location_code,
               location_name, date, type_name, abundance)
}

#' @describeIn as_internal_format convert wide data to internal representation.
#'
#' @importFrom dplyr "%>%" rename select arrange mutate
#' @importFrom tidyr spread gather
#' @importFrom stringr str_to_lower
as_internal_format.wide <- function(x, ...) {
    x %>%
        gather(key = "type_name", value = "abundance",
               names(.)[is_type_name(names(.))])
}

#' @describeIn as_internal_format convert long data to internal representation.
#'
#' @importFrom dplyr "%>%" rename select arrange mutate
#' @importFrom tidyr spread gather
#' @importFrom stringr str_to_lower
as_internal_format.long <- function(x, ...) {
    x %>%
        spread(key = type_name, value = abundance, fill = 0) %>%
        gather(key = "type_name", value = "abundance", -region_name,
               -country_code, -country_name, -location_code,
               -location_name, -date) %>%
        mutate(type_name = str_to_lower(type_name)) %>%
        select(region_name, country_code, country_name, location_code,
               location_name, date,
               type_name, abundance)
}


#' Validation of Litter File Formats
#'
#' Generic function for validation of file formats.
#'
#' @param x object to validate
#' @param \dots further arguments passed to or from other methods.
validate <- function(x, ...) {
    UseMethod("validate", x)
}

#' @describeIn validate validate OSPAR data.
#'
#' @return validated object of class \code{ospar}
#'
#' @importFrom dplyr "%>%" distinct one_of filter pull
#' @importFrom purrr map_lgl
#' @importFrom readr parse_date
#' @importFrom rlang are_na
validate.ospar <- function(x, ...) {

    # set field names to lower case
    x <- x %>%
        set_names(str_to_lower(names(.)))

    # check required fields
    required_field_names <- c("refno", "region", "country", "beach name",
                              "survey date")
    missing_field_names <- required_field_names %>% setdiff(names(x))
    n_missing_field_names <- length(missing_field_names)
    (n_missing_field_names == 1L) && stop(
        "Column ",  sQuote(missing_field_names),  " is missing", call. = FALSE)
    (n_missing_field_names  > 1L) && stop(
        "Columns ", sQuote(missing_field_names), " are missing", call. = FALSE)

    # check date field, either OSPAR or ISO 8601
    is_valid_dmy <- x %>%
        pull("survey date") %>%
        is_date_format("%d/%m/%Y")
    is_valid_ymd <- x %>%
        pull("survey date") %>%
        is_date_format("%Y-%m-%d")
    is_valid <- is_valid_dmy | is_valid_ymd
    any(!is_valid) && stop(
        "Invalid date format found in records:\n",
        toString(sequenize(which(!is_valid))),
        "\nPlease adhere to either OSPAR format (dd/mm/YYYY) or ",
        "ISO 8601 format (YYYY-mm-dd).", call. = FALSE)

    # make sure date field is consistent
    if (!(all(is_valid_dmy) || all(is_valid_ymd))) {
        stop(
            "Inconsistent date formats found.",
            "\nPlease adhere to either OSPAR format (dd/mm/YYYY) or ",
            "ISO 8601 format (YYYY-mm-dd), but not both.", call. = FALSE)
    }

    # keep only litter data
    litter_counts <- names(x)[is_type_name(names(x))]
    type_names <- c(required_field_names, litter_counts)
    redundant_field_names <- x %>%
        names %>%
        setdiff(type_names)
    x <- x %>%
        select(type_names)
    if (length(redundant_field_names) > 0L) {
        message(
            "The following fields will be excluded from analysis:\n",
            redundant_field_names %>% sQuote %>% toString)
    }

    # check for natural numbers
    no_natural_number <- x %>%
        select(litter_counts) %>%
        select_if(function(x) {
            !all(is_natural_number(x))
        }) %>%
        names
    if (length(no_natural_number) != 0L) {
        warning(
            "The following column(s) do not contain natural numbers ",
            "({0, 1, 2, ...}, ISO 80000-2):\n",
            toString(sQuote(no_natural_number)),
            "\nThese will be excluded from analysis.",
            call. = FALSE
        )
    }
    litter_counts <- litter_counts %>%
        setdiff(no_natural_number)
    type_names <- c(required_field_names, litter_counts)
    x <- x %>%
        select(type_names)

    # check for completely duplicated records
    has_duplicates <- duplicated(x)
    if (any(has_duplicates)) {
        warning(
            "The following records are duplicated and will be removed:\n",
            toString(sequenize(which(has_duplicates))),
            call. = FALSE
        )
        x <- x %>%
            filter(!has_duplicates)
    }

    # check for records with the same date/beach
    has_duplicates <- x %>%
        select(required_field_names) %>%
        duplicated
    if (any(has_duplicates)) {
        warning(
            "The following records have different counts for the same ",
            "`Beach name` and `Survey date`:\n",
            toString(sequenize(which(has_duplicates))),
            call. = FALSE
        )
    }

    # check for empty cells
    has_empty_cell <- x %>%
        select(litter_counts) %>%
        map_lgl(function(x) {
            any(is.na(x))
        })
    if (any(has_empty_cell)) {
        stop(
            "The following columns contain empty cells:\n",
            toString(names(has_empty_cell)[has_empty_cell]),
            call. = FALSE
        )
    }

    # return validated data
    x
}

#' @describeIn validate validate long format data.
#'
#' @return validated object of class \code{long}
#'
#' @importFrom dplyr "%>%" distinct one_of filter
#' @importFrom purrr map_lgl
#' @importFrom readr parse_date
#' @importFrom rlang are_na
validate.long <- function(x, ...) {

    # set field names to lower case
    x <- x %>%
        set_names(str_to_lower(names(.)))

    # check required fields
    required_field_names <- c("region_name", "country_code", "country_name",
                              "location_name", "location_code",
                              "date", "type_name", "abundance")
    missing_field_names <- required_field_names %>% setdiff(names(x))
    n_missing_field_names <- length(missing_field_names)
    (n_missing_field_names == 1L) && stop(
        "Column ",  sQuote(missing_field_names),  " is missing", call. = FALSE)
    (n_missing_field_names  > 1L) && stop(
        "Columns ", sQuote(missing_field_names), " are missing", call. = FALSE)
    redundant_field_names <- x %>%
        names %>%
        setdiff(required_field_names)
    if (length(redundant_field_names) > 0L) {
        message(
            "The following fields will be excluded from analysis:\n",
            redundant_field_names %>% sQuote %>% toString)
    }

    # check date field
    is_valid <- x %>%
        pull("date") %>%
        is_date_format("%Y-%m-%d")
    any(!is_valid) && stop(
        "Invalid date format found in records:\n",
        toString(sequenize(which(!is_valid))),
        "\nPlease adhere to ISO 8601 (YYYY-mm-dd).", call. = FALSE)

    # keep only litter data
    is_valid <- x %>%
        pull("type_name") %>%
        is_type_name
    any(!is_valid) && stop(
        "Invalid litter types found in records:\n",
        toString(sequenize(which(!is_valid))), call. = FALSE)

    # check for natural numbers in abundance field
    no_natural_number <- x %>%
        pull("abundance") %>%
        is_natural_number %>% {
            !.
        }
    if (any(no_natural_number)) {
        warning(
            "The following records do not contain natural numbers ",
            "({0, 1, 2, ...}, ISO 80000-2) for column abundance:\n",
            toString(sequenize(which(no_natural_number))),
            "\nThese will be excluded from analysis.",
            call. = FALSE
        )
        x <- x %>%
            filter(!no_natural_number)
    }

    # check for completely duplicated records
    has_duplicates <- duplicated(x)
    if (any(has_duplicates)) {
        warning(
            "The following records are duplicated and will be removed:\n",
            toString(sequenize(which(has_duplicates))),
            call. = FALSE
        )
        x <- x %>%
            filter(!has_duplicates)
    }

    # check for empty cells
    has_na <- x %>%
        pull("abundance") %>%
        are_na
    if (any(has_na)) {
        warning(
            "The following records contain missing values:\n",
            toString(sequenize(which(has_na))),
            call. = FALSE
        )
    }

    # return validated data
    x
}

#' @describeIn validate validate wide format data.
#'
#' @return validated object of class \code{wide}
#'
#' @importFrom dplyr "%>%" pull
validate.wide <- function(x, ...) {

    # set field names to lower case
    x <- x %>%
        set_names(str_to_lower(names(.)))

    # check required fields
    required_field_names <- c("region_name", "country_code", "country_name",
                              "location_code", "location_name", "date")
    missing_field_names <- required_field_names %>% setdiff(names(x))
    n_missing_field_names <- length(missing_field_names)
    (n_missing_field_names == 1L) && stop(
        "Column ",  sQuote(missing_field_names),  " is missing", call. = FALSE)
    (n_missing_field_names  > 1L) && stop(
        "Columns ", sQuote(missing_field_names), " are missing", call. = FALSE)

    # check date field
    is_valid <- x %>%
        pull("date") %>%
        is_date_format("%Y-%m-%d")
    any(!is_valid) && stop(
        "Invalid date format found in records:\n",
        toString(sequenize(which(!is_valid))),
        "\nPlease adhere to ISO 8601 (YYYY-mm-dd).", call. = FALSE)


    # keep only litter data
    litter_counts <- names(x)[is_type_name(names(x))]
    type_names <- c(required_field_names, litter_counts)
    redundant_field_names <- x %>%
        names %>%
        setdiff(type_names)
    x <- x %>%
        select(type_names)
    if (length(redundant_field_names) > 0L) {
        message(
            "The following fields will be excluded from analysis:\n",
            redundant_field_names %>% sQuote %>% toString)
    }

    # check for natural numbers
    no_natural_number <- x %>%
        select(litter_counts) %>%
        select_if(function(x) {
            !all(is_natural_number(x))
        }) %>%
        names
    if (length(no_natural_number) != 0L) {
        warning(
            "The following column(s) do not contain natural numbers ",
            "({0, 1, 2, ...}, ISO 80000-2):\n",
            toString(sQuote(no_natural_number)),
            "\nThese will be excluded from analysis.",
            call. = FALSE
        )
    }
    litter_counts <- litter_counts %>%
        setdiff(no_natural_number)
    type_names <- c(required_field_names, litter_counts)
    x <- x %>%
        select(type_names)

    # check for completely duplicated records
    has_duplicates <- duplicated(x)
    if (any(has_duplicates)) {
        warning(
            "The following records are duplicated and will be removed:\n",
            toString(sequenize(which(has_duplicates))),
            call. = FALSE
        )
        x <- x %>%
            filter(!has_duplicates)
    }

    # check for records with the same date/beach
    has_duplicates <- x %>%
        select(required_field_names) %>%
        duplicated
    if (any(has_duplicates)) {
        warning(
            "The following records have different counts for the same ",
            "`Beach name` and `Survey date`:\n",
            toString(sequenize(which(has_duplicates))),
            call. = FALSE
        )
    }

    # check for empty cells
    has_empty_cell <- x %>%
        select(litter_counts) %>%
        map_lgl(function(x) {
            any(is.na(x))
        })
    if (any(has_empty_cell)) {
        stop(
            "The following columns contain empty cells:\n",
            toString(names(has_empty_cell)[has_empty_cell]),
            call. = FALSE
        )
    }

    # return validated data
    x
}



#' Read Litter Data
#'
#' Reads litter data from various formats. Currently only the 
#' OSPAR data snapshot format, a long format, and a wide format
#' are supported. See the package vignette for more details.
#'
#' @param file name of litter file
#'
#' @return \code{tibble} with litter data in long format
#' @importFrom readr read_csv parse_number count_fields tokenizer_csv
#' @importFrom dplyr select select_if rename mutate arrange matches
#' @importFrom tidyr gather
#' @importFrom purrr set_names negate
#' @importFrom stringr str_to_lower
#' @importFrom rlang has_name
#' @importFrom fs file_exists path_file
#' @export
read_litter <- function(file) {

    # check if file exists
    (!file_exists(file)) && stop("file ", sQuote(file),
                                 " not found.", call. = FALSE)

    # check if 'file' is a genuine CSV-file with sufficient columns
    n <- file %>%
        count_fields(tokenizer_csv(), n_max = 1L)
    (n == 1L) && stop(
        str_c(
            "An incorrect column delimiter is probably used\nin file %s.\n",
            "Please use a comma as column delimiter.\n",
            "see the troubleshooting section in the tutorial.") %>%
            sprintf(sQuote(path_file(file))),
        call. = FALSE
    )

    
    
    # read file
    d <- suppressMessages(read_csv(file, guess_max = 1000000))

    # get signature
    signature <- d %>%
        get_signature
    (signature == "unknown") && stop(
        "Can't figure out the signature of file ",
        sQuote(file),
        "\nPlease check its column names. ",
        "See the package vignette for details.",
        call. = FALSE)
    class(d) <- c(str_to_lower(signature), class(d))

    # validate and reformat file
    d %>%
        validate %>%
        as_internal_format %>%
        arrange(location_name, date, type_name)
}



#' Read Litter Groups
#' 
#' Read the file that links litter codes to litter groups.
#' See the package vignette for more details.
#'
#'
#' @param file name of litter group file
#'
#' @return \code{tibble} with look-up-table of litter groups
#' @importFrom readr read_csv cols col_character
#' @importFrom fs path_package
#' @importFrom dplyr filter select
#' @importFrom tidyr gather
#' @export
read_litter_groups <- function(file) {
    (!file_exists(file)) && stop("Group file ", sQuote(file),
                                 " not found.", call. = FALSE)
    d <- file %>%
        read_csv(col_types = cols(.default = col_character()))
    class(d) <- c("litter_group", class(d))
    d %>%
        validate %>%
        gather(key = "group_name", value = "value", -type_name) %>%
        filter(!are_na(value)) %>%
        select(-value)
}



#' @describeIn validate validate litter_group file
#'
#' @return validated object of class \code{litter_group}
#'
#' @importFrom dplyr "%>%" distinct one_of filter
#' @importFrom purrr map_lgl map_dfc
#' @importFrom readr parse_date
#' @importFrom rlang are_na
validate.litter_group <- function(x, ...) {

    # convert entries to lower case
    names(x) <- str_to_upper(names(x))
    x <- x %>%
        map_dfc(str_to_lower)

    # check required fields
    required_field_names <- c("TYPE_NAME", "TA")
    missing_field_names <- required_field_names %>% setdiff(names(x))
    n_missing_field_names <- length(missing_field_names)
    (n_missing_field_names == 1L) && stop(
        "Column ",  sQuote(missing_field_names),
        " is missing", call. = FALSE)
    (n_missing_field_names  > 1L) && stop(
        "Columns ", sQuote(missing_field_names),
        " are missing", call. = FALSE)
    x <- x %>%
        rename(type_name = "TYPE_NAME")

    # check if all entries are 'x' or NA
    entry_ok <- x %>%
        select(-type_name) %>%
        map_lgl(function(x) {
            all(are_na(x) | (x == "x"))
        })

    any(!entry_ok) && stop(
        "In the litter group file only the character 'x' is allowed as ",
        "indicator\nThis is not true for columns: ",
        toString(sQuote(names(entry_ok)[!entry_ok])), call. = FALSE)

    # return result
    x
}
