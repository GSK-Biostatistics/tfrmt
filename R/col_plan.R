#' Define the Column Plan & Span Structures
#'
#' Using <[`tidy-select`][dplyr::dplyr_tidy_select]> expressions and a series
#' span_structures, define the order of the columns. The selection follows "last
#' selected" principals, meaning columns are moved to the _last_ selection as
#' opposed to preserving the first location.
#'
#'
#' @details
#'
#' ## Column Selection
#'
#' When col_plan gets applied and is used to create the output table, the
#' underlying logic sorts out which column specifically is being selected. If a column
#' is selected twice, the _last_ instance in which the column is selected will be
#' the location it gets rendered.
#'
#' Avoid beginning the \code{col_plan()} column selection with a deselection (i.e.
#' \code{col_plan(-col1)}, \code{col_plan(-starts_with("value")))}. This will
#' result in the table preserving all columns not "de-selected" in the
#' statement, and the order of the columns not changed. It is preferred when
#' creating the \code{col_plan()} to identify all the columns planned on
#' preserving in the order they are wished to appear, or if
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> arguments - such as
#' \code{\link[dplyr]{everything}}- are used, identify the de-selection after
#' the positive-selection.
#'
#' Alternatively, once the gt table is produced, use the
#' \code{\link[gt]{cols_hide}} function to remove un-wanted columns.
#'
#'
#' @rdname col_plan
#'
#' @param ... For a `col_plan` and `span_structure`,
#'   <[`tidy-select`][dplyr::dplyr_tidy_select]> arguments, unquoted expressions
#'   separated by commas, and span_structures. span_structures must have the
#'   arguments named to match the name the column in the input data has to identify the correct columns.
#'   Each named span entry must include at least one column value; empty values such as "" or c() are not allowed.See the examples
#' @param .drop Boolean. Should un-listed columns be dropped from the data.
#'   Defaults to FALSE.
#'
#' @seealso \href{https://gsk-biostatistics.github.io/tfrmt/articles/col_plan.html}{Link to related article}
#'
#' @return col_plan object
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' ## select col_1 as the first column, remove col_last, then create spanning
#' ## structures that have multiple levels
#' ##
#' ## examples also assume the tfrmt has the column argument set to c(c1, c2, c3)
#' ##
#' spanning_col_plan_ex <- col_plan(
#'  col_1,
#'  -col_last,
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c2 = "Second Label Level 1.1",
#'    c3 = c(col_3, col_4)
#'  ),
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c2 = "Second Label Level 1.2",
#'    c3 = starts_with("B")
#'    ),
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c3 = col_5
#'  ),
#'  span_structure(
#'    c2 = "Top Label Level 2",
#'    c3 = c(col_6, col_7)
#'  )
#' )
#'
#' ## select my_col_1 as the first column, then
#' ## rename col_2 to new_col_1 and put as the
#' ## second column, then select the rest of the columns
#' renaming_col_plan_ex <- col_plan(
#'    my_col_1,
#'    new_col_1 = col_2,
#'    everything()
#'  )
#'
#' renaming_col_plan_ex2 <- col_plan(
#'    my_col_1,
#'    new_col_1 = col_2,
#'    span_structure(
#'     c1 = c(`My Favorite span name` = "Top Label Level 1"),
#'     c3 = c(`the results column` = col_5)
#'    )
#'  )
#'
#' ## To one or more stub headers rename the corresponding group/label variables in the column plan.
#' ## If multiple group variables exist, any of them can be renamed.
#' ## Note: Multiple stub headers are only possible if the `row_grp_plan` label
#' ## location is set to "column". Otherwise, if more than one group/label column is renamed,
#' ## {tfrmt} will use the highest level group name available.
#'
#'  renaming_group <- col_plan(
#'     my_grp = group, # rename group
#'     label,
#'     starts_with("col")
#'   )
#'
##' @section Images:
#' Here are some example outputs:
#'
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-span_structure-cropped.jpg\" alt =\"Example of a dataset being turned into a table with spanning columns\" style=\"width:100\\%;\">"`
#' }}
#'
col_plan <- function(..., .drop = FALSE) {
    ## selectively evaluate dots (only if is a span_structure)
    ## confirm contents otherwise
    dots <- as.list(substitute(substitute(...)))[-1]
    dots <- check_col_plan_dots(dots)

    structure(
        list(
            dots = dots,
            .drop = .drop
        ),
        class = c("col_plan", "plan")
    )
}

#' @rdname col_plan
#'
#' @return span_structure object
#'
#' @export
span_structure <- function(...) {
    span_cols <- as.list(substitute(substitute(...)))[-1]
    check_span_structure_args(span_cols)
    span_cols <- parse_span_structure_dots(span_cols)
    check_span_structure_values(span_cols)

    structure(
        span_cols,
        class = "span_structure"
    )
}

is_span_structure <- function(x) {
    inherits(x, "span_structure")
}

check_span_structure_args <- function(x, call = rlang::caller_env()) {
    if (length(x) == 0) {
        cli::cli_abort(
            "{.fn span_structure} requires at least one named argument.",
            call = call
        )
    }

    x_names <- names(x)

    if (is.null(x_names) || !all(nzchar(x_names))) {
        call_label <- format(rlang::frame_call(call))
        cli::cli_abort(
            c(
          "Entries of a {.fn span_structure} must be named.",
          "i" = "Got: {call_label}"
        ),
            call = call
        )
    }
    invisible(NULL)
}


parse_span_structure_dots <- function(x, call = rlang::caller_env()) {
    x_dots <- lapply(x, function(span_entry) {
        lapply(trim_vars_quo_c(span_entry), function(x) {
            if (is.name(x)) {
                if (identical(rlang::as_label(x), "<empty>")) {
                    return(NULL) # nolint: return_linter
                } else {
                    return(rlang::quo(!!x)) # nolint: return_linter
                }
            } else if (is.call(x)) {
                if (is_valid_tidyselect_call(x)) {
                    rlang::quo(!!x)
                } else if (is_valid_quo_call(x)) {
                    return(rlang::eval_tidy(x)) # nolint: return_linter
                } else {
                    entry_label <- format(x)
                    cli::cli_abort(
                        c(
                          "Invalid entry: {.code {entry_label}}",
                          "x" = "Only the following can be entered as contents:",
                          "*" = "selection helpers (see {.url https://tidyselect.r-lib.org/reference}),",
                          "*" = "unquoted expressions representing variable names.", # nolint: duplicate_argument_linter
                          "i" = "Renaming variables with {.code new_name = old_name} is allowed."
                        ),
                        call = call
                    )
                }
            } else if (is.character(x)) {
                if (!nzchar(x)) {
                    return(NULL) # nolint: return_linter
                }
                return(as_length_one_quo.character(x)) # nolint: return_linter
            } else {
                cli::cli_abort(
                    "Unexpected entry type in {.fn span_structure}.",
                    call = call
                )
            }
        })
    })

    x_dots[!sapply(x_dots, is.null)]
}

check_span_structure_values <- function(
    x,
    call = rlang::caller_env()
) {
    is_empty_entry <- function(e) {
        length(e) == 0 || all(vapply(e, is.null, logical(1)))
    }

    empty_entries <- names(x)[sapply(x, is_empty_entry)]

    if (length(empty_entries) > 0) {
        cli::cli_abort(
            c(
          "The following {.fn span_structure} arguments have no column values:",
          "x" = "{.code {empty_entries}}"
        ),
            call = call
        )
    }

    invisible(NULL)
}

is_valid_span_structure_call <- function(x) {
    as.character(as.list(x)[[1]]) == "span_structure"
}

is_valid_tidyselect_call <- function(x) {
    ## drop - from determining if
    if (as.character(as.list(x)[[1]]) == "-") {
        x <- x[[-1]]
        if (is.name(x)) {
            return(TRUE) # nolint: return_linter
        }
    }
    as.character(as.list(x)[[1]]) %in% c(names(tidyselect::vars_select_helpers))
}

is_valid_quo_call <- function(x) {
    ## drop - from determining if
    if (as.character(as.list(x)[[1]]) == "-") {
        x <- x[[-1]]
        if (is.name(x)) {
            return(TRUE) # nolint: return_linter
        }
    }
    as.character(as.list(x)[[1]]) %in% c("vars", "quo")
}

check_col_plan_dots <- function(x, call = rlang::caller_env()) {
    lapply(x, function(entry) {
        if (is.name(entry)) {
            if (identical(rlang::as_label(entry), "<empty>")) {
                return(NULL) # nolint: return_linter
            } else {
                return(rlang::quo(!!entry)) # nolint: return_linter
            }
        } else if (is.call(entry)) {
            if (is_valid_tidyselect_call(entry)) {
                rlang::quo(!!entry)
            } else if (
                is_valid_quo_call(entry) || is_valid_span_structure_call(entry)
            ) {
                return(rlang::eval_tidy(entry)) # nolint: return_linter
            } else {
                entry_label <- format(entry)
                cli::cli_abort(
                    c(
                    "Invalid entry: {.code {entry_label}}",
                    "x" = "Only the following can be entered as contents:",
                    "*" = "span structures ({.fn span_structure}),",
                    "*" = "selection helpers (see {.url https://tidyselect.r-lib.org/reference}),", # nolint: duplicate_argument_linter
                    "*" = "unquoted expressions representing variable names.", # nolint: duplicate_argument_linter
                    "i" = "Renaming variables with {.code new_name = old_name} is allowed."
                  ),
                    call = call
                )
            }
        } else if (is.character(entry)) {
            return(as_length_one_quo.character(entry)) # nolint: return_linter
        } else {
            cli::cli_abort(
                "Unexpected entry type in {.fn col_plan}.",
                call = call
            )
        }
    })
}
