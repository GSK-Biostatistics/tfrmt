# Utilities ---------------------------------------------------------------

expr_to_filter <- function(cols, val) {
    UseMethod("expr_to_filter", cols)
}

#' @export
expr_to_filter.quosure <- function(cols, val) {
    ## If is missing a quosure, nothing to filter
    if (rlang::quo_is_missing(cols)) {
        return("TRUE")
    }

    # This is all so it works when there is a list
    if (all(val == ".default")) {
        out <- "TRUE"
    } else {
        val <- ifelse(
            stringr::str_detect(val, "^`.*`$"),
            stringr::str_sub(val, 2, -2),
            val
        )
        out <- rlang::as_label(cols) %>%
            paste0("`", ., "`") %>%
            paste0(
                " %in% c(",
                toString(shQuote(val, type = "cmd")),
                ")"
            )
    }
    out
}

#' @export
expr_to_filter.quosures <- function(cols, val) {
    if (is.null(val)) {
        out <- "TRUE"
    } else if (!is.list(val) && length(cols) == 1) {
        cols <- cols[[1]]
        out <- expr_to_filter(cols, val)
    } else if (!is.list(val) && all(val == ".default")) {
        out <- "TRUE"
    } else if (is.list(val)) {
        if (!all(names(val) %in% purrr::map_chr(cols, rlang::as_label))) {
            stop("Names of val entries do not all match col values")
        }
        out <- purrr::map2_chr(
            cols,
            val[purrr::map_chr(cols, rlang::as_label)],
            ~ expr_to_filter(.x, .y)
        ) %>%
            paste(collapse = " & ")
    } else {
        stop("If multiple cols are provided, val must be a named list")
    }
    out
}

#' Given a *_structure with specific group/label value(s) (i.e. non-default),
#' return the row indices corresponding to each unique value
#'
#' @param cur_struct current structure object
#' @param .data data to test against
#' @param group list of the group parameters
#' @param label label symbol should only be one
#'
#' @return list of row indices
#' @noRd
struct_val_idx <- function(cur_struct, .data, group, label) {
    grp_expr <- "TRUE"
    lbl_expr <- "TRUE"
    keep_vars <- NULL

    # only do this if cur_struct contains a non-default value
    if (detect_non_default(cur_struct$group_val)) {
        grp_expr <- expr_to_filter(group, cur_struct$group_val)

        if (is.list(cur_struct$group_val)) {
            keep_vars <- group[purrr::map_lgl(
                cur_struct$group_val,
                ~ !all(.x == ".default")
            )]
        } else {
            keep_vars <- group
        }
    }

    if (detect_non_default(cur_struct$label_val)) {
        lbl_expr <- expr_to_filter(label, cur_struct$label_val)
        keep_vars <- c(keep_vars, label)
    }

    if (!is.null(keep_vars)) {
        filter_expr <- paste(
            c(lbl_expr, grp_expr),
            collapse = "&"
        ) %>%
            rlang::parse_expr()

        .data %>%
            dplyr::filter(!!filter_expr) %>%
            dplyr::select(
                tidyselect::any_of(
                    c(
                        purrr::map_chr(keep_vars, rlang::as_label),
                        "TEMP_row"
                    )
                )
            ) %>%
            # split only after non-consecutive sequence
            dplyr::mutate(
                breaks = .data$TEMP_row ==
                    dplyr::lag(.data$TEMP_row, default = 0) + 1,
                breaks = cumsum(!.data$breaks)
            ) %>%
            dplyr::group_by(.data$breaks) %>%
            dplyr::group_split() %>%
            purrr::map(function(x) dplyr::pull(x, .data$TEMP_row))
    } else {
        .data %>%
            dplyr::pull(.data$TEMP_row) %>%
            list()
    }
}

# detect use of .default in a *_structure object
#' @noRd
detect_default <- function(struct) {
    purrr::map_lgl(struct, ~ any(!is.null(.x) && any(.x == ".default"))) %>%
        any()
}

# detect use of non-default in a  *_structure object entry
detect_non_default <- function(struct_val) {
    !is.null(struct_val) && !all(struct_val == ".default")
}

#' Create the group_by expression for the data
#'
#' @param cur_struct current structure object
#' @param group list of the group parameters
#' @param label label symbol should only be one
#'
#' @return character vector of variable names to group by
#' @noRd
#'
expr_to_grouping <- function(cur_struct, group, label) {
    grouping <- NULL

    if (!is.null(cur_struct$group_val)) {
        if (
            !is.list(cur_struct$group_val) &&
                all(cur_struct$group_val == ".default")
        ) {
            grp_to_add <- purrr::map_chr(group, rlang::as_label)
            grouping <- c(grouping, grp_to_add)
        } else if (
            is.list(cur_struct$group_val) &&
                any(cur_struct$group_val == ".default")
        ) {
            grp_to_add <- names(cur_struct$group_val)[purrr::map_lgl(
                cur_struct$group_val,
                ~ all(.x == ".default")
            )]
            grouping <- c(grouping, grp_to_add)
        }
    }
    if (!is.null(cur_struct$label_val) && cur_struct$label_val == ".default") {
        grouping <- c(grouping, rlang::as_label(label))
    }

    grouping %>% unname()
}
