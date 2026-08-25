#' Apply formatting
#'
#' @param .data data, but only what is getting changed
#' @param frmt_def formatting to be applied
#' @param value value symbol should only be one
#' @param mock Logical value is this is for a mock or not. By default `FALSE`
#' @param ... additional arguments for methods
#' @param param param column as a quosure
#' @param column column columns as a list of quosures
#' @param label label column as a quosure
#' @param group group column as a list of quosures
#'
#' @return formatted dataset
#'
#' @export
#' @examples
#'
#' library(tibble)
#' library(dplyr)
#' library(rlang)
#' # Set up data
#' df <- tibble(x = c(20.12,34.54,12.34))
#'
#' apply_frmt(
#'     frmt_def = frmt("XX.X"),
#'     .data = df,
#'     value = quo(x)
#' )
#'
#' @rdname apply_frmt
apply_frmt <- function(frmt_def, .data, value, mock = FALSE, ...) {
    UseMethod("apply_frmt", frmt_def)
}


#' @export
#'
#' @rdname apply_frmt
apply_frmt.frmt <- function(frmt_def, .data, value, mock = FALSE, ...) {
    if (mock) {
        out <- dplyr::mutate(
            .data,
            !!value := frmt_def$expression
        )
    } else {
        vals <- dplyr::pull(.data, !!value)

        if (length(vals) == 0) {
            return(.data)
        } else if (!is.null(frmt_def$transform)) {
            vals <- rlang::as_function(frmt_def$transform)(vals)
        }

        if (stringr::str_detect(frmt_def$expression, "[x|X]")) {
            # digits following period in expression
            dig <- frmt_def$expression %>%
                stringr::str_extract("(?<=\\.)[X|x]+") %>%
                stringr::str_count("[X|x]")

            ## There were no x's after a `.` to extract, so assume none
            if (is.na(dig)) {
                dig <- 0
            }

            ## convert to scientific if scientific
            if (!is.null(frmt_def$scientific)) {
                vals_sci <- format(vals, scientific = TRUE)

                vals <- vals_sci %>%
                    stringr::str_extract("[^e]+") %>%
                    as.numeric()

                ## remove x's from end of scientific
                multiply <- stringr::str_remove(
                    frmt_def$scientific,
                    "[xX]+(?<=$)"
                )
                sci_width <- stringr::str_extract(
                    frmt_def$scientific,
                    "[xX]+(?<=$)"
                ) %>%
                    stringr::str_count("[X|x]")

                vals_sci_post <- vals_sci %>%
                    stringr::str_extract("[^e]+$") %>%
                    as.numeric() %>%
                    format(trim = TRUE, width = sci_width) %>%
                    paste0(multiply, .)
            } else {
                vals_sci_post <- ""
            }

            # digits preceding period in expression
            pre_dec_expr <- frmt_def$expression %>%
                stringr::str_remove("\\..*$") %>%
                stringr::str_count("[X|x]")

            # vals rounded and trimmed
            rounded_vals <- format(
                round(vals, dig),
                decimal.mark = ".",
                nsmall = dig
            ) %>%
                stringr::str_trim()

            fmt_options <- tibble::tibble(
                rounded = rounded_vals,
                # digits preceding period in vals
                act_pre_dec = rounded_vals %>%
                    stringr::str_remove("\\..*$") %>%
                    stringr::str_count(".")
            ) %>%
                dplyr::mutate(
                    # keep from being negative
                    space_to_add = pmax(pre_dec_expr - .data$act_pre_dec, 0)
                )

            # when scientific is null paste rounded value, if not then append scientific expression
            fmt_vals <- stringr::str_c(
                stringr::str_dup(" ", fmt_options$space_to_add),
                fmt_options$rounded,
                vals_sci_post
            )

            expr_start <- frmt_def$expression %>%
                stringr::str_extract("^[^X|^x]*(?=[X|x])")

            expr_end <- frmt_def$expression %>%
                stringr::str_extract("(?<=[X|x])[^X|^x]*$")

            if (!is.null(frmt_def$missing)) {
                miss_val <- frmt_def$missing
            } else {
                miss_val <- NA_character_
            }

            # Combining the additional formatting
            fmt_val_output <- dplyr::case_when(
                fmt_options$rounded == "NA" ~ miss_val,
                TRUE ~ stringr::str_c(expr_start, fmt_vals, expr_end)
            )
        } else {
            fmt_val_output <- frmt_def$expression
        }

        out <- .data %>%
            dplyr::mutate(
                !!value := fmt_val_output
            )
    }

    out
}


#' @export
#'
#' @rdname apply_frmt
apply_frmt.frmt_combine <- function(
    frmt_def,
    .data,
    value,
    mock = FALSE,
    param,
    column,
    label,
    group,
    ...
) {
    fmt_param_vals <- frmt_def$expression %>%
        stringr::str_extract_all("(?<=\\{)[^\\}]+(?=\\})") %>%
        unlist()

    # Adding the unquoted version to match while long
    fmt_param_vals_uq <- stringr::str_remove_all(fmt_param_vals, "`")

    # Check if unspecified param values are in the dataset

    if (!setequal(names(frmt_def$frmt_ls), fmt_param_vals)) {
        stop(
            "The values in the expression don't match the names of the given formats "
        )
    }

    ## format params as needed
    .tmp_data <- purrr::map_dfr(fmt_param_vals, function(`__var`) {
        fmt_to_apply <- frmt_def$frmt_ls[[`__var`]]
        .data %>%
            dplyr::filter(!!param == stringr::str_remove_all(`__var`, "`")) %>%
            apply_frmt(
                frmt_def = fmt_to_apply,
                .data = .,
                value = value,
                column = column,
                param = param,
                label = label,
                group = group,
                mock = mock,
                ...
            )
    })

    #Test if common information exists
    miss_param_from_data <- .tmp_data %>%
        dplyr::pull(!!param) %>%
        unique() %>%
        setdiff(fmt_param_vals_uq, .)

    if (length(miss_param_from_data) > 0) {
        stop(paste0(
            "Unable to create formatting combination because the following parameters are missing from the data:\n ",
            paste(miss_param_from_data, collapse = " \n")
        ))
    }

    .tmp_data_wide <- .tmp_data %>%
        dplyr::select(!!value, !!param, !!!column, !!label, !!!group) %>%
        tidyr::pivot_wider(
            values_from = !!value,
            names_from = !!param
        ) %>%
        dplyr::mutate(
            .is_all_missing = all_missing(fmt_param_vals, .)
        )

    missing_param_replacements <-
        purrr::map(fmt_param_vals, ~ frmt_def$frmt_ls[[.x]]$missing) %>%
        stats::setNames(fmt_param_vals) %>%
        purrr::discard(is.null)

    if (length(missing_param_replacements) > 0) {
        ## after .is_all_missing so that can be tabulated first
        .tmp_data_wide <- .tmp_data_wide %>%
            tidyr::replace_na(missing_param_replacements)
    }

    # check that pivot_wider resulted in a reduction of rows, which indicates that at least
    #  1 row will successfully have a frmt_combine in it
    if (nrow(.tmp_data_wide) == nrow(.tmp_data)) {
        id_cols <- .tmp_data %>%
            dplyr::select(!!!column, !!label, !!!group, !!param)
        warning(
            paste0(
                "Unable to apply `frmt_combine` due to uniqueness of column/row identifiers. Params that are to be combined need to have matching values across: ",
                toString(names(id_cols %>% dplyr::select(-!!param))),
                ". Current values:\n",
                paste(
                    utils::capture.output(as.data.frame(id_cols)),
                    collapse = "\n"
                )
            )
        )
    }

    if (is.null(frmt_def$missing)) {
        frmt_def$missing <- ""
    }

    ## if both params are missing, then drop in frmt definition missing value
    ## otherwise concat the params
    .tmp_data_fmted <- .tmp_data_wide %>%
        dplyr::mutate(
            !!value := dplyr::case_when(
                .data$.is_all_missing ~ frmt_def$missing,
                TRUE ~ stringr::str_glue(!!frmt_def$expression) %>%
                    as.character()
            )
        ) %>%
        dplyr::select(
            -tidyselect::all_of(
                fmt_param_vals_uq
            ),
            -".is_all_missing"
        )

    ## if not mock remove
    if (!mock) {
        .data <- .data %>%
            dplyr::select(-!!value)
    }

    merge_group <- purrr::map(
        c(column, label, group),
        function(x) {
            if (!rlang::quo_is_missing(x)) {
                x
            }
        }
    ) %>%
        purrr::discard(is.null) %>%
        do.call("vars", .)

    # merge on new values, and remove cases other than first occurance of group/label/column pairing
    .data %>%
        dplyr::left_join(
            .tmp_data_fmted,
            by = purrr::map_chr(merge_group, rlang::as_label)
        ) %>%
        dplyr::group_by(!!!merge_group) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
}

#' @export
#'
#' @rdname apply_frmt
apply_frmt.frmt_when <- function(frmt_def, .data, value, mock = FALSE, ...) {
    if (mock) {
        frmt_to_prt <- frmt_def$frmt_ls %>%
            purrr::keep(~ rlang::f_lhs(.) == "TRUE")
        if (length(frmt_to_prt) < 1) {
            frmt_to_prt <- frmt_def$frmt_ls
        }
        str_to_prnt <- rlang::f_rhs(frmt_to_prt[[1]])$expression
        out <- .data %>%
            dplyr::mutate(
                !!value := str_to_prnt
            )
    } else {
        values_str <- rlang::as_label(value)
        n <- length(frmt_def$frmt_ls)

        val_len <- length(dplyr::pull(.data, !!value))
        right <- frmt_def$frmt_ls %>%
            purrr::map(rlang::f_rhs) %>%
            purrr::map(function(x) {
                if (is_frmt(x)) {
                    out <- x %>%
                        apply_frmt(.data, value, ...) %>%
                        dplyr::pull(!!value)
                } else {
                    out <- rep(x, val_len)
                }
                out
            })

        left <- frmt_def$frmt_ls %>%
            purrr::map_chr(f_lhs_as_char) %>%
            dplyr::if_else(. == "TRUE", ., paste0(values_str, .)) %>%
            rlang::parse_exprs() %>%
            purrr::map(rlang::eval_tidy, .data)

        out <- rep(NA_character_, val_len)
        replaced <- rep(FALSE, val_len)

        for (i in seq_len(n)) {
            out <- replace_val(out, left[[i]] & !replaced, right[[i]])
            replaced <- replaced | (left[[i]] & !is.na(left[[i]]))
        }

        if (is.null(frmt_def$missing)) {
            out <- out
        } else if (!is.null(frmt_def$missing)) {
            out <- tidyr::replace_na(
                out,
                replace = frmt_def$missing
            )
        }

        out <- .data %>%
            dplyr::mutate(
                !!value := out
            )
    }
    out
}
