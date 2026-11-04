#' @export
#' @keywords internal
parse_special_dots <- function(x, ...) {
    UseMethod("parse_special_dots", x)
}

#' @export
#' @keywords internal
parse_special_dots.name <- function(x, ...) {
    if (identical(rlang::as_label(x), "<empty>")) {
        return(NULL) # nolint: return_linter
    }

    rlang::quo(!!x)
}

#' @export
#' @keywords internal
parse_special_dots.call <- function(x, call = rlang::caller_env()) {
    if (is_valid_tidyselect_call(x)) {
        return(rlang::quo(!!x))
    }

    if (is_valid_quo_call(x) || is_valid_span_structure_call(x)) {
        return(rlang::eval_tidy(x)) # nolint: return_linter
    }

    entry_label <- format(x)
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

#' @export
#' @keywords internal
parse_special_dots.character <- function(x, ...) {
    as_length_one_quo(x)
}

#' @export
#' @keywords internal
parse_special_dots.default <- function(x, ...) {
    cli::cli_abort("not good")
}
