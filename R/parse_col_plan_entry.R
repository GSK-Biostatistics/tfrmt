parse_col_plan_entry <- function(x, ...) {
    UseMethod("parse_col_plan_entry", x)
}

#' @export
#' @keywords internal
parse_col_plan_entry.name <- function(x, ...) {
    if (identical(rlang::as_label(x), "<empty>")) {
        return(NULL)
    }

    rlang::quo(!!x)
}

#' @export
#' @keywords internal
parse_col_plan_entry.call <- function(x, call = rlang::caller_env(), ...) {
    parent_call <- rlang::frame_call(call)
    in_span_structure <- is.call(parent_call) &&
        identical(rlang::call_name(parent_call), "span_structure")

    if (is_valid_tidyselect_call(x)) {
        return(rlang::quo(!!x))
    }

    # span structures cannot be nested inside another span_structure()
    if (
        is_valid_quo_call(x) ||
            (!in_span_structure && is_valid_span_structure_call(x))
    ) {
        return(rlang::eval_tidy(x))
    }

    entry_label <- format(x)

    # nolint start: duplicate_argument_linter
    error_msg_bullets <- c(
            "Invalid entry: {.code {entry_label}}",
            "x" = "Only the following can be entered as contents:",
            "*" = "span structures ({.fn span_structure}),",
            "*" = "selection helpers (see {.url https://tidyselect.r-lib.org/reference}),",
            "*" = "unquoted expressions representing variable names.",
            "i" = "Renaming variables with {.code new_name = old_name} is allowed."
        )
    # nolint end

    if (in_span_structure) {
        # remove the span structures message as not applicable when the
        # parent call is span_structure()
        error_msg_bullets <- error_msg_bullets[-3]
    }

    cli::cli_abort(
        error_msg_bullets,
        call = call
    )
}

#' @export
#' @keywords internal
parse_col_plan_entry.character <- function(x, ...) {
    as_length_one_quo(x)
}

#' @export
#' @keywords internal
parse_col_plan_entry.default <- function(x, call = rlang::caller_env(), ...) {
    call_label <- format(rlang::frame_call(call))

    cli::cli_abort(
        "Unexpected entry type in {call_label}.",
        call = call
    )
}
