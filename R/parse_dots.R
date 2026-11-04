parse_special_dots <- function(x, ...) {
    UseMethod("parse_special_dots", x)
}

#' @export
#' @keywords internal
parse_special_dots.name <- function(x, ...) {
    if (identical(rlang::as_label(x), "<empty>")) {
        return(NULL)
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
        return(rlang::eval_tidy(x))
    }

    call_nm <- rlang::frame_call(call) |> rlang::call_name()

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

    if (call_nm == "span_structure") {
        # remove the span structures message as not applicable
        error_msg_bullets <- error_msg_bullets[-3]
    }

    entry_label <- format(x)
    cli::cli_abort(
        error_msg_bullets,
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
parse_special_dots.default <- function(x, call = rlang::caller_env()) {
    call_label <- format(rlang::frame_call(call))

    cli::cli_abort(
        "Unexpected entry type in {call_label}.",
    )
}
