#' Check if an object is a card
#'
#' Validates that the input is a valid `card` object. If the check fails,
#' it throws an informative error message using [rlang::stop_input_type()].
#'
#' @param card An object to check.
#' @param arg Character string specifying the argument name to display in error messages.
#' @param call The execution environment for error backtraces.
#' @param allow_null Logical indicating whether `NULL` should be accepted as a valid input.
#'
#' @return Invisible `NULL` if validation passes, otherwise throws an error.
#' @noRd
check_card <- function(
    card,
    arg = rlang::caller_arg(card),
    call = rlang::caller_env(),
    allow_null = FALSE
) {
    if (!missing(card)) {
        if (is_card(card)) {
            return(invisible(NULL))
        }

        if (allow_null && is.null(card)) {
            return(invisible(NULL))
        }
    }

    rlang::stop_input_type(
        card,
        "a card object",
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

#' Test if an object is a card
#'
#' Checks whether an object inherits from the `"card"` class.
#'
#' @param x An object to test.
#'
#' @return Logical `TRUE` if the object is a card, `FALSE` otherwise.
#' @noRd
is_card <- function(x) {
    inherits(x, "card")
}

#' Check if an object inherits from 'bind_ard'
#'
#' @param x An object to check.
#' @return `TRUE` if the object inherits from 'bind_ard', `FALSE` otherwise.
#' @noRd
is_bind_ard_card <- function(x) {
    inherits(x, "bind_ard")
}

#' Extract an argument from card attributes
#'
#' @param x a card object (data frame)
#' @param arg character string of the argument to extract. Defaults to `"by"`.
#'
#' @returns the value of the argument, or `NULL` if not found
#' @noRd
get_card_attr_arg <- function(x, arg = "by") {
    attr(x, "args")[[arg]]
}

#' Set a specific parameter inside an object's 'args' attribute
#'
#' @param x An object.
#' @param name A string naming the element to modify inside the 'args' attribute list.
#' @param value The value to assign to that element (e.g., NULL to drop it).
#' @return The modified object.
#' @noRd
set_card_args <- function(x, name, value) {
    attr(x, "args")[[name]] <- value
    x
}

#' Drop stale attributes from a combined ARD object
#'
#' @param x An object to check and modify.
#' @return The modified object with stale attributes removed if it inherits
#'   from 'bind_ard' and contains an 'args' attribute.
#' @noRd
drop_bind_ard_args <- function(x) {
    if (is_bind_ard_card(x) && !is.null(attr(x, "args"))) {
        x <- set_card_args(x, "by", NULL)
        x <- set_card_args(x, "variable", NULL)
        x <- set_card_args(x, "strata", NULL)
    }

    x
}
