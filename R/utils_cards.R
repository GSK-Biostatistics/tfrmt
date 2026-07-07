#' Check if an object inherits from 'bind_ard'
#'
#' @param x An object to check.
#' @return `TRUE` if the object inherits from 'bind_ard', `FALSE` otherwise.
#' @noRd
is_bind_ard_cards <- function(x) {
  inherits(x, "bind_ard")
}

#' Set a specific parameter inside an object's 'args' attribute
#'
#' @param x An object.
#' @param name A string naming the element to modify inside the 'args' attribute list.
#' @param value The value to assign to that element (e.g., NULL to drop it).
#' @return The modified object.
#' @noRd
set_card_attr <- function(x, name, value) {
  attr(x, "args")[[name]] <- value
  x
}

#' Drop stale attributes from a combined ARD object
#'
#' @param x An object to check and modify.
#' @return The modified object with stale attributes removed if it inherits
#'   from 'bind_ard' and contains an 'args' attribute.
#' @noRd
drop_bind_ard_attr <- function(x) {
  if (is_bind_ard_cards(x) && !is.null(attr(x, "args"))) {
    x <- set_card_attr(x, "by", NULL)
    x <- set_card_attr(x, "variable", NULL)
    x <- set_card_attr(x, "strata", NULL)
  }

  x
}
