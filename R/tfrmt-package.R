#' @keywords internal
"_PACKAGE"

# `vars()` is a bit problematic since vars is deprecated in tidyselect, but
# dplyr still re-exports it (tidyselect no longer does)

# something weird is going on with f_rhs which seems to be partially overloaded
# (is is being replaced with our own version but only when called inside
# `fmt_when()`)

## usethis namespace: start
#' @importFrom dplyr vars
#'
#' @importFrom rlang f_rhs f_rhs<-
#'
#' @importFrom rlang := !! !!! %||% .data
## usethis namespace: end
NULL

utils::globalVariables(
    c(
        ".",
        ".data"
    )
)
