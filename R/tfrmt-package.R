#' @keywords internal
"_PACKAGE"

# `vars()` is a bit problematic since vars is deprecated (it originally came
# from tidyselect and dplyr re-exports it, but it is no longer exported by
# tidyselect)

# `tidyselect::starts_with()`` is also a bit problematic since tfrmt supports
# tidyselect specification of selected columns, but the implementation has not
# kept up with subsequent tidyselect changes

## usethis namespace: start
#' @importFrom dplyr vars
#'
#' @importFrom rlang .data := !! !!! %||%
#'
#' @importFrom tidyselect starts_with
#'
## usethis namespace: end
NULL

#  global variables
utils::globalVariables(
    c(
        "."
    )
)

# shared variables
.tlang_delim <- "___tlang_delim___"
.tlang_struct_col_prefix <- "__tlang_span_structure_column__"
