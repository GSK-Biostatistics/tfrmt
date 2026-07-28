#' @keywords internal
"_PACKAGE"

# `vars()` is a bit problematic since vars is deprecated in tidyselect, but
# dplyr still re-exports it (tidyselect no longer does)

## usethis namespace: start
#' @importFrom dplyr vars
#'
#' @importFrom rlang .data
#' @importFrom rlang f_lhs f_rhs f_rhs<-
#' @importFrom rlang list2
#' @importFrom rlang sym syms
#' @importFrom rlang := !! !!! %||%
## usethis namespace: end
NULL

utils::globalVariables(
    c(
        ".",
        ".data",
        ".rename_col",
        ".original_col",
        "df_names",
        "new_name_in_df",
        "param_list",
        "label_quote",
        "label_collapse"
    )
)
