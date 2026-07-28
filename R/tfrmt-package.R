#' @keywords internal
"_PACKAGE"

# `vars()` is a bit problematic since vars is deprecated in tidyselect, but
# dplyr still re-exports it (tidyselect no longer does)

# something weird is going on with f_rhs which seems to be overloaded (i.e.
# we are replacing it with out own version and sometimes using that instead of
# the rlang one) - in fmt_when

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
