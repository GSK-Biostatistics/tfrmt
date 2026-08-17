#' @keywords internal
"_PACKAGE"

# `vars()` is a bit problematic since vars is deprecated (it originally came
# from tidyselect and dplyr re-exports it, but it is no longer exported by
# tidyselect)

## usethis namespace: start
#' @importFrom dplyr vars
#'
#' @importFrom forcats fct_inorder
#'
#' @importFrom rlang .data := !! !!! %||%
#'
#' @importFrom stats as.formula setNames
#'
#' @importFrom tidyr crossing expand fill nest pivot_longer pivot_wider
#' @importFrom tidyr replace_na separate unnest unnest_longer unite
#'
#' @importFrom tidyselect starts_with
#'
#' @importFrom utils capture.output
#'
## usethis namespace: end
NULL
