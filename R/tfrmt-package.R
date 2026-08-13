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
#' @importFrom glue glue glue_collapse
#'
#' @importFrom jsonlite parse_json read_json toJSON validate
#'
#' @importFrom purrr discard keep list_flatten map map_chr map_dbl map_dfc
#' @importFrom purrr map_dfr map_lgl map2 map2_chr map2_dfr map2_lgl pmap
#' @importFrom purrr pmap_chr reduce quietly safely
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
