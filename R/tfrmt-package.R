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
#' @importFrom gt cells_body cell_borders cells_column_labels cells_column_spanners cells_row_groups cells_stub cell_text cols_hide cols_label default_fonts gt gt_group md opt_footnote_marks opt_table_font px sub_missing tab_footnote tab_header tab_options tab_source_note tab_spanner tab_stubhead tab_style text_transform
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
