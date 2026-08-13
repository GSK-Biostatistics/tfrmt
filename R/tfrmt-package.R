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
#' @importFrom rlang abort as_function as_label as_name caller_call caller_env
#' @importFrom rlang check_dots_empty0 .data eval_bare eval_tidy expr expr_text
#' @importFrom rlang f_lhs f_rhs f_rhs<- frame_call inform is_empty is_quosure
#' @importFrom rlang is_quosures list2 parse_expr parse_exprs quo quo_get_expr
#' @importFrom rlang quo_is_missing quo_name sym syms warn
#' @importFrom rlang := !! !!! %||%
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
