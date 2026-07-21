#' @keywords internal
"_PACKAGE"

# `vars()` is a bit problematic since vars is deprecated (it originally came
# from tidyselect and dplyr re-exports it, but it is no longer exported by
# tidyselect)

## usethis namespace: start
#' @importFrom dplyr vars
#'
#' @importFrom gt cells_body cell_borders cells_column_labels cells_column_spanners cells_row_groups cells_stub cell_text cols_hide cols_label default_fonts gt gt_group md opt_footnote_marks opt_table_font px sub_missing tab_footnote tab_header tab_options tab_source_note tab_spanner tab_stubhead tab_style text_transform
#'
#' @importFrom purrr discard keep list_flatten map map_chr map_dbl map_dfc
#' @importFrom purrr map_dfr map_lgl map2 map2_chr map2_dfr map2_lgl pmap
#' @importFrom purrr pmap_chr reduce quietly safely
#'
#' @importFrom rlang abort as_function as_label as_name caller_call caller_env
#' @importFrom rlang check_dots_empty0 .data eval_bare eval_tidy expr expr_text
#' @importFrom rlang f_lhs f_rhs f_rhs<- frame_call inform is_empty is_quosure
#' @importFrom rlang is_quosures list2 parse_expr parse_exprs quo quo_get_expr
#' @importFrom rlang quo_is_missing quo_name sym syms warn
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
