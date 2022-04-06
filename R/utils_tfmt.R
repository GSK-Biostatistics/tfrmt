apply_tfmt <- function(.data, tfmt){

  apply_all_fmts(.data, tfmt$body_style, group = tfmt$group, label = tfmt$label, param = tfmt$param, tfmt$values) %>%
    pivot_wider(names_from = !!tfmt$column,
                values_from = !!tfmt$values) %>%
    arrange(!!!tfmt$sorting_cols) %>%
    select(!!!tfmt$col_select)
}
