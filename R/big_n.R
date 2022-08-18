#' Big N Structure
#'
#' Big N structure allows you to specify which values should become the subject totals ("big N" values)
#' and how they should be formatted in the table's column labels. Values are specified by providing
#' the value(s) of the `param` column for which the values are big N's.
#' This will remove these from the body of the table and place them into columns
#' matching the values in the column column(s). The default formatting is `N = xx`,
#' on its own line, but that can be changed by providing a different `frmt()` to
#' `n_frmt`
#'
#' @param param_val row value(s) of the parameter column for which the values
#'   are big n's
#' @param n_frmt [frmt()] to control the formatting of the big n's
#'
#' @return big_n_structure object
#' @export
#'
big_n_structure<- function(param_val, n_frmt = frmt("\nN = xx")){
  structure(
    list(
      param_val = param_val,
      n_frmt = n_frmt
    ),
    class = c("big_n_structure","structure")
  )
}



apply_big_n_df <- function(col_plan_vars, columns, value, big_n_df){

  if(!is.null(big_n_df) && nrow(big_n_df) > 0){
    col_lab <- columns %>% map_chr(as_label)
    data_names <- col_plan_vars %>%
      as_vars() %>% # Ensures col_plan_vars is a vars w/ names
      map_chr(as_label) %>%
      split_data_names_to_df(data_names= c(), preselected_cols = .,
                             column_names = col_lab)

    for(i in seq(nrow(big_n_df))){
      big_n_i <- big_n_df %>%
        slice(i)
      data_names <- data_names %>%
        mutate(!!big_n_i$name := if_else(!!parse_expr(big_n_i$exp),
                                         paste0(!!sym(big_n_i$name),
                                                pull(big_n_i, !!value)),
                                         !!sym(big_n_i$name)
        ))
    }

    out <- unite_df_to_data_names(data_names, preselected_cols = c(), column_names = col_lab)
  } else {
    out <- col_plan_vars
  }
  out
}

remove_big_ns <- function(.data, param, big_n_structure){
  if(!is.null(big_n_structure)){
    .data <- .data %>%
      filter(!(!!param) %in% big_n_structure$param_val)
  }
  .data
}

#' @importFrom dplyr slice_tail
get_big_ns <-  function(.data, param, value, columns, big_n_structure, mock){
  if(!is.null(big_n_structure)){
    frmtted_vals <- .data %>%
      filter((!!param) %in% big_n_structure$param_val) %>%
      apply_frmt.frmt(big_n_structure$n_frmt, ., value, mock) %>%
      select(!!!columns, !!value)
    # Test for missing big n's
    if(nrow(frmtted_vals) == 0){
      warning("Unable to add big n's as there are no matching parameter values in the given ARD")
    }

    # Test for too many big n's
    multi_test <- frmtted_vals %>%
      group_by(!!!columns) %>%
      summarise(n = n()) %>%
      filter(n > 1)
    if(nrow(multi_test) > 0){

      warn_df <- multi_test %>%
        select(-n)

      warning(c("The following columns have multiple Big N's associated with them :\n", warn_df),
              call. = FALSE)
    }
    .data <- frmtted_vals %>%
      mutate(`_tfrmt______id` = row_number()) %>%
      pivot_longer(-c(`_tfrmt______id`, !!value)) %>%
      filter(!is.na(value)) %>%
      group_by(`_tfrmt______id`) %>%
      mutate(exp = paste0(name, "=='", value, "'",  collapse = "&"),
             name = paste0("__tfrmt_new_name__", name)) %>%
      slice_tail() %>%
      ungroup()%>%
      select(-`_tfrmt______id`)

  } else {
    .data <- NULL
  }
  .data
}
