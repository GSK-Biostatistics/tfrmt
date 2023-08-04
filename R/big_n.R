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
#' @param by_page Option to include different big Ns for each group-defined set of pages (defined by any variables set to ".default" in the `page_plan`). Default is `FALSE`, meaning only the overall Ns are applied
#'
#' @seealso \href{https://gsk-biostatistics.github.io/tfrmt/articles/big_ns.html}{Link to related article}
#'
#' @return big_n_structure object
#' @export
#'
big_n_structure <- function(param_val, n_frmt = frmt("\nN = xx"), by_page = FALSE){

  if(!is_frmt(n_frmt) | is_frmt_combine(n_frmt) | is_frmt_when(n_frmt)){
    stop("`n_frmt` must be given a frmt object")
  }

  structure(
    list(
      param_val = param_val,
      n_frmt = n_frmt,
      by_page = by_page
    ),
    class = c("big_n_structure","structure")
  )
}



#' Applies the big N df to the column names
#'
#' @param col_plan_vars named string vector of columns
#' @param columns column quosure list
#' @param value value column quosure
#' @param big_n_df big_n_df
#'
#' @return named string vector
#' @noRd
apply_big_n_df <- function(big_n_df, col_plan_vars, columns, value){

  if(!is.null(big_n_df) && nrow(big_n_df) > 0){
    col_lab <- columns %>% map_chr(as_label)
    data_names <- col_plan_vars %>%
      map_chr(as_label) %>%
      split_data_names_to_df(data_names= c(), preselected_cols = .,
                             column_names = col_lab)

    for(i in seq(nrow(big_n_df))){
      big_n_i <- big_n_df %>%
        slice(i)
      data_names <- data_names %>%
        mutate(!!big_n_i$`__tfrmt_big_n_names__` := if_else(!!parse_expr(big_n_i$exp),
                                         paste0(!!sym(big_n_i$`__tfrmt_big_n_names__`),
                                                pull(big_n_i, !!value)),
                                         !!sym(big_n_i$`__tfrmt_big_n_names__`)
        ))
    }

    out <- unite_df_to_data_names(data_names, preselected_cols = c(), column_names = col_lab) %>%
      map(~char_as_quo(.x)) %>%
      do.call("vars",.)

  } else {
    out <- col_plan_vars
  }
  out
}

#' Remove big n's from data
#'
#' @param .data ARD
#' @param param param quosure
#' @param big_n_structure big_n_structure object
#'
#' @return ARD with big n's in
#' @noRd
remove_big_ns <- function(.data, param, big_n_structure){
  if(!is.null(big_n_structure)){
    .data <- .data %>%
      filter(!(!!param) %in% big_n_structure$param_val)
  }
  .data
}


#' Makes the big n data from the raw ARD
#'
#' @param .data raw ARD
#' @param param param quosures
#' @param value value quosures
#' @param columns list of column quosures
#' @param big_n_structure big_n_stcuture object
#' @param mock boolean if it is T/F
#' @return tibble of the formatted big n's and expressions for where each goes
#'
#' @importFrom dplyr slice_tail filter select
#' @importFrom tidyselect where
#' @noRd
get_big_ns <-  function(.data, param, value, columns, big_n_structure, mock){
  if(!is.null(big_n_structure)){

    frmtted_vals <- .data %>%
      filter((!!param) %in% big_n_structure$param_val) %>%
      apply_frmt.frmt(big_n_structure$n_frmt, ., value, mock)

    if (big_n_structure$by_page){
      frmtted_vals <-  frmtted_vals %>%
        select(!!!columns, !!value, where(~sum(is.na(.x))==0), -!!param)
    } else {
      frmtted_vals <-  frmtted_vals %>%
        select(!!!columns, !!value)
    }

    # Test for missing big n's
    if(nrow(frmtted_vals) == 0){
      warning("Unable to add big n's as there are no matching parameter values in the given ARD")
    }

    # Test for too many big n's
    grp_vars <- setdiff(names(frmtted_vals), as_label(value))
    multi_test <- frmtted_vals %>%
      group_by(across(all_of(grp_vars))) %>%
      summarise(n = n()) %>%
      filter(n > 1)
    if(nrow(multi_test) > 0){

      warn_df <- multi_test %>%
        select(-"n")

      warning(c("The following columns have multiple Big N's associated with them :\n", warn_df),
              call. = FALSE)
    }

    by_var <- setdiff(grp_vars, map_chr(columns, as_label))

    .data <- frmtted_vals %>%
      mutate(`_tfrmt______id` = row_number()) %>%
      pivot_longer(
        -c("_tfrmt______id", !!value, all_of(by_var)),
        names_to = "__tfrmt_big_n_names__",
        values_to = "__tfrmt_big_n_values__"
      ) %>%
      filter(!is.na(.data$`__tfrmt_big_n_values__`) & .data$`__tfrmt_big_n_values__` != "") %>%
      group_by(.data$`_tfrmt______id`) %>%
      mutate(exp = paste0(.data$`__tfrmt_big_n_names__`, "=='", .data$`__tfrmt_big_n_values__`, "'",  collapse = "&"),
             `__tfrmt_big_n_names__` = paste0("__tfrmt_new_name__", .data$`__tfrmt_big_n_names__`)) %>%
      slice_tail() %>%
      ungroup()%>%
      select(-"_tfrmt______id")

    if (big_n_structure$by_page){

     .data <-  .data %>%
        group_by(across(all_of(by_var))) %>%
       group_split()
    }

  } else {
    .data <- NULL
  }
  .data
}
