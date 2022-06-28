#' Make mock data for display shells
#'
#' @param tfrmt tfrmt object
#' @param .default Number of unique levels to create for group/label values set to ".default"
#' @param n_cols Number of columns in the output table (not including group/label variables)
#'
#' @return tibble containing mock data
#'
#' @importFrom tidyr crossing unnest expand
#' @importFrom dplyr rowwise mutate pull rename ungroup coalesce group_by tibble across cur_column
#' @importFrom purrr map map_dfr map_chr map_dfc
#' @importFrom rlang as_name
#' @importFrom tidyselect everything all_of
#'
#' @noRd
make_mock_data <- function(tfrmt, .default = 1:3, n_cols = 3){
  body_plan <- tfrmt$body_plan
  grp_vars <- tfrmt$group %>% map_chr(as_name)

  # create tibble of all frmt_structure grp/label/param: 1 row per group_val per frmt_structure
  all_frmt_spec <- body_plan %>%
    map_dfr(
      function(x){
        crossing(
            # if group_val is a named list, return as a tibble with list names as colnames
            # otherwise (group_val = ".default") convert to tibble with colname "grp"
            if(is.list(x$group_val)) as_tibble(x$group_val) else tibble(..grp = x$group_val[[1]]),
            !!tfrmt$label := x$label_val,
            !!tfrmt$param := x$param_val
          )
    },
    .id = "frmt_num")


  # find out which group variables are not present
  cols_to_add <- setdiff(c("..grp", grp_vars), names(all_frmt_spec))

  # add the missing group variables & fill using .grp
  # & replace .default's
  all_frmt_vals <- bind_cols(all_frmt_spec,
                             map(cols_to_add, function(x) tibble(!!x := NA_character_))) %>%
    mutate(..grp = replace_na(.data$..grp, ".default"),
           across(grp_vars, ~ coalesce(.x, .data$..grp))) %>%
    select(-.data$..grp)  %>%
    rowwise()  %>%
    mutate(across(!!tfrmt$param, ~ process_for_mock(.x, cur_column(), 1)),
           across(all_of(grp_vars), ~ process_for_mock(.x, cur_column(), .default)))

  expand_cols <- c(tfrmt$group,  tfrmt$param)

  if(!quo_is_missing(tfrmt$label)){
    all_frmt_vals <- all_frmt_vals %>%
      mutate(
        across(!!tfrmt$label, ~ process_for_mock(.x, cur_column(), .default))
      )
    expand_cols <- c(expand_cols, tfrmt$label )
  }

  output_dat <- all_frmt_vals %>%
    unnest(everything()) %>%
    group_by(.data$frmt_num) %>%
    expand(!!!expand_cols) %>%
    ungroup

  ## add sorting_cols. Not functional, will not impact actual output
  if(!is.null(tfrmt$sorting_cols)){

    sorting_cols_vars <- tfrmt$sorting_cols %>% map_chr(as_name)
    n_sorting_cols <- length(sorting_cols_vars)

    sorting_cols_def <- map_dfc(seq_len(n_sorting_cols), function(x){
      tibble(!!sorting_cols_vars[x] := 1)
    })

    output_dat <- output_dat %>%
      mutate(
        `__tfrmt__mock__sorting_col` = list(sorting_cols_def)
      ) %>%
      unnest(.data$`__tfrmt__mock__sorting_col`)
  }


  ## add `column` columns
  column_vars <- tfrmt$column %>% map_chr(as_name)
  n_spans <- length(column_vars)
  col_def <- tibble(!!column_vars[n_spans] := paste0("col", seq(1:n_cols)))
  if(n_spans > 1){
    col_spans_df <- map_dfc(seq_len(n_spans-1), function(x){
      tibble(!!column_vars[x] := rep(paste0("span_", column_vars[x]), n_cols))
    })
    col_def <- bind_cols(col_spans_df, col_def)
  }
  output_dat <- output_dat %>%
    mutate(
      `__tfrmt__mock__columns` = list(col_def)
    ) %>%
    unnest(.data$`__tfrmt__mock__columns`)

  # remove the frmt_num field
  output_dat %>%
    select(-.data$frmt_num)

}


#' Given the input value, generate values to be used for mock data
#'
#' @param x input character value
#' @param column type of column (group, label, param)
#' @param .default Number of numbered values to create
#'
#' @return list of character values
#' @noRd
process_for_mock <-function(x, column, .default = 1:3){

      if(x == ".default"){
        str_c(column, "_", .default) %>% list
      } else {
        list(x)
      }
}
