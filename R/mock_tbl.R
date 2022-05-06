#' Make mock data for display shells
#'
#' @param tfrmt tfrmt object
#' @param .default Number of unique levels to create for group/label values set to ".default"
#' @param n_cols Number of columns in the output table (not including group/label variables)
#'
#' @return tibble containing mock data
#' @export
#'
#' @importFrom tidyr crossing unnest expand
#' @importFrom dplyr rowwise mutate pull rename ungroup coalesce group_by tibble across
#' @importFrom purrr map map_dfr map_chr
#' @importFrom rlang as_name
#' @importFrom tidyselect everything
make_mock_data <- function(tfrmt, .default = 1:3, n_cols = 3){
  body_style <- tfrmt$body_style
  grp_vars <- c(".grp",
                tfrmt$group %>% map_chr(as_name))

  # create tibble of all frmt_structure grp/label/param: 1 row per group_val per frmt_structure
  all_frmt_spec <- body_style %>%
    map_dfr(
      function(x){
        crossing(
            # if group_val is a named list, return as a tibble with list names as colnames
            # otherwise (group_val = ".default") convert to tibble with colname "grp"
            if(is.list(x$group_val)) as_tibble(x$group_val) else tibble(.grp = x$group_val[[1]]),
            lab = x$label_val,
            param = x$param_val
          )
    },
    .id = "frmt_num")


  # find out which group variables are not present
  cols_to_add <- setdiff(grp_vars, names(all_frmt_spec))

  # add the missing group variables & fill using .grp
  # & replace .default's
  all_frmt_vals <- bind_cols(all_frmt_spec,
                             map(cols_to_add, function(x) tibble(!!x := NA_character_))) %>%
    mutate(.grp = replace_na(.data$.grp, ".default"),
           across(-c(.data$.grp, .data$lab, .data$param, .data$frmt_num), function(x) coalesce(x, .data$.grp))) %>%
    select(-.data$.grp)  %>%
    rowwise() %>%
    mutate(lab = process_for_mock(.data$lab, "label", .default),
           param = process_for_mock(.data$param, "param", .default = 1),
           across(-c(.data$lab, .data$param, .data$frmt_num), function(x) process_for_mock(x, "group", .default)))

  col_names <- paste0("col", seq(1:n_cols))

  # Within a frmt, do all combinations of values
  all_frmt_vals %>%
    unnest(everything()) %>%
    rename(!!tfrmt$label := .data$lab,
           !!tfrmt$param := .data$param) %>%
    group_by(.data$frmt_num) %>%
    expand(!!!tfrmt$group, !!tfrmt$label, !!tfrmt$param) %>%
    ungroup %>%
    crossing(!!tfrmt$column := col_names) %>%
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
        str_c(column, .default) %>% list
      } else {
        list(x)
      }
}
