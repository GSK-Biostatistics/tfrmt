#' Apply row group block formatting to data
#'
#' @param .data data
#' @param row_grp_plan row group plan object
#' @param group symbolic list of grouping
#'
#' @noRd
#' @importFrom dplyr tibble mutate group_by arrange slice group_map case_when left_join row_number select
#' @importFrom purrr map map2_dfr
#' @importFrom tidyr unnest nest
#' @importFrom rlang !!!
apply_row_grp_plan <- function(.data, row_grp_plan, group, ...){

  .data <- .data %>%
    mutate(TEMP_row = row_number())

  TEMP_appl_row <- row_grp_plan %>%
    map(grp_row_test_data, .data, group)
  TEMP_block_to_apply <- row_grp_plan %>% map(~.$block_to_apply[[1]])

  dat_plus_block <- tibble(
    TEMP_appl_row,
    TEMP_block_to_apply) %>%
    mutate(TEMP_block_rank = row_number()) %>%
    unnest(cols = c(TEMP_appl_row)) %>%
    group_by(TEMP_appl_row) %>%
    arrange(TEMP_appl_row, desc(.data$TEMP_block_rank)) %>%
    slice(1) %>%
    left_join(.data, ., by= c("TEMP_row" = "TEMP_appl_row")) %>%
    group_by(TEMP_block_rank, TEMP_block_to_apply ) %>%
    nest() %>%
    mutate(data = case_when(
      is.na(TEMP_block_rank) ~ map(data, list),
      TRUE ~ map(data, ~ .x %>%  group_by(!!!group) %>% group_map(~as_tibble(.x), .keep = TRUE))
    )) %>%
    unnest(data)

  map2_dfr(dat_plus_block$data,
           dat_plus_block$TEMP_block_to_apply,
           function(x,y) {
             if(is.null(y)){
               x
             } else {
               apply_grp_block(x, group = group, y)
             }
           }) %>%
    arrange(TEMP_row) %>%
    select(-TEMP_row)
}



#' Test of the grp rows in the data
#'
#' @param cur_block current row group block
#' @param data data to test against NOTE: `TEMP_row` must be in the dataset
#' @param group list of the group parameters
#'
#' @return vector of the rows which this format could be applied to
#' @noRd
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang parse_expr
grp_row_test_data <- function(cur_block, .data, group){

  filter_expr <- expr_to_filter.quosures(group, cur_block$group_val) %>%
    parse_expr()

  .data %>%
    filter(!!filter_expr) %>%
    pull(.data$TEMP_row)
}


#' Apply row group element blocks
#'
#' @param .data data, but only what is getting changed
#' @param group list of the group parameters
#' @param element_block element_block to be applied
#'
#' @return dataset with element block applied
#'
#' @importFrom dplyr slice n mutate across bind_rows
#' @importFrom tidyr fill
#' @importFrom purrr map_chr
#' @importFrom tidyselect vars_select_helpers
#' @importFrom rlang !!!
#'
#' @export
apply_grp_block <- function(.data, group, element_block){

    # create add-on row
  # utilize TEMP_row to retain the ordering
  grp_row_add <- .data %>%
    slice(n()) %>%
    mutate(across(c(-map_chr(group, as_name), -vars_select_helpers$where(is.numeric)),
                  ~ replace(.x, values = element_block$post_space)),
           TEMP_row = TEMP_row + 0.1)

  # combine with original data
  bind_rows(.data, grp_row_add) %>%
    fill(!!!group)

}
