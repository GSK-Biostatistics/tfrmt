#' Apply page structure to data
#'
#' @param .data data
#' @param page_struct_list list of page structure objects
#' @param group symbolic list of grouping
#' @param label symbolic label column
#'
#' @noRd
#' @importFrom purrr map
#' @importFrom dplur mutate row_number group_by arrange slice left_join select desc
#' @importFrom tidyr unnest replace_na fill
apply_page_struct <- function(.data, page_struct_list, group, label){

  .data <- .data %>%
    mutate(TEMP_row = row_number())

  # get indices of each tbl section
  TEMP_appl_row <- page_struct_list %>%
    map(page_test_data, .data, group, label)

  # merge with data
dat_plus_split <- tibble(TEMP_appl_row) %>%
  mutate(`..tfrmt_page_num` = row_number()) %>%
  unnest(cols = c(TEMP_appl_row)) %>%
  group_by(TEMP_appl_row) %>%
  arrange(TEMP_appl_row, desc(.data$`..tfrmt_page_num`)) %>%
  slice(1) %>%
  left_join(.data, ., by= c("TEMP_row" = "TEMP_appl_row")) %>%
  arrange(TEMP_row) %>%
  select(-TEMP_row)

# fill missings so every row has a table index
dat_plus_split %>%
  fill(`..tfrmt_page_num`, .direction = "up") %>%
  mutate(`..tfrmt_page_num` = replace_na(`..tfrmt_page_num`, max(`..tfrmt_page_num`, na.rm = TRUE) + 1))
}

#' Test of the page rows in the data
#'
#' @param cur_struct current page struct
#' @param .data data to test against NOTE: `TEMP_row` must be in the dataset
#' @param group list of the group parameters
#' @param label label symbol should only be one
#'
#' @return vector of the rows where a page split follows
#' @noRd
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang parse_expr
page_test_data <- function(cur_struct, .data, group, label){

  grp_expr <- expr_to_filter(group, cur_struct$group_val)
  lbl_expr <- expr_to_filter(label, cur_struct$label_val)

  filter_expr <- paste(
    c(lbl_expr,grp_expr),
    collapse = "&"
  ) %>%
    parse_expr()

  .data %>%
    filter(!!filter_expr)%>%
    pull(.data$TEMP_row)
}

