#' Apply page structure to data
#'
#' @param .data data
#' @param page_struct_list list of page structure objects
#' @param group symbolic list of grouping
#' @param label symbolic label column
#' @param note_loc location of page note
#'
#' @noRd
#' @importFrom purrr map
#' @importFrom dplyr tibble row_number mutate group_by slice arrange left_join select desc  filter pull summarise
#' @importFrom tidyr unnest nest pivot_longer
#' @importFrom tidyselect starts_with
apply_page_struct <- function(.data, page_struct_list, group, label, note_loc){

  .data <- .data %>%
    mutate(TEMP_row = row_number())

  # 1. get the first round of sub-tables - based on values not set to .default
  TEMP_appl_row <- page_struct_list %>%
    map(page_test_data, .data, group, label)

  # merge with data
  dat_plus_split <- tibble(TEMP_appl_row) %>%
    mutate(`..tfrmt_struct_num` = row_number()) %>%
    unnest(cols = .data$TEMP_appl_row) %>%
    group_by(.data$TEMP_appl_row) %>%
    arrange(.data$TEMP_appl_row, desc(.data$`..tfrmt_struct_num`)) %>%
    slice(1) %>%
    left_join(.data, ., by= c("TEMP_row" = "TEMP_appl_row")) %>%
    arrange(.data$TEMP_row) %>%
    select(-"TEMP_row")

  # fill missings so every row has a table index
  dat_plus_split <- dat_plus_split %>%
    fill(.data$`..tfrmt_struct_num`, .direction = "up") %>%
    mutate(`..tfrmt_struct_num` = replace_na(.data$`..tfrmt_struct_num`, max(.data$`..tfrmt_struct_num`, na.rm = TRUE) + 1))

  tbls_split_1 <- dat_plus_split %>%
    group_by(.data$`..tfrmt_struct_num`) %>%
    nest(.key = "..tfrmt_data") %>%
    mutate(`..tfrmt_struct` = page_struct_list[.data$`..tfrmt_struct_num`])

  # 2. get any groupings to further split by - based on values set to .default
  tbls_split_1_grp <- tbls_split_1 %>%
    mutate(`..tfrmt_grouping` = map(.data$`..tfrmt_struct`, expr_to_grouping, group, label)) %>%
    ungroup

  # 3. further split the tables by the grouping variables
  tbls_split_2 <- tbls_split_1_grp %>%
    mutate(`..tfrmt_data` = map2(.data$`..tfrmt_data`, .data$`..tfrmt_grouping`, function(x, y) {
      x %>%
        group_by(across(all_of(y))) %>%
        nest(.key = "..tfrmt_data")
     })) %>%
    unnest(cols = .data$`..tfrmt_data`)  %>%
    mutate(`..tfrmt_page_num` = row_number())

  # 4. add the names
  if (ncol(select(tbls_split_2, - starts_with("..tfrmt")))>0){
    tbl_nms <- tbls_split_2 %>%
      select(-starts_with("..tfrmt"), "..tfrmt_page_num") %>%
      pivot_longer(cols = -.data$`..tfrmt_page_num`, names_to = "grouping_col", values_to = "grouping_val") %>%
      group_by(.data$`..tfrmt_page_num`) %>%
      filter(!is.na(.data$grouping_val)) %>%
      summarise(`..tfrmt_page_note` = paste0(.data$grouping_col, ": ", .data$grouping_val) %>% paste0(collapse = ",\n"))

    tbls_split_2 <- left_join(tbls_split_2, tbl_nms, by = "..tfrmt_page_num")
  }

  pg_note <- NULL
  if ("..tfrmt_page_note" %in% names(tbls_split_2)){
    pg_note <- tbls_split_2$`..tfrmt_page_note`
  }

  tbls_split_2 %>%
        pull(.data$`..tfrmt_data`) %>%
        setNames(pg_note)

}

#' Test of the page rows in the data
#'
#' @param cur_struct current page struct
#' @param .data data to test against
#' @param group list of the group parameters
#' @param label label symbol should only be one
#'
#' @return tibble filtered to the rows for the table
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
    filter(!!filter_expr) %>%
    pull(.data$TEMP_row)
}

#' Create the group_by expression for the data
#'
#' @param cur_struct current page struct
#' @param group list of the group parameters
#' @param label label symbol should only be one
#'
#' @return list of indices for each of the tables
#' @noRd
expr_to_grouping <- function(cur_struct, group, label){

  grouping <- NULL

  if (!is.null(cur_struct$group_val)){
    if(!is.list(cur_struct$group_val) && cur_struct$group_val==".default"){
      grp_to_add <- map_chr(group, as_label)
      grouping <- c(grouping, grp_to_add)
    } else if (is.list(cur_struct$group_val) && any(cur_struct$group_val==".default")){
      grp_to_add <- names(cur_struct$group_val)[map_lgl(cur_struct$group_val, ~.x==".default")]
      grouping <- c(grouping, grp_to_add)
    }
  }
  if (!is.null(cur_struct$label_val) && cur_struct$label_val==".default"){
    grouping <- c(grouping, as_label(label))
  }

  grouping
}
