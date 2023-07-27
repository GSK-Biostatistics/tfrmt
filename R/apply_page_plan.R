#' Apply page structure to data
#'
#' @param .data data
#' @param page_struct_list list of page structure objects
#' @param group symbolic list of grouping
#' @param label symbolic label column
#' @param note_loc location of page note
#'
#' @noRd
#' @importFrom purrr map map_dbl
#' @importFrom dplyr tibble row_number mutate group_by slice arrange left_join select desc  filter pull summarise last lag
#' @importFrom tidyr unnest nest pivot_longer drop_na
#' @importFrom tidyselect starts_with
apply_page_struct <- function(.data, page_struct_list, group, label, note_loc){

  .data <- .data %>%
    mutate(TEMP_row = row_number())

  # 1. check that only 1 page_structure contains a .default, drop extras
  struct_defaults_idx <- which(map_lgl(page_struct_list, detect_default))
  if (length(struct_defaults_idx)>1){
    struct_defaults_idx_drop <- struct_defaults_idx[-last(struct_defaults_idx)]
    page_struct_list <- page_struct_list[-struct_defaults_idx_drop]
    message("`page_plan` contains multiple `page_structures` with values set to \".default\". \n",
            "Only the last one specified will be used.")
  }

  # 2. If applicable, split by .default
  struct_defaults_idx <- which(map_lgl(page_struct_list, detect_default))

  if (length(struct_defaults_idx)>0){
    grping <- expr_to_grouping(page_struct_list[[struct_defaults_idx]], group, label)

    dat_split_1 <- .data %>%
      group_by(across(all_of(grping)))%>%
      nest(.key = "..tfrmt_data") %>%
      ungroup() %>%
      mutate(`..tfrmt_split_num` = row_number())

    tbl_nms <- dat_split_1 %>%
      select(-"..tfrmt_data") %>%
      pivot_longer(cols = -.data$`..tfrmt_split_num`, names_to = "grouping_col", values_to = "grouping_val") %>%
      group_by(.data$`..tfrmt_split_num`) %>%
      filter(!is.na(.data$grouping_val)) %>%
      summarise(`..tfrmt_page_note` = paste0(.data$grouping_col, ": ", .data$grouping_val) %>% paste0(collapse = ",\n"))

    dat_split_1 <- left_join(dat_split_1, tbl_nms, by = "..tfrmt_split_num") %>%
      select(-"..tfrmt_split_num")
  } else {
    dat_split_1 <- tibble(`..tfrmt_data` = list(!!.data))
  }

  # 3. Further split the sub-data based on specific values (loop over all combinations of page_structure & data)
  dat_split_2 <- dat_split_1 %>%
    mutate(split_idx = map(.data$`..tfrmt_data`, function(x){
      map_dbl(page_struct_list, function(y){
        page_test_data(y, x, group, label)
      })
    })) %>%
    mutate(`..tfrmt_data` = map2(.data$`..tfrmt_data`, .data$split_idx, function(x, y){
      x %>%
        mutate(`..tfrmt_split_after` = .data$TEMP_row %in% y,
               `..tfrmt_split_after` = cumsum(.data$`..tfrmt_split_after`<lag(.data$`..tfrmt_split_after`, default = FALSE))) %>%
        group_by(.data$`..tfrmt_split_after`) %>%
        group_split(.keep = FALSE)
    })) %>%
    select(-"split_idx") %>%
    unnest(cols = .data$`..tfrmt_data`)


  pg_note <- NULL
  if ("..tfrmt_page_note" %in% names(dat_split_2)){
    pg_note <- dat_split_2$`..tfrmt_page_note`
  }

  dat_split_2 %>%
        mutate(`..tfrmt_data` = map(.data$`..tfrmt_data`, ~select(.x, - "TEMP_row"))) %>%
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
#' @return tibble filtered to the rows where a split occurs following
#' @noRd
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang parse_expr
page_test_data <- function(cur_struct, .data, group, label){

  grp_expr <- "TRUE"
  lbl_expr <- "TRUE"
  keep_vars <- NULL

  # only do this if cur_struct contains a non-default value
  if (detect_non_default(cur_struct$group_val)){
    grp_expr <- expr_to_filter(group, cur_struct$group_val)

    if (!is.list(cur_struct$group_val)){
      keep_vars <- group
    } else {
      keep_vars <- group[map_lgl(cur_struct$group_val, ~!.x==".default")]
    }
  }

  if (detect_non_default(cur_struct$label_val)){
    lbl_expr <- expr_to_filter(label, cur_struct$label_val)
    keep_vars <- c(keep_vars, label)
  }

  if (!is.null(keep_vars)){
    filter_expr <- paste(
      c(lbl_expr,grp_expr),
      collapse = "&"
    ) %>%
      parse_expr()

    .data %>%
      filter(!!filter_expr) %>%
      select(any_of(c(map_chr(keep_vars, as_label), "TEMP_row"))) %>%
      # split only after non-consecutive sequence
      mutate(breaks = .data$TEMP_row==lag(.data$TEMP_row, default = 0)+1,
             breaks = cumsum(!.data$breaks)) %>%
      group_by(.data$breaks) %>%
      slice(n()) %>%
      pull(.data$TEMP_row)

  } else {
    0
  }

}

# detect use of .default
detect_default <- function(struct){

  map_lgl(struct, ~ any(!is.null(.x) && any(.x==".default"))) %>% any()
}

# detect use of non-default
detect_non_default <- function(struct_val){

  any(!is.null(struct_val) && any(!struct_val==".default"))

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
