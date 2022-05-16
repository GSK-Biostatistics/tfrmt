#' Apply row group block formatting to data
#'
#' @param .data data
#' @param row_grp_plan row group plan object
#' @param group symbolic list of grouping
#'
#' @noRd
#' @importFrom dplyr tibble mutate group_by arrange slice group_map case_when left_join row_number select summarise across
#' @importFrom purrr map map2_dfr
#' @importFrom tidyr unnest nest
#' @importFrom tidyselect everything
#' @importFrom rlang !!!
apply_row_grp_plan <- function(.data, row_grp_plan, group, label, ...){

  # determine which rows each block applies to
  .data <- .data %>%
    mutate(TEMP_row = row_number())

  .data <- .data %>% combine_group_cols(group,
                               label,
                               row_grp_plan$spanning_label)

  TEMP_appl_row <- row_grp_plan$struct_ls %>%
    map(grp_row_test_data, .data, group)
  TEMP_block_to_apply <- row_grp_plan$struct_ls %>% map(~.$block_to_apply[[1]])

  # similar to frmts, only allow 1 element_block for a given row
  #   - within block-specific data, split data further by grouping vars
  dat_plus_block <- tibble(
    TEMP_appl_row,
    TEMP_block_to_apply) %>%
    mutate(TEMP_block_rank = row_number()) %>%
    unnest(cols = c(TEMP_appl_row)) %>%
    group_by(TEMP_appl_row) %>%
    arrange(TEMP_appl_row, desc(.data$TEMP_block_rank)) %>%
    slice(1) %>%
    left_join(.data, ., by= c("TEMP_row" = "TEMP_appl_row")) %>%
    group_by(.data$TEMP_block_rank, .data$TEMP_block_to_apply ) %>%
    nest() %>%
    mutate(data = case_when(
      is.na(TEMP_block_rank) ~ map(.data$data, list),
      TRUE ~ map(.data$data, ~ .x %>%  group_by(!!!group) %>% group_map(~as_tibble(.x), .keep = TRUE))
    )) %>%
    unnest(.data$data)

  # get max character width for each column in the full data
  dat_max_widths <- .data %>% summarise(across(everything(), ~max(nchar(.x))))

  # apply group block function to data subsets
  map2_dfr(dat_plus_block$data,
           dat_plus_block$TEMP_block_to_apply,
           function(x,y) {
             if(is.null(y)){
               x
             } else {
               apply_grp_block(.data = x,
                               group = group,
                               element_block = y,
                               widths = dat_max_widths)
             }
           }) %>%
    arrange(.data$TEMP_row) %>%
    select(-.data$TEMP_row)
}



#' Test of the grp rows in the data
#'
#' @param cur_block current row group block
#' @param .data data to test against NOTE: `TEMP_row` must be in the dataset
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
#' @param widths tibble of widths for each column
#'
#' @return dataset with element block applied
#'
#' @importFrom dplyr slice n mutate across bind_rows cur_column
#' @importFrom tidyr fill
#' @importFrom purrr map_chr
#' @importFrom tidyselect vars_select_helpers
#' @importFrom rlang !!!
#'
#' @export
apply_grp_block <- function(.data, group, element_block, widths){

  if (!is.null(element_block$post_space)){
    # create add-on row
    # utilize TEMP_row to retain the ordering
    grp_row_add <- .data %>%
      slice(n()) %>%
      mutate(across(c(-map_chr(group, as_name), -vars_select_helpers$where(is.numeric)),
                    ~ replace(.x, values = fill_post_space(post_space = element_block$post_space,
                                                           width = widths[[cur_column()]]))),
             TEMP_row = .data$TEMP_row + 0.1)

    # combine with original data
    bind_rows(.data, grp_row_add) %>%
      fill(!!!group)
  } else {
    .data
  }

}


#' Fill the cell value with post space character
#'
#' @param post_space Character value for post space
#' @param width width to make the post_space value in order to fill the cell
#'
#' @return character value containing post space value modified to fill cell
#' @noRd
#'
#' @importFrom stringr str_sub
fill_post_space <- function(post_space, width){

  length_post_space <- nchar(post_space)
  reps <- ceiling(width/length_post_space)
  fill_val <- rep(post_space, reps) %>% paste(collapse = "") %>% str_sub(1, width)

  return(fill_val)

}


#' Combine group cols into the Labels
#'
#' @param .data Pre-processed data that just needs columns combining
#' @param group list of the group parameters
#' @param label label symbol should only be one
#' @param spanning_label Boolean of whether or not the highest group should be spanning
#'
#' @return dataset with the group columns combines
#' @noRd
combine_group_cols <- function(.data, group, label, spanning_label){
  if(spanning_label == TRUE & length(group) > 0){
    group = group[-length(group)]
  }

  while(length(group) > 0){
    split_dat <- .data %>%
      group_by(!!!group) %>%
      group_split()

    .data<- split_dat %>%
      map_dfr(function(lone_dat){
        new_row <- lone_dat %>%
          select(!!!group, !!label) %>%
          mutate(!!label := !!last(group)) %>%
          distinct()
        lone_dat %>%
          mutate(!!label := str_c("  ", !!label)) %>%
          bind_rows(new_row, .)
      })
    group = group[-length(group)]
  }
  .data

}
