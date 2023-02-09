#' Apply row group structure formatting to data
#'
#' @param .data data
#' @param row_grp_struct_list list of row group structure objects
#' @param group symbolic list of grouping
#' @param label symbolic label column
#'
#' @noRd
#' @importFrom dplyr tibble mutate group_by arrange slice group_map case_when left_join row_number select summarise across
#' @importFrom purrr map map2_dfr
#' @importFrom tidyr unnest nest
#' @importFrom tidyselect everything
#' @importFrom rlang !!!
#' @importFrom stringr str_split
apply_row_grp_struct <- function(.data, row_grp_struct_list, group, label = NULL, ...){
  # Locate which groups need which formatting
  # determine which rows each block applies to
  .data <- .data %>%
    mutate(TEMP_row = row_number())

  TEMP_appl_row <- row_grp_struct_list %>%
    map(grp_row_test_data, .data, group)
  TEMP_block_to_apply <- row_grp_struct_list %>% map(~.$block_to_apply)

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
    unnest("data")

  # get max character width for each column in the full data
  dat_max_widths <- .data %>%
    summarise(across(everything(), function(x) {
      if (is.character(x)) {
        max(sapply(str_split(x, "\\n"), function(y) {
          max(nchar(y), na.rm = TRUE)
        }), na.rm = TRUE)
      } else{
        max(nchar(x), na.rm = TRUE)
      }
    }))

  # apply group block function to data subsets
  add_ln_df <- map2_dfr(dat_plus_block$data,
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
    select(-"TEMP_row")

  add_ln_df
}



#' Apply row group block labelled to data
#'
#' @param .data data
#' @param element_row_grp_loc element object specifying row group label location
#' @param group symbolic list of grouping
#' @param label symbolic label column
#'
#' @noRd
#' @importFrom dplyr select group_by
#' @importFrom rlang !!!
apply_row_grp_lbl <- function(.data, element_row_grp_loc, group, label = NULL, ...){

  # check which group/label columns are available

  grps_avail <- eval_tidyselect_on_colvec(group, names(.data))

  if(length(grps_avail)==0 || is_empty(label) || element_row_grp_loc$location %in% c("gtdefault", "noprint")){
    add_ln_df <- .data
  } else{
      #  combine any grouping columns that need combining into label
      add_ln_df <- .data %>% combine_group_cols(as_vars(grps_avail),
                                                label,
                                                element_row_grp_loc)

  }

  add_ln_df
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
    mutate(across(c(!!!group), ~str_trim(.x))) %>%
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
#' @noRd
apply_grp_block <- function(.data, group, element_block, widths){

  if (!is.null(element_block$post_space)){

    # create add-on row
    # utilize TEMP_row to retain the ordering
    grp_row_add <- .data %>%
      slice(n()) %>%
      mutate(across(c(-map_chr(group, as_name), -vars_select_helpers$where(is.numeric)),
                    ~replace(.x, value = fill_post_space(post_space = element_block$post_space,
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

  ## if only white space, no need to make wider for visuals
  if(grepl("^\\s*$", post_space)){
    return(" ")
  }

  length_post_space <- nchar(post_space)
  reps <- ceiling(width/length_post_space)
  fill_val <- strrep(post_space, reps) %>% str_sub(1, width)

  return(fill_val)

}


#' Combine group cols into the Labels
#'
#' @param .data Pre-processed data that just needs columns combining
#' @param group list of the group parameters
#' @param label label symbol should only be one
#' @param element_row_grp_loc row group location element. If null then will just indent
#'
#' @return dataset with the group columns combines
#' @noRd
#' @importFrom dplyr group_by group_split mutate select distinct bind_rows across last any_of slice
#' @importFrom tidyr replace_na
#' @importFrom stringr str_trim
#' @importFrom purrr map_dfr map_chr
#' @importFrom tidyselect vars_select_helpers
#' @importFrom forcats fct_inorder
#' @importFrom tibble add_row
combine_group_cols <- function(.data, group, label, element_row_grp_loc = NULL){

  orig_group_names <- map_chr(group, as_name)
  top_grouping <- group #used for spliting in case of spanning label

  # to retain the order of the data when splitting by group
  .data <- .data %>%
    mutate(across(c(!!!group), ~fct_inorder(.x)),
           ..tfrmt_row_grp_lbl = FALSE)

  # ensure label is character
  .data <- .data %>%
    mutate(across(!!label, ~as.character(.x)))

  if(is.null(element_row_grp_loc)){
    indent = "  "
  } else if(element_row_grp_loc$location %in% c("spanning", "column") & length(group) > 0){
    group = group[-1]
    indent = element_row_grp_loc$indent
  } else {
    indent = element_row_grp_loc$indent
  }

  while(length(group) > 0 & !is.null(label)){

    split_dat <- .data %>%
      group_by(!!!top_grouping) %>%
      group_split()

    .data<- split_dat %>%
      map_dfr(function(lone_dat){

        lone_dat_summ <- lone_dat %>%
          mutate(..tfrmt_summary_row = str_trim(!!label, side = "left") == str_trim(!!last(group), side = "left"))

        if (any(lone_dat_summ$..tfrmt_summary_row)==FALSE){

          # if the set of rows contains NO group-level summary data, create an
          # extra row to be added

          # first containing grouping/label values
          new_row <- lone_dat %>%
            select(!!!top_grouping, !!label) %>%
            mutate(!!label := !!last(group)) %>%
            distinct()

          # next all of the other variables (as missing)
          new_row <- lone_dat %>%
            select(-c(any_of(names(new_row)))) %>%
            slice(0) %>%
            add_row()  %>%
            mutate(across(vars_select_helpers$where(is.list), ~map(.x, ~if (is.null(.)) NA_character_ else .)))  %>%  #convert NULL to NA in list-cols
            bind_cols(new_row, .)%>%
            mutate(..tfrmt_row_grp_lbl = TRUE)

        } else {
          new_row <- tibble()
        }

        lone_dat_summ  %>%
          # only indent if not a summary row
          mutate(!!label := ifelse(.data$..tfrmt_summary_row==TRUE,
                                   !!label,
                                   str_c(indent, !!label))) %>%
          select(-"..tfrmt_summary_row") %>%
          bind_rows(new_row, .)
      })
    group = group[-length(group)]
    top_grouping = top_grouping[-length(top_grouping)]
  }

  .data%>%
    mutate(across(any_of(orig_group_names), ~as.character(.x)))


}

#' Remove row groups based on element_row_grp_loc and grouping
#'
#' @param .data data
#' @param element_row_grp_loc element object specifying row group label location
#' @param group symbolic list of grouping
#' @param label symbolic label column
#'
#' @noRd
remove_grp_cols <- function(.data, element_row_grp_loc, group, label = NULL){

  # check which group/label columns are available
  grps_avail <- eval_tidyselect_on_colvec(group, names(.data))

  if(length(grps_avail)==0 || element_row_grp_loc$location=="gtdefault"){
    add_ln_df <- .data
  } else{

    group <- as_vars(grps_avail)

    # Either drop group columns ("no print"), or format them w/ label

    if (element_row_grp_loc$location=="noprint"){

      add_ln_df <- .data %>% select(-c(!!!group))

    } else if(element_row_grp_loc$location == "indented"){
      add_ln_df <- .data %>%
        select(-c(!!!group))
    } else if(length(group) == 1){ #Using the grouping in gt + a single grouping
      add_ln_df <- .data %>%
        group_by(!!group[[1]])
    } else { # Using the grouping in gt, but needs to drop all groups in label
      add_ln_df <- .data %>%
        group_by(!!group[[1]]) %>%
        select(-c(!!!group[-1]))
    }
  }
  add_ln_df
}
