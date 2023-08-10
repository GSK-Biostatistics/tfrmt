#' Apply page plan to data
#'
#' @param .data data
#' @param page_plan tfrmt page_plan
#' @param group symbolic list of grouping
#' @param label symbolic label column
#' @param row_grp_plan_label_loc row_grp_plan label location
#'
#' @noRd
#' @importFrom rlang is_empty
apply_page_plan <- function(.data, page_plan, group, label, row_grp_plan_label_loc = "indented"){

  # preferentially apply any page_structures;
  # if none, then apply max_rows
  if (!is_empty(page_plan$struct_list)){
    apply_page_struct(.data, page_plan$struct_list, group, label, page_plan$note_loc)
  } else if (!is.null(page_plan$max_rows)){
    apply_page_max_rows(.data, page_plan$max_rows, group, label, row_grp_plan_label_loc)
  } else {
    .data
  }
}

#' Apply page plan splits based on row #s
#'
#' @param .data data
#' @param max_rows max number of rows per page / # of rows to split after
#' @param row_grp_plan_label_loc row_grp_plan label location
#'
#' @noRd
#' @importFrom dplyr  slice bind_rows filter pull
#' @importFrom tidyselect contains
#' @importFrom tibble tibble
#' @importFrom forcats fct_inorder
apply_page_max_rows <- function(.data, max_rows, group, label, row_grp_plan_label_loc){

  .data <- .data %>%
    mutate(TEMP_row = row_number())

  # determine # of rows to be added for the group during row grp lbl formatting
  # only proceed if the # of group rows to be added < max_rows
  #
  #      - "gtdefault": +1
  #      - "spanning" / "indented" : # of grouping vars
  #      - "column": # of grouping vars - 1
  #      - "noprint": +0
  n_grp_vars <- length(group)
  n_grp_rows <- switch(row_grp_plan_label_loc$location,
                       gtdefault = 1,
                       spanning = n_grp_vars,
                       indented = n_grp_vars,
                       column = n_grp_vars-1,
                       noprint = 0)

  if (!n_grp_rows<max_rows){
    message("Unable to complete pagination because `max_rows` specified in `page_plan` is smaller than the number of rows dedicated to group labels. Suggest increasing `max_rows` and trying again.")
    return(.data)
  }

  # 2) split the data by row number, accounting for row groups to be added

  # start with top row, then add rows 1 by 1, testing # of rows each time
  remain_dat <- .data
  cur_dat <- tibble()
  all_summ_row <- numeric(0)
  tbl_list <- vector(mode = "list")
  i <- 1

  while(nrow(remain_dat)>0){

    # candidate row to consider adding to tbl
    next_dat <- remain_dat %>% slice(1)

    # add the previous summary row from previous table if needed
    # (only applies to the start of new page)
    if(length(all_summ_row)>0){

      # get the data from the summary row
      prev_summ_row <- .data[all_summ_row,]

      next_dat <- next_dat %>%
        add_summary_rows(prev_summ_row, group, label)

      all_summ_row <- numeric(0) # reset until current tbl is finished
    }

    # if this row is added, how many rows will be in table
    cur_dat_new <- bind_rows(cur_dat, next_dat) %>%
      combine_group_cols_mod(group, label, row_grp_plan_label_loc)


     # if the table is within our limit, keep it
    if (nrow(cur_dat_new) <= max_rows){
      cur_dat <- bind_rows(cur_dat, next_dat)
      remain_dat <- remain_dat %>% slice(-1)
    }

    # if we have hit or exceeded the limit, save the table & move to next
    if (nrow(cur_dat_new) >= max_rows | nrow(remain_dat)==0){

      # summary row groups to carry forward to next tbl
      if ("..tfrmt_summary_row" %in% names(cur_dat_new)){
        all_summ_row <- cur_dat_new %>%
          filter(.data$`..tfrmt_summary_row`) %>%
          pull(.data$TEMP_row)
      }

      # save tbl to list
      tbl_list[[i]] <- cur_dat
      i <- i + 1
      cur_dat <- tibble()
    }
  }

  # return single tbl or list of tbls
  if (length(tbl_list)==1) {
    tbl_list[[1]]
  } else {
    tbl_list
  }

}

#' Apply page plan splits based on page_structures
#'
#' @param .data data
#' @param page_struct_list list of page structure objects
#' @param group symbolic list of grouping
#' @param label symbolic label column
#' @param note_loc location of page note
#'
#' @noRd
#' @importFrom purrr map map_dbl accumulate
#' @importFrom dplyr tibble row_number mutate group_by slice arrange left_join select desc  filter pull summarise last lag
#' @importFrom tidyr unnest nest pivot_longer drop_na
#' @importFrom tidyselect starts_with all_of
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

  # 2. If applicable, split by any group/label vars set to ".default"
  struct_defaults_idx <- which(map_lgl(page_struct_list, detect_default))

  if (length(struct_defaults_idx)>0){

    # split on all set to .default
    grping <- expr_to_grouping(page_struct_list[[struct_defaults_idx]], group, label)

    dat_split_1 <- .data %>%
      nest(`..tfrmt_data` = everything(), .by = (all_of(grping))) %>%
      mutate(`..tfrmt_split_num` = row_number())

    # create the page_notes to be carried forward as names for now
    tbl_nms <- dat_split_1 %>%
      select(-"..tfrmt_data") %>%
      pivot_longer(cols = -"..tfrmt_split_num", names_to = "grouping_col", values_to = "grouping_val") %>%
      group_by(.data$`..tfrmt_split_num`) %>%
      filter(!is.na(.data$grouping_val)) %>%
      summarise(`..tfrmt_page_note` = paste0(.data$grouping_col, ": ", .data$grouping_val) %>% paste0(collapse = ",\n"))

    dat_split_1 <- left_join(dat_split_1, tbl_nms, by = "..tfrmt_split_num") %>%
      select(c("..tfrmt_data", "..tfrmt_page_note"))

    page_grp_vars <- grping # save for later

  } else {
    # no default - just nest to get in same structure for next step
    dat_split_1 <- tibble(`..tfrmt_data` = list(!!.data))

    page_grp_vars <- NULL
  }

  # 3. Further split the sub-data based on specific values (loop over all combinations of page_structure & data)

  # find indices of specific values in data
  dat_split_2_idx <- dat_split_1 %>%
    mutate(split_idx = map(.data$`..tfrmt_data`, function(x){
      map_dbl(page_struct_list, function(y){
        page_test_data(y, x, group, label)
      })
    }))

  # determine where the splits should occur in data
  dat_split_2 <- dat_split_2_idx%>%
    mutate(`..tfrmt_data` = map2(.data$`..tfrmt_data`, .data$split_idx, function(x, y){

      x %>%
        mutate(`..tfrmt_split_idx` = .data$TEMP_row %in% y,
               # get the column to represent everything leading up to the split
               `..tfrmt_split_idx` = ifelse(!.data$`..tfrmt_split_idx` &
                                              !lag(.data$`..tfrmt_split_idx`, default = FALSE),
                                            NA_real_, # set to NA if within a tbl
                                            .data$`..tfrmt_split_idx`), # otherwise keep as is
               # cumulative sum to capture each tbl chunk
               `..tfrmt_split_after` = accumulate(.data$`..tfrmt_split_idx`, function(acc, x){
                                                               if (is.na(acc)){ # first row
                                                                 1
                                                               } else if (is.na(x)){ # same idx
                                                                 acc
                                                               } else {
                                                                 acc + 1  # new tbl idx
                                                               }
                                                             })) %>%
        fill("..tfrmt_split_after", .direction = "up") %>%
        select(- "..tfrmt_split_idx") %>%
        group_by(.data$`..tfrmt_split_after`) %>%
        group_split(.keep = FALSE)
    })) %>%
    select(-"split_idx") %>%
    unnest(cols = "..tfrmt_data")

  # 4. return the values

  # prep list of tbsl
  dat_out <- dat_split_2 %>%
        mutate(`..tfrmt_data` = map(.data$`..tfrmt_data`, ~select(.x, - "TEMP_row"))) %>%
        pull(.data$`..tfrmt_data`)

  # add pg_note to individual tbls as applicable
  if ("..tfrmt_page_note" %in% names(dat_split_2)){
    pg_note <- dat_split_2$`..tfrmt_page_note`
    dat_out <-  dat_out %>%
      map2(., pg_note, ~ structure(.x,
                                   .page_note = .y))

  }

  structure(
    dat_out,
    .page_grp_vars = page_grp_vars
  )

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

  grouping %>% unname()
}


#' adapted from the row_grp_plan to remove anything unnecessary but keep the logic
#' @noRd
#' @importFrom forcats fct_inorder
#' @importFrom dplyr mutate select across group_by group_split distinct last bind_rows
#' @importFrom tibble tibble
#' @importFrom stringr str_trim
#' @importFrom tidyselect any_of
combine_group_cols_mod <- function(.data, group, label, element_row_grp_loc = NULL){

  top_grouping <- group #used for splitting in case of spanning label

  # to retain the order of the data when splitting by group
  .data <- .data %>%
    select(c(!!!group, !!label, "TEMP_row")) %>%
    mutate(across(c(!!!group), ~fct_inorder(.x)),
           ..tfrmt_row_grp_lbl = FALSE)%>%
    mutate(`..tfrmt_summary_row` = str_trim(!!label, side = "left") == str_trim(!!last(group), side = "left"))


  if(element_row_grp_loc$location %in% c("spanning", "column") & length(group) > 0){
    group = group[-1]
  }

  while(length(group) > 0 & !is.null(label)){

    split_dat <- .data %>%
      group_by(!!!top_grouping) %>%
      group_split()

    .data<- split_dat %>%
      map_dfr(function(lone_dat){

        lone_dat_summ <- lone_dat %>%
          mutate(`..tfrmt_summary_row_cur` = str_trim(!!label, side = "left") == str_trim(!!last(group), side = "left"))

        if (any(lone_dat_summ$`..tfrmt_summary_row_cur`)==FALSE){

          # if the set of rows contains NO group-level summary data, create an extra row to be added
          new_row <- lone_dat %>%
            select(!!!top_grouping, !!label) %>%
            mutate(!!label := !!last(group)) %>%
            distinct()%>%
            mutate(..tfrmt_row_grp_lbl = TRUE)
        } else {
          new_row <- tibble()
        }

        lone_dat_summ  %>%
          bind_rows(new_row, .)
      })
    group = group[-length(group)]
    top_grouping = top_grouping[-length(top_grouping)]
  }

  .data %>%
    select(-any_of("..tfrmt_summary_row_cur"))
}

#' add any related summary rows from previous tbl to next tbl
#' @noRd
#' @importFrom dplyr mutate slice pull filter across select bind_rows
#' @importFrom purrr map_chr map map2_lgl
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
add_summary_rows <- function(next_dat, prev_summ, group, label){

  #get grouping values from the summary row
  prev_summ_top_grp <- prev_summ %>%
    mutate(across(c(!!!group), ~ .x==!!label)) %>%
    pivot_longer(map_chr(group, as_label), names_to = "..tfrmt_summ_grp_num", values_to = "..tfrmt_summ_row") %>%
    filter(.data$`..tfrmt_summ_row`) %>%
    group_by(.data$TEMP_row) %>%
    slice(1)  %>%
    mutate(`..tfrmt_summ_grp_num` = which(.data$`..tfrmt_summ_grp_num`==map_chr(group, as_label))) %>%
    pull(.data$`..tfrmt_summ_grp_num`)

  prev_summ_top_grp_vars <- map(seq_len(nrow(prev_summ)),
                                ~prev_summ[.x,] %>% select(c(!!!group)) %>% select(1:all_of(prev_summ_top_grp[.x])))

  # get the grouping values from the next row
  next_summ_top_grp_vars <- map(prev_summ_top_grp_vars,
                                ~next_dat %>% select(names(.x)))

  # check which are identical & add all that match to the data
  check_eq <- map2_lgl(prev_summ_top_grp_vars, next_summ_top_grp_vars, function(x,y){
    identical(x,y)
  })
  if(any(check_eq)){
    to_add <- which(check_eq)

    next_dat <- bind_rows(
      prev_summ[to_add,],
      next_dat)
  }
  next_dat
}

