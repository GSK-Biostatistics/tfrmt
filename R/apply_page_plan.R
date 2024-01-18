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
#' @importFrom tibble is_tibble
#' @importFrom purrr list_flatten
apply_page_plan <- function(.data, page_plan, group, label, row_grp_plan_label_loc = "indented"){

  # first apply page structures
  if (!is_empty(page_plan$struct_list)){
    .data <- apply_page_struct(.data, page_plan$struct_list, group, label, page_plan$note_loc)
  }

  # then apply max rows splits
  if (!is.null(page_plan$max_rows)){

    if (is_tibble(.data)){
      .data <- apply_page_max_rows(.data, page_plan$max_rows, group, label, row_grp_plan_label_loc)
    } else {
      .data <- structure(
        map(.data, ~ apply_page_max_rows(.x, page_plan$max_rows, group, label, row_grp_plan_label_loc)) %>%
        list_flatten(),
        .page_grp_vars = attr(.data, ".page_grp_vars")
      )
    }

  }

  .data
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
  n_grp_rows <- switch(row_grp_plan_label_loc,
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
      tbl_list[[i]] <- structure(cur_dat %>% select(-"TEMP_row"),
                                 .page_note = attr(.data, ".page_note"))
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
#' @importFrom purrr map map2 map_dbl
#' @importFrom dplyr tibble row_number mutate group_by left_join select filter summarise  lag last
#' @importFrom tidyr  pivot_longer
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

  # 2. get all the subsets of data

  # a) If applicable, split by any group/label vars set to ".default"
  struct_defaults_idx <- which(map_lgl(page_struct_list, detect_default)) # do this again post-dropping duplicates

  if (length(struct_defaults_idx)>0){

    # split on all set to .default
    grping <- expr_to_grouping(page_struct_list[[struct_defaults_idx]], group, label)

    dat_split_1 <- .data %>%
      nest(`..tfrmt_data` = everything(), .by = (all_of(grping))) %>%
      mutate(`..tfrmt_split_num` = row_number())

  } else {
    # no default - just nest to get in same structure for next step
    dat_split_1 <- tibble(`..tfrmt_data` = list(!!.data))
  }

  # b) Further split the sub-data based on specific values (loop over all combinations of page_structure & data)

  # find indices of specific values in data
  dat_split_2_idx <- dat_split_1 %>%
    mutate(split_idx = map(.data$`..tfrmt_data`, function(x){
      map(page_struct_list, function(y){
        struct_val_idx(y, x, group, label) %>% # returns all indices in the block of data
          map_dbl(last) # keep just the last one to split after
      }) %>% unlist()
    }))

  # determine where the splits should occur in data
  dat_split_2 <- dat_split_2_idx%>%
    mutate(`..tfrmt_data` = map2(.data$`..tfrmt_data`, .data$split_idx, function(x, y){

      x %>%
        mutate(`..tfrmt_split_idx` = .data$TEMP_row %in% y,
               # carry it forward to denote start of next table,
               `..tfrmt_start_idx` = lag(.data$`..tfrmt_split_idx`, default = TRUE),
               `..tfrmt_split_after` = cumsum(.data$`..tfrmt_start_idx`)) %>%
        select(- c("..tfrmt_start_idx","..tfrmt_split_idx")) %>%
        group_by(.data$`..tfrmt_split_after`) %>%
        group_split(.keep = FALSE)
    })) %>%
    select(-"split_idx") %>%
    unnest(cols = "..tfrmt_data")

  # 3. create the page_notes as applicable
  if ("..tfrmt_split_num" %in% names(dat_split_2)){
    # create the page_notes to be carried forward as names for now
    tbl_nms <- dat_split_2 %>%
      select(-"..tfrmt_data") %>%
      pivot_longer(cols = -"..tfrmt_split_num", names_to = "grouping_col", values_to = "grouping_val") %>%
      group_by(.data$`..tfrmt_split_num`) %>%
      filter(!is.na(.data$grouping_val)) %>%
      unique() %>%
      summarise(`..tfrmt_page_note` = paste0(.data$grouping_col, ": ", .data$grouping_val) %>% paste0(collapse = ",\n"))

    page_grp_vars <- setdiff(names(dat_split_2), c("..tfrmt_data", "..tfrmt_split_num"))

    dat_split_2 <- left_join(dat_split_2, tbl_nms, by = "..tfrmt_split_num") %>%
      select(c("..tfrmt_data", "..tfrmt_page_note"))
  } else {
    page_grp_vars <- NULL
  }


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


  if(element_row_grp_loc %in% c("spanning", "column") & length(group) > 0){
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

