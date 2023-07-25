#' Apply column style plan - alignment - non-value cols
#'
#' @param .data data
#' @param tfrmt_obj tfrmt object
#' @param col_plan_vars the planned renaming of columns
#' @importFrom dplyr mutate across select tibble group_by slice n filter cur_column pull ungroup
#' @importFrom tidyr unnest
#' @importFrom tidyselect everything
#' @importFrom purrr map map_dfr discard
#' @importFrom rlang as_name
#' @importFrom tibble as_tibble_row
#' @importFrom forcats fct_inorder
#'
#' @noRd
apply_col_style_plan <- function(.data, tfrmt_obj, col_plan_vars = as_vars.character(colnames(.data))){

  style_plan <- tfrmt_obj$col_style_plan

  if(is.null(style_plan) || length(style_plan) == 0){
    return(.data)
  }

  if(is_empty(tfrmt_obj$column)){
    # create placeholder
    column_names <- "col"
  }else{
    column_names <- map_chr(tfrmt_obj$column, as_label)
  }

  total_col_style_selection <- list()

  for(el_idx in seq_along(style_plan)){

    style_el <- style_plan[[el_idx]]

    col_selections <- list()

    for( sel_id in seq_along(style_el$cols)){

      selection <- style_el$cols[sel_id]

      col_selection <- col_style_selections(selection, column_names, col_plan_vars)

      col_selection <- setdiff(col_selection, "..tfrmt_row_grp_lbl")

      col_selections <- c(col_selections, col_selection)
    }

    if(length(col_selections) > 0){
      col_style_selection <- style_el[setdiff(names(style_el),"cols")] %>%
        map(list) %>%
        as_tibble() %>%
        bind_cols(
          tibble( col = unlist(col_selections) )
        )
      total_col_style_selection <- c(
        total_col_style_selection,
        list(col_style_selection)
      )
    }
  }

  if(length(total_col_style_selection) > 0){

    total_col_styles <- bind_rows(total_col_style_selection) %>%
      group_by(col) %>%
      slice(n()) %>%
      ungroup()

    for(col_style_idx in seq_len(nrow(total_col_styles))){

      col_style_to_apply <- total_col_styles %>%
        slice(col_style_idx) %>%
        as.list()

      col_to_modify <- col_style_to_apply$col %>% char_as_quo()

      .data <-  .data %>%
        mutate(!!col_to_modify := apply_style_to_col(!!col_to_modify, col_style_to_apply[setdiff(names(col_style_to_apply),"col")]))
    }

    force(.data)
  }

  force(.data)

  return(.data)

}

# function to get all columns the col_style_structure applies to
col_style_selections <- function(selection, column_names, col_plan_vars){
  if(!is_span_structure(selection[[1]])){
    col_selection <- col_plan_quo_to_vars(
      x = selection,
      column_names = column_names,
      data_names = c(),
      preselected_cols = col_plan_vars %>% map_chr(as_label),
      return_only_selected = TRUE,
      default_everything_behavior = TRUE
    )
  }else{
    col_selection <- col_plan_span_structure_to_vars(
      x = selection,
      column_names = column_names,
      data_names = c(),
      preselected_cols = col_plan_vars %>% map_chr(as_label),
      return_only_selected = TRUE
    )
  }

  col_selection <- col_selection[!grepl("^-",col_selection)]

  ## use names if they exist, else use content
  if(!is.null(names(col_selection))){
    col_sel_names <- names(col_selection)
    if(any(col_sel_names == "")){
      col_sel_names[col_sel_names == ""] <- col_selection[col_sel_names == ""]
    }
    col_selection <- col_sel_names
  }

  col_selection
}

#' Column level styling to vector defined in styling list
#'
#' @param x vector to be styled
#' @param styling_list list of styles to apply
#'
#' @noRd

apply_style_to_col <- function(x, styling_list){

  x %>%
    tentative_process(
      apply_col_alignment,
      styling_list$align[[1]],
      styling_list$type
    ) %>%
    tentative_process(
      apply_col_width,
      styling_list$width[[1]]
    )

}


#' Apply alignment to a column
#'
#' @param col Character vector of data values to align
#' @param align Alignment to be applied to column. Defaults to `left` alignment.
#' @param type Type of alignment: "char" or "pos", for character alignment (default), and positional-alignment, respectively. Positional alignment allows for aligning over multiple positions in the column.
#' @param column_name name alignment being applied to. Defaults to NULL
#'
#' @return Character vector containing aligned data values
#'
#' @noRd
apply_col_alignment <- function(col, align, type = "char"){

  col_na_idx <- which(is.na(col))
  if(length(col_na_idx) > 0){
    col[col_na_idx] <- ""
  }

  if (type=="char"){
    out <- apply_col_alignment_char(col, align)
  } else {
    out <- apply_col_alignment_pos(col, align)
  }

  if(length(col_na_idx) > 0){
    out[col_na_idx] <- NA
  }

  out

}

#' Apply alignment to a column - character alignment
#'
#' @param col Character vector of data values to align
#' @param align Alignment to be applied to column.
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across tibble bind_cols
#' @importFrom stringr str_dup str_c str_trim str_detect
#' @importFrom rlang warn
#'
#' @noRd
apply_col_alignment_char <- function(col, align){

  if (!all(align %in% c("left","right"))){
    align <- ifelse(str_detect(align, "[[:alnum:]]"), paste0("\"", align, "\""), paste0("\\", align))
    align <- paste(align, collapse = "|")
    align <- paste0("(?=[", align, "])")
    tbl_dat <-  tibble(col = trimws(col)) %>%
      separate(col, c("add_left","add_right"), sep = align, extra = "merge", fill = "right", remove = FALSE) %>%
      mutate(across(c("add_left", "add_right"), function(x) {
        replace_na(x, "") %>%
          nchar() %>%
          {max(.)-.} %>%
          {str_dup(" ", .)}
      }))
  } else {

    tbl_dat <-  tibble(col = str_trim(col, side = "right")) %>%
      mutate(string_col = nchar(.data$col),
             string_tot = max(.data$string_col),
             space_to_add = str_dup(" ", .data$string_tot-.data$string_col))
    if(align == "left"){
      tbl_dat <- tibble(add_left = "",
                        add_right = tbl_dat$space_to_add) %>%
        bind_cols(tbl_dat, .)
    } else {
      tbl_dat <- tibble(add_left = tbl_dat$space_to_add,
                        add_right = "") %>%
        bind_cols(tbl_dat, .)
    }

  }

  str_c(
    tbl_dat$add_left,
    tbl_dat$col,
    tbl_dat$add_right)
}

#' Apply alignment to a column - positional alignment
#'
#' @param col Character vector of data values to align
#' @param align Alignment to be applied to column.
#'
#' @importFrom dplyr lag left_join tibble mutate group_by summarise arrange row_number select filter
#' @importFrom tidyr separate pivot_longer replace_na
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_replace_all str_count str_dup str_extract str_detect str_c
#'
#' @noRd
apply_col_alignment_pos <- function(col, align){

  # merge the alignment vec in with the column
  col_with_align <- tibble(col = trimws(col),
                           col_as_x = str_replace_all(col, "[0-9]", "x")) %>% # convert column values to x's
    left_join(
      tibble(align = trimws(align),
             col_as_x = str_replace_all(align, "\\|", "")),
      by = "col_as_x"
    )

  if (any(is.na(col_with_align$align))){
    message("`align` input for `type`=\"pos\" in col_style_structure does not cover all possible values. Some cells may not be aligned.")
  }

  # if nothing to align on, just return the original vec
  if (all(is.na(col_with_align$align))){
    return(col)
  }

  # account for any with missing align strings
  col_with_align <- col_with_align %>%
    mutate(align = ifelse(is.na(.data$align), .data$col_as_x, .data$align)) %>%
    select(-"col_as_x") %>%
    mutate(n_split_levs = str_count(.data$align, "(?<!\\\\)[\\|]")+1)

  # get the maximum number of splits to make
  # ignore any |'s that have been escaped by user
  n_split_levs_max <- max(col_with_align$n_split_levs, na.rm = TRUE)

  # create splits on the align vec at the |'s and count the number of chars for each
  # this will be used to help us split the col by position
  col_with_pos <- col_with_align %>%
    mutate(col_idx= row_number()) %>%
    separate("align", into = paste0("col_split_",1:n_split_levs_max), sep = "(?<!\\\\)[\\|]", remove = FALSE, fill = "right") %>%
    pivot_longer(starts_with("col_split_"), names_to = "col_split_lev", values_to = "col_split_val") %>%
    arrange(.data$col_idx, .data$col_split_lev) %>%
    group_by(.data$col_idx) %>%
    mutate(col_split_end = nchar(.data$col_split_val) %>% cumsum(),
           col_split_start = case_when(
             is.na(.data$col_split_val) ~ NA ,
             TRUE ~ lag(.data$col_split_end, default = 0)+1),
           col_split_lev = gsub("col_split_", "", .data$col_split_lev) %>% as.numeric())

  # 1) split the original col using the positions identified above
  #     * except on the final one, do not split as this will be right-padded
  #
  # 2) further split into 2 pieces, so padding can be added consistently:
  #  - if the first substring, padding will be added to the left of entire string
  #  - if > first substring, split will occur on 1st identified space (if available).
  #       padding will be added to the left of 2nd piece
  col_with_splits <- col_with_pos %>%
    mutate(col_sub = substr(.data$col, .data$col_split_start, .data$col_split_end)) %>%
    mutate(col_sub_1 = case_when(
      .data$col_split_lev == 1 ~ NA_character_, # first substring so do not split - will go to  col_sub_2
      .data$col_split_lev == .data$n_split_levs ~ col_sub, # last substring so do not split - will go to col_sub_1
      !str_detect(.data$col_sub, " ") & ! .data$col_split_lev == 1 ~ col_sub, # no space found - cannot split or pad
      TRUE ~ str_extract(.data$col_sub, "^.+?(?= )")), # extract string prior to first space
      col_sub_2 = case_when(
        .data$col_split_lev == 1 ~ .data$col_sub,   # first substring so put the whole thing here
        .data$col_split_lev == .data$n_split_levs ~ NA_character_, # last substring, nothing to left-pad
        TRUE ~ str_extract(.data$col_sub, "(?= ).*")))  # extract string following & including first space

  # within each split level, find the # of chars it needs to take up, then left pad
  col_left_padded00 <- col_with_splits %>%
    group_by(.data$col_split_lev) %>%
    mutate(to_add_left = nchar(.data$col_sub) %>%
             {max(., na.rm=TRUE)-.} %>%
             {str_dup(" ", .)})

  # notify user if left padding was intended but no space found
  col_left_padded01 <- col_left_padded00 %>%
    mutate(no_space = ifelse((.data$col_split_lev >1 & .data$col_split_lev < .data$n_split_levs) & # not the first or final level
                               (is.na(.data$col_sub_2))& # unable to split on a space
                               (! .data$col_sub=="") &    # there is actually a value there
                               (nchar(.data$to_add_left)>0),# there is postive padding
                             TRUE, FALSE),
           to_add_left = ifelse(.data$no_space, "", .data$to_add_left),
           across(c("col_sub_1", "to_add_left", "col_sub_2"), ~replace_na(., "")),
           col_sub_out = str_c(.data$col_sub_1, .data$to_add_left, .data$col_sub_2))

  if (nrow(filter(col_left_padded01, .data$no_space))>0){
    message("Unable to complete positional alignment in col_style_structure due to lack of whitespace available formatted value")
  }

  # collapse back to 1 rec per formatted string
  # & pad the right hand side
  col_left_padded_sum <- col_left_padded01 %>%
    group_by(.data$col_idx) %>%
    summarise(col = paste0(.data$col_sub_out, collapse = "")) %>%
    mutate(to_add_right = .data$col %>%
             nchar() %>%
             {max(.)-.} %>%
             {str_dup(" ", .)})

  str_c(col_left_padded_sum$col,
        col_left_padded_sum$to_add_right)

}


#' Set column widths to a specified number of characters
#'
#' @param col Character vector of data values to align
#' @param width Width of column, as number of characters
#'
#' @return Character vector containing padded aligned data values
#'
#' @importFrom stringr str_dup str_pad
#' @importFrom purrr pmap_chr
#' @noRd
apply_col_width <- function(col, width){

  col_na_idx <- which(is.na(col))
  col_empty_strings_idx <- which(grepl("^\\s+$",col))

  if(length(col_na_idx) > 0){
    col[col_na_idx] <- ""
  }

  pad_left <- str_dup(" ", nchar(col) - nchar(trimws(col, "left")))
  pad_right <- str_dup(" ", nchar(col) - nchar(trimws(col, "right")))
  out <- pmap_chr(list(trimws(col), width, pad_left, pad_right), wrap_string)

  if(length(col_na_idx) > 0){
    out[col_na_idx] <- NA
  }
  if(length(col_empty_strings_idx) > 0){
    out[col_empty_strings_idx] <- str_pad(col[col_empty_strings_idx], min(c(nchar(col[col_empty_strings_idx]), width)))
  }

  out
}

#' @importFrom stringi stri_wrap
wrap_string <- function(x, width, pad_left, pad_right){
  word_list <- stri_wrap(x, width = width, normalize = FALSE)
  paste0(pad_left, word_list, pad_right,collapse = "\n")
}



