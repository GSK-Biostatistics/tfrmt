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
      styling_list$align[[1]]
    ) %>%
    tentative_process(
      apply_col_width,
      styling_list$width[[1]]
    )

}


#' Apply alignment to a column
#'
#' @param col Character vector of data values to align
#' @param align Vector of one or more strings to align on. Strings should be 1
#'   character in length and not alphanumeric (i.e. spaces or punctuation). If
#'   NULL, data values will be aligned on the first occurrence of a decimal
#'   place or space. If more than one character is provided, alignment will be
#'   based on the first occurrence of any of the characters. For alignment based
#'   on white space, leading white spaces will be ignored.
#' @param column_name name alignment being applied to. Defaults to NULL
#'
#' @return Character vector containing aligned data values
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across pull tibble
#' @importFrom stringr str_dup str_c str_trim
#' @importFrom rlang warn
#' @noRd
apply_col_alignment <- function(col, align){

  col_na_idx <- which(is.na(col))
  if(length(col_na_idx) > 0){
    col[col_na_idx] <- ""
  }


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

  out <- str_c(
    tbl_dat$add_left,
    tbl_dat$col,
    tbl_dat$add_right)

  if(length(col_na_idx) > 0){
    out[col_na_idx] <- NA
  }

  out

}

#' Apply alignment to a column
#'
#' @param col Character vector of data values to align
#' @param align Vector of one or more strings to align on. Strings should be 1
#'   character in length and not alphanumeric (i.e. spaces or punctuation). If
#'   NULL, data values will be aligned on the first occurrence of a decimal
#'   place or space. If more than one character is provided, alignment will be
#'   based on the first occurrence of any of the characters. For alignment based
#'   on white space, leading white spaces will be ignored.
#' @param column_name name alignment being applied to. Defaults to NULL
#'
#' @return Character vector containing aligned data values
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across pull tibble
#' @importFrom stringr str_dup str_c str_trim str_pad
#' @importFrom rlang warn
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



