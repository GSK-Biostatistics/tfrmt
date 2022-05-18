#' Align a column on character
#'
#' @param col Character vector containing data values
#' @param align Vector of one or more characters to align on. If NULL, data values will be aligned on the first occurrence of a decimal place or space. If more than one
#' character is provided, alignment will be based on the first occurrence of any of the characters. For alignment based on white space, leading white spaces will be ignored.
#'
#' @return Character vector containing aligned data values
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across pull tibble
#' @importFrom stringr str_dup str_c
#' @noRd
col_align_char <- function(col, align){

  if (is.null(align)){
    align <- "."
  }

  align <- paste(paste0("\\", align), collapse = "|")

  align <- paste0("(?=[", align, "])")

  tibble(col = trimws(col)) %>%
    separate(col, c("string_left","string_right"), sep = align, extra = "merge", fill = "right", remove = FALSE) %>%
    mutate(across(c(.data$string_left, .data$string_right), ~replace_na(.x, "") %>% nchar)) %>%
    mutate(col_out = str_c(
                        str_dup(" ", max(.data$string_left)-.data$string_left),
                        col,
                        str_dup(" ", max(.data$string_right)-.data$string_right))) %>%
    pull(.data$col_out)

}

#' Left or right align a column
#'
#' @param col Character vector containing data values
#' @param align Side to align to, either left or right
#' @importFrom stringr str_dup str_c
#' @importFrom dplyr mutate pull tibble
#' @noRd
col_align_lr <- function(col, align){

  tbl_dat <- tibble(col = col) %>%
    mutate(string_col = nchar(col),
           string_tot = max(.data$string_col),
           space_to_add = str_dup(" ", .data$string_tot-.data$string_col)) %>%
    rowwise %>%
    mutate(add_left = ifelse(align=="left", "", .data$space_to_add),
           add_right = ifelse(align=="right", "", .data$space_to_add))

    tbl_dat %>%
      rowwise %>%
      mutate(col_out = str_c(.data$add_left,
                             .data$col,
                             .data$add_right)) %>%
      pull(.data$col_out)

}

#' Apply column alignment plan
#'
#' @param .data data
#' @param align_plan col_align_plan object
#' @importFrom dplyr mutate across select tibble group_by slice n filter cur_column pull ungroup
#' @importFrom tidyr unnest
#' @importFrom tidyselect everything
#' @importFrom purrr safely list_modify map_dfr
#' @importFrom rlang as_name
apply_col_align_plan <- function(.data, align_plan){

  selections <- align_plan %>%
    map(function(x) list_modify(x, col_checked = safely(select)(.data, !!!x$col)))


  map(selections, function(x){
    if(!is.null(x$col_checked$error)){
      stop(paste0("Variable Specified in element_align doesn't exist in the supplied dataset. Please check the tfrmt and try again."),
           call. = FALSE)
    }
  })


  # keep the last col align for each col
  align_spec <- selections %>%
    map_dfr(~tibble(align = list(.x$align), column = list(.x$col_checked$result %>% names))) %>%
    unnest(.data$column) %>%
    ungroup() %>%
    group_by(.data$column) %>%
    slice(n()) %>%
    mutate(column = as_name(.data$column))

  .data %>%
    mutate(across(align_spec$column, function(x){
      align <- filter(align_spec, .data$column == cur_column()) %>% pull(align) %>% unlist
      if(length(align)>0){
        apply_col_align(x, align)
      } else {
        x
      }
    }))
}

#' Apply alignment to a column
#'
#' @param col Vector to align
#' @param align  Alignment to be applied to column
#'
#' @return
#' @noRd
apply_col_align <- function(col, align){

  if (all(align %in% c("left","right"))){
    col_align_lr(col, align)
  } else{
    col_align_char(col, align)
  }

 }
