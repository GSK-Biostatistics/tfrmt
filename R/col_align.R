#' Align a column on character
#'
#' @param col Character vector containing data values
#' @param char Character to align on. Will be used in regular expressions. If NULL, values will be aligned on first decimal place or space.
#'
#' @return Character vector containing aligned data values
#'
#' @examples
#'
#' col <- c("xx.xx","  x", " xx (xx, xx)", "  x (xx.x, xx.x)", "xx, xx")
#' col_align_char(col, char = "\\.|,")
#'
col_align_char <- function(col, char = NULL){

  if (is.null(char)){
    char <- "\\."
  }
  char <- paste0("(?=[", char, "|\\s])") # center on first decimal or space (if no dec)

  tibble(col = trimws(col)) %>%
    separate(col, c("string_left","string_right"), sep = char, extra = "merge", fill = "right", remove = FALSE) %>%
    mutate(across(c(string_left, string_right), ~replace_na(.x, "") %>% nchar)) %>%
    mutate(col_out = str_c(
                        str_dup(" ", max(string_left)-string_left),
                        col,
                        str_dup(" ", max(string_right)-string_right))) %>%
    pull(col_out)

}

#' Left or right align a column
#'
#' @inheritParams col_align_char
#'
col_align_lr <- function(col, side){

  tbl_dat <- tibble(col = trimws(col)) %>%
    mutate(string_col = nchar(col),
           string_tot = max(string_col),
           space_to_add = str_dup(" ", string_tot-string_col))

  if(side=="left"){

    tbl_dat %>%
      mutate(col_out = str_c(col,
                             space_to_add)) %>%
      pull(col_out)

  } else if(side=="right"){

    tbl_dat %>%
      mutate(col_out = str_c(space_to_add,
                             col)) %>%
      pull(col_out)

  }
}

#' Align value columns based on element_align specs
#'
#' @param .data data
#' @param col_align element_align object
#'
col_align_all <- function(.data, col_align){

  .data %>%
    mutate(across(col_align$dec_pl %>% map(quo_name) %>% unlist, ~col_align_char(.x, char = "\\.|,")),
           across(col_align$left  %>% map(quo_name) %>% unlist, ~col_align_lr(.x, side = "left")),
           across(col_align$right %>% map(quo_name) %>% unlist, ~col_align_lr(.x, side = "right")))

}
