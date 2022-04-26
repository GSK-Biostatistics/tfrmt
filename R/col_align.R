#' Align a column on character
#'
#' @param col Character vector containing data values
#' @param char_val Character to align on. If NULL, values will be aligned on first decimal place or space.
#'
#' @return Character vector containing aligned data values
#'
#' @examples
#'
#' col <- c("xx.xx","  x", " xx (xx, xx)", "  x (xx.x, xx.x)", "xx, xx")
#' tlang:::col_align_char(col, char_val = c(".", " ", ","))
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across pull
#'
col_align_char <- function(col, char_val){

  if (is.null(char_val)){
    char_val <- "."
  }

  char_val <- paste(paste0("\\", char_val), collapse = "|")

  char_val <- paste0("(?=[", char_val, "])")

  tibble(col = trimws(col)) %>%
    separate(col, c("string_left","string_right"), sep = char_val, extra = "merge", fill = "right", remove = FALSE) %>%
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
#' @param side Side to align to, either left or right
#' @importFrom stringr str_dup str_c
#' @importFrom dplyr mutate pull
col_align_lr <- function(col, side = c("left", "right")){

  tbl_dat <- tibble(col = col) %>%
    mutate(string_col = nchar(col),
           string_tot = max(.data$string_col),
           space_to_add = str_dup(" ", .data$string_tot-.data$string_col))

  if(side=="left"){

    tbl_dat %>%
      mutate(col_out = str_c(.data$col,
                             .data$space_to_add)) %>%
      pull(.data$col_out)

  } else if(side=="right"){

    tbl_dat %>%
      mutate(col_out = str_c(.data$space_to_add,
                             .data$col)) %>%
      pull(.data$col_out)

  }
}

#' Align value columns based on element_align specs
#'
#' @param .data data
#' @param col_align element_align object
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate across
col_align_all <- function(.data, col_align){

  .data %>%
    mutate(across(col_align$char %>% map_chr(quo_name), ~col_align_char(.x, char_val = col_align$char_val)),
           across(col_align$left  %>% map_chr(quo_name), ~col_align_lr(.x, side = "left")),
           across(col_align$right %>% map_chr(quo_name), ~col_align_lr(.x, side = "right")))

}
