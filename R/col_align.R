#' Apply alignment to a column
#'
#' @param col Character vector containing data values
#' @param align Vector of one or more characters to align on. If NULL, data values will be aligned on the first occurrence of a decimal place or space. If more than one
#' character is provided, alignment will be based on the first occurrence of any of the characters. For alignment based on white space, leading white spaces will be ignored.
#'
#' @return Character vector containing aligned data values
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across pull tibble
#' @importFrom stringr str_dup str_c str_trim
#' @noRd
apply_col_align <- function(col, align){

  if (!all(align %in% c("left","right"))){
    align <- paste(paste0("\\", align), collapse = "|")
    align <- paste0("(?=[", align, "])")

    tbl_dat <-  tibble(col = trimws(col)) %>%
      separate(col, c("add_left","add_right"), sep = align, extra = "merge", fill = "right", remove = FALSE) %>%
      mutate(across(c(.data$add_left, .data$add_right), function(x) {
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

    str_c(tbl_dat$add_left,
        tbl_dat$col,
        tbl_dat$add_right)

}


#' Apply column alignment plan
#'
#' @param .data data
#' @param align_plan col_align_plan object
#' @param column symbolic list of columns
#' @param value symbolic value column
#' @importFrom dplyr mutate across select tibble group_by slice n filter cur_column pull ungroup
#' @importFrom tidyr unnest
#' @importFrom tidyselect everything
#' @importFrom purrr safely list_modify map_dfr
#' @importFrom rlang as_name
#' @importFrom tibble as_tibble_row
#' @importFrom forcats fct_inorder
apply_col_align_plan <- function(.data, align_plan, column, value){

  last_col <- column[[length(column)]]

  cols <- .data %>%
    pull(!!last_col) %>%
    unique()
  dummy_dat <- rep(" ",length(cols))
  names(dummy_dat) <- cols
  dummy_dat <- dummy_dat %>%
    as_tibble_row()


  selections <- align_plan %>%
    map(function(x) list_modify(x, col_checked = safely(select)(dummy_dat, !!!x$col)))


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
    rename(!!last_col := .data$column)

  .data %>%
    left_join(align_spec, by = as_label(last_col)) %>%
    mutate(across(c(!!!column), fct_inorder)) %>%
    group_by(!!!column) %>%
    group_split() %>%
    map_dfr(function(x){
      if(!is.null(x$align[[1]])){
      x <-  x %>%
          mutate(!!value := apply_col_align(!!value, x$align[[1]]))
      }
      x
    }) %>%
    select(-.data$align) %>%
    mutate(across(c(!!!column), as.character))
}

