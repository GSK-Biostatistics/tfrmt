#' Big N Structure
#'
#' Big N strucutre allows you to specify which values should become the big n
#' values and how they should be formatted. Values are specified by providing
#' the row value(s) of the parameter column for which the values are big n's.
#' This will remove these from the body of the table and place them into columns
#' matching the values in the column column(s). The default formatting is `N = xx`,
#' on its own line, but that can be changed by providing a different `frmt()` to
#' `n_frmt`
#'
#' @param param_val row value(s) of the parameter column for which the values
#'   are big n's
#' @param n_frmt [frmt()] to control the formatting of the big n's
#'
#' @return
#' @export
#'
big_n_structure<- function(param_val, n_frmt = frmt("\nN = xx")){
  structure(
    list(
      param_val = param_val,
      n_frmt = n_frmt
    ),
    class = c("big_n_structure","structure")
  )
}



apply_big_n_structure <- function(.data, big_n_df){
  browser()
  to_rename <- big_n_df %>%
    mutate(new_col_part = paste0(col_part, val))

  new_nm <- names(.data)
  for(i in seq(nrow(to_rename))){
    new_nm <- str_replace(new_nm, to_rename$col_part[i],
                          to_rename$new_col_part[i])
  }
}

remove_big_ns <- function(.data, param, big_n_structure){
  if(!is.null(big_n_structure)){
    .data <- .data %>%
      filter(!(!!param) %in% big_n_structure$param_val)
  }
  .data
}

get_big_ns <-  function(.data, param, value, columns, big_n_structure, mock){
  if(!is.null(big_n_structure)){
    .data <- .data %>%
      filter((!!param) %in% big_n_structure$param_val) %>%
      apply_frmt.frmt(big_n_structure$n_frmt, ., value, mock) %>%
      select(!!!columns, !!value)
    # %>%
      # unite("col_part", !!!columns, sep = .tlang_delim, na.rm = TRUE)
  } else {
    .data <- NULL
  }
  .data
}
