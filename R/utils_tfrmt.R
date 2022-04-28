#' Title
#'
#' @param .data Data to apply the tfrmt to
#' @param tfrmt tfrmt object to apply to the data
#'
#' @return formatted tibble
#' @noRd
apply_tfrmt <- function(.data, tfrmt){
  validate_cols_match(.data, tfrmt)

  apply_table_frmt_plan(
    .data = .data,
    table_frmt_plan = tfrmt$body_style,
    group = tfrmt$group,
    label = tfrmt$label,
    param = tfrmt$param,
    values = tfrmt$values,
    column = tfrmt$column
    ) %>%
    pivot_wider(names_from = !!tfrmt$column,
                values_from = !!tfrmt$values) %>%
    tentative_process(arrange, tfrmt$sorting_cols) %>%
    tentative_process(select, tfrmt$col_select)%>%
    col_align_all(tfrmt$col_align)
}


#' Tentatively apply an element
#'
#' Will only apply the element to the data if the param isn't null
#'
#' @param .data data to process
#' @param fx processing function
#' @param param parameter to control processing
#'
#' @return processed data
#' @noRd
tentative_process <- function(.data, fx, param){
  if(is.null(param)){
    out <- .data
  } else {
    out <- .data %>%
      fx(!!!param)
  }
  out
}


#' Checks required columns exsist
#'
#' @param .data Data to apply the tfrmt to
#' @param tfrmt tfrmt object to apply to the data
#'
#' @return formatted tibble
#' @noRd
#' @importFrom purrr map safely
#' @importFrom rlang !! !!!
#' @importFrom dplyr select
validate_cols_match <- function(.data, tfrmt){
  #Required variables
  c("label", "param", "values", "column") %>%
    map(function(x){
      var_test <- tfrmt[[x]]
      check <- safely(select)(.data, !!var_test)
      if(!is.null(check$error)){
        stop(paste0("Variable Specified in '", x, "' doesn't exist in the supplied dataset. Please check the tfrmt and try again."),
             call. = FALSE)
      }
    }
    )

  c("group") %>%
    map(function(x){
      var_test <- tfrmt[[x]]
      check <- safely(select)(.data, !!!var_test)
      if(!is.null(check$error)){
        stop(paste0("Variable Specified in '", x, "' doesn't exist in the supplied dataset. Please check the tfrmt and try again."),
             call. = FALSE)
      }
    }
    )

}
