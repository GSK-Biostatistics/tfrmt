apply_tfmt <- function(.data, tfmt){

  apply_all_fmts(.data, tfmt$body_style, group = tfmt$group, label = tfmt$label, param = tfmt$param, tfmt$values) %>%
    pivot_wider(names_from = !!tfmt$column,
                values_from = !!tfmt$values) %>%
    tentative_process(arrange, tfmt$sorting_cols) %>%
    tentative_process(arrange, tfmt$col_select)
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
