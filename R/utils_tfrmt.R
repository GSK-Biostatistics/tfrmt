#' Title
#'
#' @param .data Data to apply the tfrmt to
#' @param tfrmt tfrmt object to apply to the data
#'
#' @return formatted tibble
#' @noRd
apply_tfrmt <- function(.data, tfrmt_spec){

  apply_table_frmt_plan(
    .data = .data,
    table_frmt_plan = tfrmt_spec$body_style,
    group = tfrmt_spec$group,
    label = tfrmt_spec$label,
    param = tfrmt_spec$param,
    values = tfrmt_spec$values,
    column = tfrmt_spec$column
    ) %>%
    pivot_wider(names_from = !!tfrmt_spec$column,
                values_from = !!tfrmt_spec$values) %>%
    tentative_process(arrange, tfrmt_spec$sorting_cols) %>%
    tentative_process(select, tfrmt_spec$col_select)
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

