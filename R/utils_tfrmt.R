#' Title
#'
#' @param .data Data to apply the tfrmt to
#' @param tfrmt tfrmt object to apply to the data
#' @param mock Logical value is this is for a mock or not. By default `FALSE`
#'
#' @return formatted tibble
#'
#' @importFrom purrr quietly
#' @importFrom tidyr pivot_wider unnest
#' @importFrom dplyr arrange select
#' @importFrom tidyselect eval_select
#'
#' @noRd
apply_tfrmt <- function(.data, tfrmt, mock = FALSE){

  validate_cols_match(.data, tfrmt, mock)

  tbl_dat <- apply_table_frmt_plan(
    .data = .data,
    table_frmt_plan = tfrmt$body_style,
    group = tfrmt$group,
    label = tfrmt$label,
    param = tfrmt$param,
    values = tfrmt$values,
    column = tfrmt$column,
    mock = mock
  )

  if(!is.null(tfrmt$col_plan)){
    tfrmt$column <- ammend_col_plan_column(tfrmt$column, tfrmt$col_plan)
    tbl_dat <- ammend_col_plan_spanners(tfrmt$column, tfrmt$col_plan)
  }

  tbl_dat_wide <- quietly(pivot_wider)(
    tbl_dat,
    names_from = names(eval_select(expr(c(!!!tfrmt$column)), tbl_dat)),
    names_sep = .tlang_delim,
    values_from = !!tfrmt$values
    ) %>%
    rename_with(
      function(x){gsub(paste0("NA",.tlang_delim),"",x)},
      .cols = starts_with(paste0("NA",.tlang_delim))
    )

  if (mock == TRUE &&
      length(tbl_dat_wide$warnings)>0 &&
      str_detect(tbl_dat_wide$warnings, paste0("Values from `", as_label(tfrmt$values), "` are not uniquely identified"))){
    message("Mock data contains more than 1 param per unique label value. Param values will appear in separate rows.")
    tbl_dat_wide <- tbl_dat_wide$result %>% unnest(cols = everything())
  } else {
    tbl_dat_wide <- tbl_dat_wide$result
  }

  tbl_dat_wide %>%
    tentative_process(arrange, tfrmt$sorting_cols) %>%
    tentative_process(select_col_plan, tfrmt$col_plan)%>% ## select the columns & rename per col_plan
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
  } else{
    exists_test <- .data %>%
      safely(select)(!!!param) %>%
      .[["error"]] %>%
      is.null()
    if(exists_test){
      out <- .data %>%
        fx(!!!param)
    } else {
      out <- .data
      message("Unable to complete formatting because COLNAME isn't in the dataset")
    }
  }
  out
}


#' Checks required columns exsist
#'
#' @param .data Data to apply the tfrmt to
#' @param tfrmt tfrmt object to apply to the data
#' @param mock Logical value is this is for a mock or not. By default `FALSE`
#'
#' @return formatted tibble
#' @noRd
#' @importFrom purrr map safely
#' @importFrom rlang !! !!!
#' @importFrom dplyr select
validate_cols_match <- function(.data, tfrmt, mock){

  #Required variables
  if(mock){
    req_quo <- c("label", "param")
  } else {
    req_quo <- c("label", "param", "values")
  }
  req_var <- c("group","column")

  .data <- .data %>% ungroup

  req_quo %>%
    map(function(x){
      var_test <- tfrmt[[x]]
      check <- safely(select)(.data, !!var_test)
      if(!is.null(check$error)){
        stop(paste0("Variable Specified in '", x, "' doesn't exist in the supplied dataset. Please check the tfrmt and try again."),
             call. = FALSE)
      }
    }
    )

  req_var %>%
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
