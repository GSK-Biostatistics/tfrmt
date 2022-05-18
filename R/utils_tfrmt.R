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

  if(length(tfrmt$column) > 1){
    amended_values <- amend_col_plan_and_column(tfrmt, tbl_dat)
    tbl_dat <- amended_values$tbl_dat
    tfrmt <- amended_values$tfrmt
  }

  tbl_dat_wide <- safely(pivot_wider)(
    tbl_dat,
    names_from = !!(tfrmt$column[[1]]),
    values_from = !!tfrmt$values,
    names_repair = "minimal"
    )

  if (mock == TRUE &&
      length(tbl_dat_wide$warnings)>0 &&
      str_detect(tbl_dat_wide$warnings, paste0("Values from `", as_label(tfrmt$values), "` are not uniquely identified"))){
    message("Mock data contains more than 1 param per unique label value. Param values will appear in separate rows.")
    tbl_dat_wide <- tbl_dat_wide$result %>% unnest(cols = everything())
  } else {
    tbl_dat_wide <- tbl_dat_wide$result
  }

  tbl_dat_wide <- tbl_dat_wide %>%
    tentative_process(arrange, tfrmt$sorting_cols, "Unable to arrange dataset") %>%
    tentative_process(select_col_plan, tfrmt$col_plan, "Unable to subset dataset columns") %>% ## select the columns & rename per col_plan
    col_align_all(tfrmt$col_align)

  list(
    data = tbl_dat_wide,
    tfrmt = tfrmt
  )
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
tentative_process <- function(.data, fx, param, fail_desc = NULL){
  if(is.null(param)){
    out <- .data
  } else{
    out <- .data %>%
      safely(fx)(param)
    if(!is.null(out[["error"]])){
      out <- .data
      if(is.null(fail_desc)){
        message("Unable to to apply formatting", format(substitute(fx)))
      }else{
        message(fail_desc)
      }
    }else{
      out <- out$result
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
