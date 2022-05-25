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

  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  tbl_dat <- apply_table_frmt_plan(
    .data = .data,
    table_frmt_plan = tfrmt$body_style,
    group = tfrmt$group,
    label = tfrmt$label,
    param = tfrmt$param,
    values = tfrmt$values,
    column = tfrmt$column,
    mock = mock
  ) %>%
    tentative_process(apply_col_align_plan, tfrmt$col_align,
                      tfrmt$column, tfrmt$values,
                      fail_desc= "Unable to align dataset")

  ## append span structures to dataset for handling post-this function
  if(!is.null(tfrmt$col_plan$span_structures)){
    tbl_dat_span_cols <- apply_span_structures_to_data(
      tfrmt,
      tbl_dat
    )
  }else{
    tbl_dat_span_cols <- tbl_dat
  }

  tbl_dat_wide <- safely(pivot_wider)(
    tbl_dat_span_cols,
    names_from = c(starts_with(.tlang_struct_col_prefix), !!!tfrmt$column),
    names_sep = .tlang_delim,
    values_from = !!tfrmt$values
    )


  if (mock == TRUE &&
      length(tbl_dat_wide$warnings)>0 &&
      str_detect(tbl_dat_wide$warnings, paste0("Values from `", as_label(tfrmt$values), "` are not uniquely identified"))){
    message("Mock data contains more than 1 param per unique label value. Param values will appear in separate rows.")
    tbl_dat_wide <- tbl_dat_wide$result %>%
      unnest(cols = everything()) %>%
      clean_spanning_col_names()
  } else {
    tbl_dat_wide <- tbl_dat_wide$result %>%
      clean_spanning_col_names()
  }

  tbl_dat_wide <- tbl_dat_wide %>%
    tentative_process(arrange_enquo, tfrmt$sorting_cols, fail_desc= "Unable to arrange dataset") %>%
    #Select before grouping to not have to deal with if it indents or not
    tentative_process(select_col_plan, tfrmt, fail_desc = "Unable to subset dataset columns") %>%
    tentative_process(apply_row_grp_plan, tfrmt$row_grp_style, tfrmt$group, tfrmt$label)


  tbl_dat_wide
}


#' Tentatively apply an element
#'
#' Will only apply the element to the data if the param isn't null
#'
#' @param .data data to process
#' @param fx processing function
#' @param ... inputs supplied to function arguments
#'
#' @return processed data
#' @importFrom purrr map_lgl
#' @noRd
tentative_process <- function(.data, fx, ..., fail_desc = NULL){
  args <- list(...)

  if(any(map_lgl(args, is.null))){
    out <- .data
  } else{
    out <- .data %>%
      safely(fx)(...)
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

arrange_enquo <- function(dat, param){
  arrange(dat, !!!param)
}

#' Clean Spanning column names
#'
#' This function removes the prefix on columns that aren't nested
#' @param data data to rename
#'
#' @return dataset with renaming in needed
#' @noRd
#' @importFrom stringr str_count str_remove
clean_spanning_col_names <- function(data){
  # Get number of layers
  lyrs <- names(data) %>%
    str_count(.tlang_delim) %>%
    max()
  # remove the layering for unnested columns
  empty_layers <- strrep(paste0("NA", .tlang_delim), lyrs)
  if(empty_layers != ""){
    data <- data %>%
      rename_with(~str_remove(., empty_layers))
  }
  data
}
