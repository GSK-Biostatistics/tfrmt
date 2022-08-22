#' Title
#'
#' @param .data Data to apply the tfrmt to
#' @param tfrmt tfrmt object to apply to the data
#' @param mock Logical value is this is for a mock or not. By default `FALSE`
#'
#' @return formatted tibble
#'
#' @noRd
apply_tfrmt <- function(.data, tfrmt, mock = FALSE){

  validate_cols_match(.data, tfrmt, mock)

  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  tbl_dat <- .data %>%
    remove_big_ns(param = tfrmt$param,
                  big_n_structure = tfrmt$big_n) %>%
    apply_table_frmt_plan(
      .data = .,
      table_frmt_plan = tfrmt$body_plan,
      group = tfrmt$group,
      label = tfrmt$label,
      param = tfrmt$param,
      value = tfrmt$value,
      column = tfrmt$column,
      mock = mock
    ) %>%
    tentative_process(
      apply_col_style_plan_alignment_values,
      tfrmt,
      fail_desc = "Failure while aligning data values"
    )

  non_data_cols <- setdiff(names(tbl_dat),c(tfrmt$column, tfrmt$param, tfrmt$value) %>% map_chr(as_label))
  data_col_values <- tbl_dat %>% pull(!!tfrmt$column[[length(tfrmt$column)]]) %>% unique()
  big_n_df <- get_big_ns(.data, param = tfrmt$param,
                         value = tfrmt$value,
                         columns =tfrmt$column,
                         big_n_structure = tfrmt$big_n,
                         mock = mock)

  tbl_dat_wide <- tbl_dat %>%
    pivot_wider_tfrmt(tfrmt, mock) %>%
    # arrange if sorting cols are applied
    tentative_process(arrange_enquo, tfrmt$sorting_cols, fail_desc = "Unable to arrange dataset") %>%

    # Apply alignment to non-data columns
    tentative_process(
      apply_col_style_plan_alignment_non_values,
      tfrmt,
      non_data_cols,
      data_col_values,
      fail_desc = "Failure while aligning non-data values"
    )

  col_plan_vars <- tentative_process(
    names(tbl_dat_wide),
    create_col_order,
    cp = tfrmt$col_plan,
    columns = tfrmt$column,
    fail_desc = "Unable to create dataset subset vars"
    ) %>%
    tentative_process(
      .,
      apply_big_n_df,
      columns = tfrmt$column,
      value = tfrmt$value,
      big_n_df = big_n_df,
      fail_desc = "Unable to add big N's"
    )

  tbl_dat_wide_processed <- tbl_dat_wide %>%
    #Select before grouping to not have to deal with if it indents or not
    tentative_process(apply_col_plan, col_plan_vars, fail_desc = "Unable to subset dataset columns") %>%

    #Apply row group structures defined in row_grp_plan
    tentative_process(
      apply_row_grp_struct,
      tfrmt$row_grp_plan$struct_ls,
      tfrmt$group,
      tfrmt$label,
      fail_desc = "Unable to apply row group structure"
    ) %>%
    tentative_process(apply_row_grp_lbl,
                      tfrmt$row_grp_plan$label_loc,
                      tfrmt$group,
                      tfrmt$label)


  structure(
    tbl_dat_wide_processed,
    .col_plan_vars = col_plan_vars,
    class = c("processed_tfrmt_tbl",class(tbl_dat_wide_processed))
  )

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
#' @importFrom rlang is_empty
#' @noRd
tentative_process <- function(.data, fx, ..., fail_desc = NULL){

  args <- list(...)

  if(any(map_lgl(args, is_empty))){
    out <- .data
  } else{
    out <- .data %>%
      safely(fx)(...)
    if(!is.null(out[["error"]])){
      if(is.null(fail_desc)){
        fail_desc <- paste0("Unable to to apply ",format(substitute(fx)),".")
      }
      fail_desc <-paste0(
        fail_desc,"\n",
        "Reason: ",
        out[["error"]]$message
      )
      message(fail_desc)

      out <- .data
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
    req_quo <- c("label", "param", "value")
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

#' Arrange data based on quosures
#'
#' @param dat data to arrange
#' @param param list of quosures to arrange on
#'
#' @noRd
#' @importFrom dplyr arrange
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
#' @importFrom dplyr rename_with
clean_spanning_col_names <- function(data){
  # Get number of layers
  lyrs <- count_spanning_layers(names(data))
  # remove the layering for unnested columns
  if(lyrs > 0){
    data <- data %>%
      rename_with(~remove_empty_layers(.x, nlayers = lyrs))
  }
  data
}

count_spanning_layers <- function(x){
  x %>%
    str_count(.tlang_delim) %>%
    max()
}


## also used in select_col_plan to process column names the same way
remove_empty_layers <- function(x, nlayers = 1){
  empty_str <- paste0("^",strrep(paste0("NA", .tlang_delim), nlayers))
  str_remove(x, empty_str)
}

#' Pivot formatted values into a wide dataset
#'
#' @param data
#'
#' @return data pivoted wider
#' @noRd
#'
#' @importFrom purrr quietly
#' @importFrom tidyselect starts_with everything
#' @importFrom dplyr group_by across summarise n tally pull
#' @importFrom stringr str_detect
#' @importFrom tidyr unnest
pivot_wider_tfrmt <- function(data, tfrmt, mock){

  # check if data can be transformed wide w/o list columns
  num_rec_by_row <- data %>%
    group_by(across(c(-!!tfrmt$value, -!!tfrmt$param))) %>%
    summarise(
      param_list = list(!!tfrmt$param),
      n = n()
    )

  if (any(num_rec_by_row$n>1) ){

    val_fill <- list("")

    if(!mock){
      suggested_frmt_structs <- num_rec_by_row %>%
        ungroup %>%
        filter(n > 1) %>%
        select(-c(!!!tfrmt$column)) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          suggested_frmt_struct = frmt_struct_string(
            grp = list(!!!tfrmt$group),
            lbl = !!tfrmt$label,
            param_vals = .data$param_list
            )
        ) %>%
        pull(.data$suggested_frmt_struct) %>%
        paste0("- `",.,"`",collapse = "\n")


     inform(
       paste0(
       "Multiple param listed for the same group/label values.\n",
       "The following frmt_structures may be missing from the body_plan\n",
       "or the order may need to be changed:"
       ),
       body = suggested_frmt_structs,
       class = "_tlang_missing_frmt_structs"
     )
    }
  } else {
    val_fill <- ""
  }


  tbl_dat_wide <- data %>%
    select(-!!tfrmt$param) %>%
    quietly(pivot_wider)(
      names_from = c(starts_with(.tlang_struct_col_prefix), !!!tfrmt$column),
      names_sep = .tlang_delim,
      values_from = !!tfrmt$value,
      values_fill = val_fill
      )

  if (mock == TRUE && length(tbl_dat_wide$warnings)>0 &&
      str_detect(tbl_dat_wide$warnings, paste0("Values from `", as_label(tfrmt$value), "` are not uniquely identified"))){
    message("Mock data contains more than 1 param per unique label value. Param values will appear in separate rows.")
    tbl_dat_wide <- tbl_dat_wide$result %>%
      unnest(cols = everything()) %>%
      clean_spanning_col_names()
  } else {
    tbl_dat_wide <- tbl_dat_wide$result %>%
      clean_spanning_col_names()
  }

  tbl_dat_wide

}


frmt_struct_string <- function(grp, lbl, param_vals){

  group_names <- substitute(grp) %>% map_chr(as_label) %>% .[-1]

  if(length(group_names) > 1){
    group_val_char <- capture.output(dput(setNames(grp, group_names)))
  }else if(length(group_names) == 1){
    group_val_char <-  capture.output(dput(grp[[1]]))
  }else{
    group_val_char <-  "\".default\""
  }

  label_val_char <- capture.output(dput(lbl))

  param_expr_char <- paste0("\"",paste0("{",param_vals,"}", collapse = ", "),"\"")
  param_frmt_char <- paste0(param_vals," = frmt(\"xx\")", collapse = ", ")

  paste0(
    "frmt_structure(",
    "group_val = ",group_val_char,
    ", label_val = ",label_val_char,
    ", frmt_combine(",
    param_expr_char,",",
    param_frmt_char,
    "))"
  )
}
