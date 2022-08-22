apply_footnote_meta <- function(.data, footnote_plan, col_plan_vars, element_row_grp_loc,
                                group, label, columns){
  footnote_locs <- footnote_plan$struct_list %>%
    map(locate_fn, .data = .data,
        col_plan_vars = col_plan_vars, element_row_grp_loc = element_row_grp_loc,
        group = group, label = label, columns = columns)

  structure(
    .data,
    .footnote_locs = footnote_locs,
    class = c("processed_tfrmt_tbl",class(.data))
  )
}


#' Locates the footnote for a given  footnote structure
#'
#' @param footnote_structure footnote structure
#' @param .data data set
#' @param col_plan_vars col plan variables
#' @param element_row_grp_loc row group location element
#' @param group group quosures
#' @param label label quosure
#' @param columns column quosure
#'
#' @return list with the row, column and footnote
#' @noRd purrr cross_df
locate_fn <- function(footnote_structure, .data, col_plan_vars, element_row_grp_loc,
                            group, label, columns){
  loc_info <- footnote_structure %>%
    discard(is.null) %>%
    .[names(.) != "footnote_text"]

  spanning <- FALSE
  col_loc <- NULL
  row_loc <- NULL

  # Get column information
  if(names(loc_info) == "column_val"){
    col_str <- columns %>% map_chr(as_label)

    loc_col_df <- loc_info$column_val %>%
      cross_df()
    col_val_nm <- names(loc_info$column_val)

    col_loc_df <- split_data_names_to_df(NULL, col_plan_vars%>%
                                           map_chr(as_label), col_str) %>%
      inner_join(loc_col_df, by = col_val_nm)

    # if not a column return the spanning column name
    span_lvl <- col_str %in% col_val_nm %>%
      which() %>%
      max() %>%
      col_str[.]

    if(last(col_str) == span_lvl){
      col_loc <- unite_df_to_data_names(col_loc_df, preselected_cols = c(), column_names = col_str)
      if(!is_null(names(loc_col))){
        col_loc <- if_else(names(col_loc) != "", names(col_loc), col_loc)
      }
    } else {
      spanning <- TRUE
      col_loc <- col_loc_df %>%
        pull(paste0("__tfrmt_new_name__", span_lvl)) %>%
        unique()
    }
  }


  if(names(loc_info) %in% c("group_val", "label_val")){

    # Add row information
    grp_expr <- expr_to_filter(group, loc_info$group_val)
    lbl_expr <- expr_to_filter(label, loc_info$label_val)


    filter_expr <- paste(
      c(lbl_expr,grp_expr),
      collapse = "&"
    ) %>%
      parse_expr()

    row_loc<- .data %>%
      ungroup() %>%
      mutate(`___tfrmt_test` = !!filter_expr,
             `___tfrmt_TEMP_rows` = row_number()) %>%
      filter(`___tfrmt_test`) %>%
      pull(`___tfrmt_TEMP_rows`)

    # If column is still missing
    if(is_null(col_loc)){
      if(element_row_grp_loc$location == "indented" | !is_null(loc_info$label_val)){
        col_loc <- as_label(label)
      } else if(element_row_grp_loc$location == "noprint" & !is_null(loc_info$group_val)){
        row_loc <- NULL
        warning("Can not apply footnotes to group columns without printing the group")
      } else if(spanning){
        row_loc <- NULL
        warning("Can not apply footnotes to group cells when only the columns and rows are specified")
      }else if(length(group) > 1 &loc_info$group_val != as_label(first(group))){
        col_loc <- as_label(label)
      } else {
        col_loc <- as_label(group[[1]])
      }
    }
  }

  list(row = row_loc, col = col_loc, note = footnote_structure$footnote_text)

}



