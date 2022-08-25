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
#' @noRd
#' @importFrom purrr cross_df
#' @importFrom dplyr inner_join first last cur_group_id
locate_fn <- function(footnote_structure, .data, col_plan_vars, element_row_grp_loc,
                            group, label, columns){
  col_info <- get_col_loc(footnote_structure, .data, col_plan_vars, columns)

  loc_info <- get_row_loc(footnote_structure, .data, element_row_grp_loc,
                          group, label, col_info)
  loc_info$note <- footnote_structure$footnote_text
  loc_info
}




get_col_loc <- function(footnote_structure, .data, col_plan_vars, columns){
  loc_info <- footnote_structure %>%
    discard(is.null) %>%
    .[names(.) != "footnote_text"]

  # Get column information
  if( "column_val" %in% names(loc_info)){
    col_str <- columns %>% map_chr(as_label)

    if(is_empty(names(loc_info$column_val))){
      col_val_nm <- col_str
      loc_col_df <- tibble(!!col_str := loc_info$column_val)
    } else {
      loc_col_df <- loc_info$column_val %>%
        cross_df()
      col_val_nm <- names(loc_info$column_val)
    }

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
      if(!is.null(names(col_loc))){
        col_loc <- if_else(names(col_loc) != "", names(col_loc), col_loc)
      }
      out <- list(col_loc = col_loc, spanning = FALSE)
    } else {
      col_loc <- col_loc_df %>%
        pull(paste0("__tfrmt_new_name__", span_lvl)) %>%
        unique()
      out <- list(col_loc = col_loc, spanning = TRUE)
    }
  } else {
    out <- list(col_loc = NULL, spanning = FALSE)
  }
  out
}

get_row_loc <- function(footnote_structure, .data, element_row_grp_loc,
                        group, label, col_info){

  loc_info <- footnote_structure %>%
    discard(is.null) %>%
    .[names(.) != "footnote_text"]

  row_grp <- ifelse(is.null(element_row_grp_loc$location), "",
                     element_row_grp_loc$location)
   if(any(names(loc_info) %in% c("group_val", "label_val"))){
     # Things that don't have rows
     if(!is.null(col_info$spanning) && col_info$spanning){
       warning("Can not apply footnotes to group cells when only the columns and rows are specified")
       col_info$row_loc <- NULL
     } else if(row_grp == "noprint" & !is_empty(loc_info$group_val)){
       warning("Can not apply footnotes to group cells when only the columns and rows are specified")
       col_info$row_loc <- NULL
     } else {
       group_str <- group %>% map_chr(as_label)
       # Test if there are more than the first group
       highest_grp <- setdiff(names(loc_info$group_val), first(group_str)) %>%
         length() == 0

       #Will the footnote be in the label column
       if(!is.null(loc_info$label_val) || row_grp == "intented" |!highest_grp){
         grp_expr <- expr_to_filter(group, loc_info$group_val)
         label_vals <- ifelse(is.null(loc_info$label_val), loc_info$group_val,
                               loc_info$label_val)
         lbl_expr <- expr_to_filter(label, label_vals)

         filter_expr <- paste(c(lbl_expr,grp_expr),collapse = "&") %>%
           parse_expr()

         col_info$row_loc <- .data %>%
           ungroup() %>%
           mutate(
             across(c(!!!group, !!label), str_remove, paste0("^", element_row_grp_loc$indent)),
             `___tfrmt_test` = !!filter_expr,
             `___tfrmt_TEMP_rows` = row_number()) %>%
           filter(`___tfrmt_test`) %>%
           pull(`___tfrmt_TEMP_rows`)

         col_info$col_loc <- ifelse(is.null(col_info$col_loc), as_label(label),
                                     col_info$col_loc)

       } else if(highest_grp){
         col_info$row_loc<-.data %>%
           group_by(!!first(group)) %>%
           mutate(`___tfrmt_grp_n` = cur_group_id(),
                  `___tfrmt_test` = !!filter_expr) %>%
           filter(`___tfrmt_test`) %>%
           pull(`___tfrmt_grp_n`) %>%
           unique()
         col_info$col_loc <- ifelse(is.null(col_info$col_loc), first(group_str),
                                     col_info$col_loc)
       } else if(row_grp == ""){

       }


     }
  } else{
    col_info$row_loc <- NULL
  }
  col_info

}
