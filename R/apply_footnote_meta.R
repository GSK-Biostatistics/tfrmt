#' Apply the footnote metadata to data
#'
#' Gets the location of each footnote in the footnote plan and adds it to the dataset as an attribute
#' @param .data formatted data
#' @param footnote_plan footnote plan
#' @param col_plan_vars named vector of the columns with new and old names
#' @param element_row_grp_loc element_row_grp_loc to help with where the footnote should go
#' @param group symbolic group(s)
#' @param label symbolic label
#' @param columns symbolic column(s)
#'
#' @return a processed tfrmt tbl object, which has a .footnote_locs attribute
#' @noRd
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
#' @importFrom dplyr inner_join first last cur_group_id
locate_fn <- function(footnote_structure, .data, col_plan_vars, element_row_grp_loc,
                            group, label, columns){

  col_info <- get_col_loc(footnote_structure, .data, col_plan_vars, columns)

  loc_info <- get_row_loc(footnote_structure, .data, element_row_grp_loc,
                          group, label, col_info)
  loc_info$note <- footnote_structure$footnote_text
  loc_info
}




#' Get the column location of the footnote
#'
#' @param footnote_structure a single footnote structure
#' @param .data formatted data
#' @param col_plan_vars named vector of the columns with new and old names
#' @param columns symbolic column(s)
#'
#' @return list with column locations (col) and if they are spanning or not (spanning)
#' @noRd
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
        expand.grid(stringsAsFactors = FALSE)
      col_val_nm <- names(loc_info$column_val)
    }

    col_loc_df <- split_data_names_to_df(NULL, col_plan_vars%>%
                                           map_chr(as_label), col_str) %>%
      inner_join(loc_col_df, by = col_val_nm)

    if(nrow(col_loc_df) == 0){

      message_text <- c(
       paste0(
        "The provided column location does not exist in the provided data for the footnote",
        "\"",
        footnote_structure$footnote_text,
        "\""
        ),
        "Provided column location:"
        )

      for(col_var in col_val_nm){
        message_text <- c(
          message_text,
          paste0(col_var,": ",paste0("`",loc_info$column_val[[col_var]],"`", collapse = ","))
        )
      }

      message(paste0(message_text, collapse = "\n"))

      out <- list(col = NULL, spanning = FALSE)

    }else{
      # if not a column return the spanning column name
      span_lvl <- col_str %in% col_val_nm %>%
        which() %>%
        max() %>%
        col_str[.]

      if(last(col_str) == span_lvl){
        col_loc <- unite_df_to_data_names(col_loc_df, preselected_cols = c(), column_names = col_str)
        if(!is.null(names(col_loc))){
          col_loc <- if_else(names(col_loc) != "", names(col_loc), unname(col_loc))
        }
        out <- list(col = col_loc, spanning = FALSE)
      } else {
        col_loc <- col_loc_df %>%
          pull(paste0("__tfrmt_new_name__", span_lvl)) %>%
          unique()
        out <- list(col = col_loc, spanning = TRUE)
      }
    }
  } else {
    out <- list(col = NULL, spanning = FALSE)
  }
  out
}

#' Get the row location of the footnote
#'
#' @param footnote_structure a single footnote structure
#' @param .data formatted dataset
#' @param element_row_grp_loc row group location element
#' @param group group quosures
#' @param label label quosure
#' @param col_info list of column information from [get_col_loc()]
#'
#' @return a list with all column information, row, col, and spanning
#' @noRd
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
       warning("Cannot apply footnotes to rows when you have only specified a spanning column")
       col_info$row <- NULL
     } else if(row_grp == "noprint" & !is_empty(loc_info$group_val)){
       warning("Can not apply footnotes to group columns when 'noprint' is set")
       col_info$row <- NULL
     } else {
       group_str <- group %>% map_chr(as_label)
       # Test if there are more than the first group
       highest_grp <- setdiff(names(loc_info$group_val), first(group_str)) %>%
         length() == 0

       #Will the footnote be in the label column
       lb_col_test <- !is.null(loc_info$label_val) | #label level so in label col
         row_grp == "indented" | #indented so in label column
         (!highest_grp & !row_grp %in% c("", "gtdefault")) #there is a row_grp_plan and this isn't the highest group so indented

       if(lb_col_test){
         grp_expr <- expr_to_filter(group, loc_info$group_val)
         label_vals <- ifelse(is.null(loc_info$label_val), loc_info$group_val,
                               loc_info$label_val) %>% unlist()
         lbl_expr <- expr_to_filter(label, label_vals)

         filter_expr <- paste(c(lbl_expr,grp_expr),collapse = "&") %>%
           parse_expr()

         col_info$row <- .data %>%
           ungroup() %>%
           mutate(
             across(c(!!!group, !!label),
                    ~str_remove(.x, paste0("^", element_row_grp_loc$indent, "+"))),
             `___tfrmt_test` = !!filter_expr,
             `___tfrmt_TEMP_rows` = row_number()) %>%
           filter(.data$`___tfrmt_test`) %>%
           pull(.data$`___tfrmt_TEMP_rows`)

         col_info$col <- ifelse(is.null(col_info$col), as_label(label),
                                     col_info$col)

       } else if(highest_grp){
         filter_expr <- expr_to_filter(group, loc_info$group_val) %>%
           parse_expr()
         col_info$row<-.data %>%
           group_by(!!first(group)) %>%
           mutate(`___tfrmt_grp_n` = cur_group_id(),
                  `___tfrmt_test` = !!filter_expr) %>%
           filter(.data$`___tfrmt_test`) %>%
           pull(.data$`___tfrmt_grp_n`) %>%
           unique()
         col_info$col <- ifelse(is.null(col_info$col), first(group_str),
                                     col_info$col)
       } else if(row_grp %in% c("", "gtdefault")){
         filter_expr <- expr_to_filter(group, loc_info$group_val) %>%
           parse_expr()
         col_info$row<-.data %>%
           group_by(!!first(group)) %>%
           mutate(`___tfrmt_grp_n` = cur_group_id(),
                  `___tfrmt_test` = !!filter_expr) %>%
           filter(.data$`___tfrmt_test`) %>%
           pull(.data$`___tfrmt_grp_n`) %>%
           unique()

         lowest_grp <- group_str %in% names(loc_info$group_val) %>%
           which() %>%
           max() %>%
           group_str[.]

         col_info$col <- ifelse(is.null(col_info$col), lowest_grp,
                                col_info$col)

       }

     }
  } else{
    col_info$row <- NULL
  }
  col_info

}
