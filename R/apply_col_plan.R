#' @importFrom tidyr unite
#' @importFrom dplyr as_tibble relocate last_col
#' @importFrom stringr str_remove str_detect
#' @importFrom purrr pmap_chr map2
#' @importFrom utils capture.output
#' @importFrom rlang quo
apply_col_plan <- function(data, col_selection){


  if(is.character(col_selection)){
    quo_col_selections <- map(col_selection, ~char_as_quo(.x))
    col_selection <- do.call(vars, quo_col_selections)
  }

  select(data,!!!col_selection)

}

#' Creates a named vector explicitly calling all the columns
#'
#' @importFrom rlang is_empty
#' @importFrom purrr map map_chr
#'
#' @noRd
create_col_order <- function(data_names, columns, cp){

  if(is.null(cp)){
    col_selections <- data_names
  }else{
    if(is_empty(columns)){
      # create placeholder
      column_names <- "col"
    }else{
      column_names <- map_chr(columns, as_label)
    }

    col_selections <- c()

    for(cp_el_idx in seq_along(cp$dots)){
      cp_el <- cp$dots[cp_el_idx]
      if(!is_span_structure(cp_el[[1]])){
        col_selections <- col_plan_quo_to_vars(
          x = cp_el,
          column_names = column_names,
          data_names = data_names,
          preselected_cols = col_selections
        )
      }else{
        col_selections <- col_plan_span_structure_to_vars(
          x = cp_el,
          column_names = column_names,
          data_names = data_names,
          preselected_cols = col_selections
          )
      }

      col_selections <- col_selections[!duplicated(gsub("^-","",col_selections), fromLast = TRUE)]

    }

    ## add back in the non-specified columns
    if(cp$.drop == FALSE){
      missing_cols <- setdiff(data_names, gsub("^-","",col_selections))
      col_selections <- c(col_selections,missing_cols)
    }
  }

  quo_col_selections <- map(col_selections, ~char_as_quo(.x))

  do.call(vars, quo_col_selections)

}

col_plan_quo_to_vars <- function(x, column_names, data_names, preselected_cols,
                                 return_only_selected = FALSE, default_everything_behavior = FALSE){

  ## ensure data_names order matches preselected_cols
  split_data_names <- split_data_names_to_df(data_names, preselected_cols, column_names)

  col_id <- column_names[length(column_names)]
  col_quo <- quo(!!sym(col_id))
  col_name_quo <- quo(!!sym(paste0("__tfrmt_new_name__", col_id)))

  ## only apply tidyselect to _bottom_ column
  data_names_tmp <- split_data_names[[col_id]]

  selected <- eval_col_plan_quo(x[[1]], data_names_tmp, preselected_cols, default_everything_behavior = default_everything_behavior)

  ## if is subtraction, inverse selection to get subtracted columns and prepend with -
  if(grepl("^-",as_label(x[[1]]))){
    split_data_names <- split_data_names %>%
      filter(!(!!col_quo %in% selected)) %>%
      mutate(
        subtraction_status = TRUE
      )
  }else{
    split_data_names <- split_data_names %>%
      filter(!!col_quo %in% selected)

    if(!is.null(names(x)) && names(x) != ""){

      rename_val <- names(x)

      if(is_valid_tidyselect_call(quo_get_expr(x[[1]])) && length(selected) > 1){
        rename_val <- paste0(rename_val, seq_len(length(selected)))
      }

      rows_to_rename <- split_data_names[[as_label(col_quo)]] %in% selected
      split_data_names[rows_to_rename, as_label(col_name_quo)] <- rename_val

    }

  }

  unite_df_to_data_names(split_data_names, preselected_cols, column_names,
                         return_only_selected  = return_only_selected )
}

#' @importFrom rlang quo_get_expr quo
#' @importFrom tidyr separate unite
#' @importFrom dplyr filter pull mutate arrange left_join select
col_plan_span_structure_to_vars <- function(x, column_names, data_names, preselected_cols,
                                            return_only_selected = FALSE){

  split_data_names <- split_data_names_to_df(data_names, preselected_cols, column_names)

  col_selections <- list()

  ## evaluate selections to identify columns
  for(col_id_idx in seq_along(column_names)){

    col_id <- column_names[col_id_idx]
    col_quo <- quo(!!sym(col_id))
    col_name_quo <- quo(!!sym(paste0("__tfrmt_new_name__", col_id)))

    if(col_id %in% names(x[[1]])){

      selections <- x[[1]][[col_id]]
      split_data_selections <- list()

      for(sel_id_idx in seq_along(selections)){

        sel_id <- selections[[sel_id_idx]]
        sel_id_col_selections <- unique(eval_tidyselect_on_colvec(sel_id, split_data_names[[col_id]]))

        is_subtraction_selection <- grepl("^-",as_label(sel_id))

        if(!is_subtraction_selection){
          if(!is.null(names(selections)) && names(selections)[[sel_id_idx]] != ""){

            rename_val <- names(selections)[[sel_id_idx]]

            if(is_valid_tidyselect_call(quo_get_expr(sel_id)) && length(sel_id_col_selections) > 1){
              rename_val <- paste0(rename_val, seq_len(length(sel_id_col_selections)))
            }

            rows_to_rename <- split_data_names[[as_label(col_quo)]] %in% sel_id_col_selections
            split_data_names[rows_to_rename, as_label(col_name_quo)] <- rename_val

          }

          split_data_selections[[sel_id_idx]] <- split_data_names %>%
            filter(!!col_quo %in% sel_id_col_selections)

        }else{

          split_data_selections[[sel_id_idx]] <- split_data_names %>%
            mutate(
              subtraction_status = case_when(
                !(!!col_quo %in% sel_id_col_selections) ~ TRUE,
                TRUE ~ FALSE
              )
            )
        }
      }

      split_data_names <- bind_rows(split_data_selections) %>%
        unique()

      col_selections[[col_id]] <- split_data_names %>%
        pull(!!col_quo) %>%
        unique

    }else{

      split_data_names <- split_data_names %>%
        filter(!is.na(!!col_quo))

      col_selections[[col_id]] <- split_data_names %>%
        pull(!!col_quo) %>%
        unique
    }

  }

  ## create order df
  ords <- do.call("crossing",col_selections) %>%
    mutate(across(everything(), ~factor(.x, levels = col_selections[[cur_column()]]))) %>%
    arrange(across(everything())) %>%
    mutate(ord_col = seq_len(n()))

  split_data_names %>%
    left_join(ords, by = names(col_selections)) %>%
    arrange(.data$ord_col) %>%
    select(-"ord_col") %>%
    unite_df_to_data_names(preselected_cols, column_names,
                           return_only_selected = return_only_selected)

}

## given a string - x - see how to convert to a quosure.
##  if negative is TRUE, it will mark it as a `-`.
char_as_quo <- function(x) {

  is_negative <- grepl("^-", x)
  x <- gsub("^-", "", x)

  ## if x is a valid tidyselect call, leave it as is,
  ## otherwise wrap it in "`". This is so we can pass
  ## colnames with spaces (which are common in spanned columns)
  ## to quo. IE `spanned col header__tfrmt_delim__col1`
  x_text <- tryCatch({
    x_lang <- parse(text = x)[[1]]
    if (is_valid_tidyselect_call(x_lang)) {
      x
    } else{
      paste0("`", x, "`")
    }},
    error = function(e) {
      paste0("`", x, "`")
    }
  )

  if (is_negative) {
    expr_to_eval <- paste0("quo(-", x_text, ")")
  } else{
    expr_to_eval <- paste0("quo(", x_text, ")")
  }

  eval(parse(text = expr_to_eval)[[1]])
}

#' @importFrom rlang quo_get_expr as_label is_empty
eval_col_plan_quo <- function(x, data_names, preselected_vals, default_everything_behavior = FALSE){

  if(identical(as_label(x), "everything()") & !default_everything_behavior){
    # dump any pre-selected columns from everything() call. we are _not_ using
    # the default behavior of everything().

    if(!is_empty(preselected_vals)){
      data_names <- data_names[-seq_along(preselected_vals)]
    }
  }

  eval_tidyselect_on_colvec(x, data_names)
}


#' Creates a split out data.frame of the potential columns and their spans.
#'
#' Combines the original column names with the potential renamed ones from
#' preselected_cols into a single vector, removing duplicates then splits out
#' the contents into a data.frame.
#'
#' @param data_names original names of the dataset
#' @param preselected_cols named character vector of selected columns from the
#'   dataset. names of the vector reflect the new name to apply
#' @param column_names the original ARD column names as a character vector
#'
#' @noRd
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr separate
#' @importFrom tibble tibble
split_data_names_to_df <- function(data_names, preselected_cols, column_names){

  data_names <- c(preselected_cols, setdiff(data_names, preselected_cols))

  if(is.null(names(data_names))){
    names(data_names) <- data_names
  }else{
    if(any(is_preserved_name <- names(data_names) == "")){
      names(data_names)[is_preserved_name] <- data_names[is_preserved_name]
    }
  }

  tibble(
    original = data_names,
    new_name = names(data_names)
   ) %>%
    mutate(
      subtraction_status = str_detect(.data$original ,"^-"),
      original = str_remove(.data$original,"^-")
    ) %>%
    separate(
      .data$original,
      into = column_names,
      sep = .tlang_delim,
      fill = "left"
    ) %>%
    separate(
      .data$new_name,
      into = paste0("__tfrmt_new_name__", column_names),
      sep = .tlang_delim,
      fill = "left"
    )

}

#' Combines the split out data.frame of the potential columns and their spans into a named vector.
#'
#'
#' @param split_data_names tibble containing two columns for every
#'   "column_name" value - one that is the column name, the second is the column
#'   name prepended with "__tfrmt_new_name__". Finally, there is a "subtraction_status" column,
#'   indicating if the column should be subtracted or not.
#' @param preselected_cols named character vector of selected columns from the
#'   dataset. names of the vector reflect the new name to apply
#' @param column_names the original ARD column names as a character vector
#'
#' @noRd
#'
#' @importFrom dplyr case_when mutate pull
#' @importFrom tidyr unite
#' @importFrom tibble tibble
unite_df_to_data_names <- function(split_data_names, preselected_cols, column_names,
                                   return_only_selected = FALSE){

  new_preselected_cols_full <- split_data_names %>%
    unite("original",-c(starts_with("__tfrmt_new_name__"), "subtraction_status"),
          sep = .tlang_delim) %>%
    unite("new_name",
          starts_with("__tfrmt_new_name__"),
          sep = .tlang_delim) %>%
    mutate(across(c("original", "new_name"), ~remove_empty_layers(.x, length(column_names) -1)))

  selected <- new_preselected_cols_full %>%
    mutate(
      original = case_when(
        .data$subtraction_status == TRUE ~ paste0("-",.data$original),
        TRUE ~ .data$original
      )
    ) %>%
    pull(.data$original)

  new_names <- ifelse(new_preselected_cols_full$original != new_preselected_cols_full$new_name,
                      new_preselected_cols_full$new_name, "")

  names(selected) <- new_names

  if(return_only_selected){
    selected
  }else{
    c(preselected_cols,selected)
  }
}
