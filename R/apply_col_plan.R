#' @importFrom tidyr unite
#' @importFrom dplyr as_tibble relocate last_col right_join
#' @importFrom stringr str_remove str_detect
#' @importFrom purrr pmap_chr map2
#' @importFrom utils capture.output
#' @importFrom rlang quo
apply_col_plan <- function(data, tfrmt){

  if (is.null(tfrmt$col_plan)){
    if(!is.null(tfrmt$row_grp_plan$label_loc$location) &&
       tfrmt$row_grp_plan$label_loc$location=="noprint"){

      out <- data %>% select(-c(!!!tfrmt$group))

    } else {
      out <- data
    }
  } else {

    col_plan_names <- create_col_order(tfrmt$col_plan, tfrmt$column, names(data))
    out <- select(data, !!!col_plan_names)

  }

  out
}

create_col_order <- function(cp, columns, data_names){

  column_names <- map_chr(columns, as_label)

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

  ## remove duplicates, keeping _last_ version


  ## add back in the non-specified columns
  if(cp$.drop == FALSE){
    missing_cols <- setdiff(data_names, gsub("^-","",col_selections))
    col_selections <- c(col_selections,missing_cols)
  }

  quo_col_selections <- map(col_selections, ~char_as_quo(.x))

  do.call(vars, quo_col_selections)

}

col_plan_quo_to_vars <- function(x, column_names, data_names, preselected_cols){

  ## ensure data_names order matches preselected_cols
  data_names <- c(preselected_cols, setdiff(data_names, preselected_cols))

  ## only apply tidyselect to _bottom_ column
  data_names_tmp <- gsub(paste0(".*", .tlang_delim), "", data_names)

  selections <- eval_col_plan_quo(x[[1]], data_names_tmp, preselected_cols)
  selected <- data_names[which(data_names_tmp %in% selections)]

  ## if is subtraction, inverse selection to get subtracted columns and prepend with -
  if(grepl("^-",as_label(x[[1]]))){
    selected <- paste0("-",setdiff(data_names, selected))
  }else{
    if(!is.null(names(x))){

      if(length(selected) == 1){
        if(is.null(names(selected))){

          if(names(x) != ""){
            names(selected) <-
            gsub(paste0("(.*", .tlang_delim, ")*(", selections, ")"),
                 paste0("\\1", names(x)),
                 selected)
          }
        }else{
          if(names(x) != ""){
            if(names(selected) == ""){
              ref_name <- selected
            }else{
              ref_name <- names(selected)
            }

            names(selected) <-
              gsub(paste0("(.*", .tlang_delim, ")(", selections, ")"),
                   paste0("\\1", names(x)),
                   ref_name)
          }
        }
      }else{
        names(selected) <- paste0(names(x), seq_along(selected))
      }
    }
  }

  c(preselected_cols,selected)
}

col_plan_span_structure_to_vars <- function(x, column_names, data_names, preselected_cols){

  ## ensure data_names order matches preselected_cols
  data_names <- c(preselected_cols, setdiff(data_names, preselected_cols))

  if(is.null(names(data_names))){
    names(data_names) <- data_names
  }else{
    if(any(is_preserved_name <- names(data_names) == "")){
      names(data_names)[is_preserved_name] <- data_names[is_preserved_name]
    }
  }

  split_data_names <- tibble(
      original = data_names,
      new_name = names(data_names)
    ) %>%
    separate(
      original,
      into = column_names,
      sep = .tlang_delim,
      fill = "left"
    ) %>%
    separate(
      new_name,
      into = paste0("__tfrmt_new_name__", column_names),
      sep = .tlang_delim,
      fill = "left"
    )


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

        if(!is.null(names(selections))){

          rename_val <- names(selections)[[sel_id_idx]]

          if(rename_val == ""){
            rename_val <- sel_id_col_selections
          }

          if(is_valid_tidyselect_call(rlang::quo_get_expr(sel_id))){
            rename_val <- paste0(rename_val, seq_len(length(sel_id_col_selections)))
          }

          split_data_names <- split_data_names %>%
            mutate(
              !!col_name_quo := case_when(
                !!col_quo %in% sel_id_col_selections ~ rename_val,
                TRUE ~ !!col_name_quo
              )
            )
        }

        split_data_selections[[sel_id_idx]] <- split_data_names %>%
          filter(!!col_quo %in% sel_id_col_selections)

      }

      split_data_names <- bind_rows(split_data_selections) %>%
        unique()
    }

  }

  new_preselected_cols_full <- split_data_names %>%
    unite(
      "original",
      -starts_with("__tfrmt_new_name__"),
      sep = .tlang_delim
    ) %>%
    unite(
      "new_name",
      starts_with("__tfrmt_new_name__"),
      sep = .tlang_delim
    ) %>%
    mutate(across(everything(), remove_empty_layers, length(column_names) -1 ))

  selected <- new_preselected_cols_full$original

  new_names <- new_preselected_cols_full %>%
    filter(original != new_name)

  if(nrow(new_names) > 0){
    names(selected)[selected %in% new_names$original] <- new_names$new_name
  }

  c(preselected_cols,selected)

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

eval_col_plan_quo <- function(x, data_names, preselected_vals){
  if(identical(as_label(rlang::quo_get_expr(x)), "everything()")){

    # dump any pre-selected columns from everything() call. we are _not_ using
    # the default behavior of everything().
    data_names <- data_names[-seq_along(preselected_vals)]
  }
  eval_tidyselect_on_colvec(x, data_names)
}

