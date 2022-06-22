#' Make mock data for display shells
#'
#' @param tfrmt tfrmt object
#' @param .default Number of unique levels to create for group/label values set to ".default"
#' @param n_cols Number of columns in the output table (not including group/label variables)
#'
#' @return tibble containing mock data
#'
#' @importFrom tidyr crossing unnest expand
#' @importFrom dplyr rowwise mutate pull rename ungroup coalesce group_by tibble across cur_column
#' @importFrom purrr map map_dfr map_chr
#' @importFrom rlang as_name
#' @importFrom tidyselect everything all_of
#'
#' @noRd
make_mock_data <- function(tfrmt, .default = 1:3, n_cols = 3){

  body_plan <- tfrmt$body_plan
  grp_vars <- tfrmt$group %>% map_chr(as_name)
  column_vars <- tfrmt$column %>% map_chr(as_name)
  sorting_cols <- tfrmt$sorting_cols %>% map_chr(as_name)

  # create tibble of all frmt_structure grp/label/param: 1 row per group_val per frmt_structure
  all_frmt_spec <- body_plan %>%
    map_dfr(
      function(x){
        crossing(
            # if group_val is a named list, return as a tibble with list names as colnames
            # otherwise (group_val = ".default") convert to tibble with colname "grp"
            if(is.list(x$group_val)) as_tibble(x$group_val) else tibble(..grp = x$group_val[[1]]),
            !!tfrmt$label := x$label_val,
            !!tfrmt$param := x$param_val
          )
    },
    .id = "frmt_num")


  # find out which group variables are not present
  cols_to_add <- setdiff(c("..grp", grp_vars), names(all_frmt_spec))

  # add the missing group variables & fill using .grp
  # & replace .default's
  all_frmt_vals <- bind_cols(all_frmt_spec,
                             map(cols_to_add, function(x) tibble(!!x := NA_character_))) %>%
    mutate(..grp = replace_na(.data$..grp, ".default"),
           across(grp_vars, ~ coalesce(.x, .data$..grp))) %>%
    select(-.data$..grp)  %>%
    rowwise()  %>%
    mutate(across(!!tfrmt$param, ~ process_for_mock(.x, cur_column(), 1)),
           across(!!tfrmt$label, ~ process_for_mock(.x, cur_column(), .default)),
           across(all_of(grp_vars), ~ process_for_mock(.x, cur_column(), .default)))

  # Within a frmt, do all combinations of values
  output_dat <- all_frmt_vals %>%
    unnest(everything()) %>%
    group_by(.data$frmt_num) %>%
    expand(!!!tfrmt$group, !!tfrmt$label, !!tfrmt$param) %>%
    ungroup


  browser()

  ## add columns
  if(is.null(tfrmt$col_plan)){

    n_spans <- length(column_vars)
    col_def <- tibble(!!column_vars[n_spans] := paste0("col", seq(1:n_cols)))

    if(n_spans > 1){
      col_spans_df <- purrr::map_dfc(seq_len(n_spans-1), function(x){
        tibble(!!column_vars[x] := rep(paste0("span_", column_vars[x]), n_cols))
      })

      col_def <- bind_cols(
        col_spans_df, col_def
      )
    }

  }else{

    col_plan_dots <- tfrmt$col_plan$dots
    ## evaluate into valid column values
    col_values <- process_col_plan_dots_to_chr(col_plan_dots, existing_cols = colnames(output_dat), n_cols = n_cols)
    col_def <- tibble(!!column_vars[1] := col_values)

  }

  output_dat <- output_dat %>%
    mutate(
      `__tfrmt__mock__col` = list(col_def)
    ) %>%
    unnest(`__tfrmt__mock__col`)




  output_dat %>%
    select(-.data$frmt_num)

}


#' Given the input value, generate values to be used for mock data
#'
#' @param x input character value
#' @param column type of column (group, label, param)
#' @param .default Number of numbered values to create
#'
#' @return list of character values
#' @noRd
process_for_mock <-function(x, column, .default = 1:3){

      if(x == ".default"){
        str_c(column, "_", .default) %>% list
      } else {
        list(x)
      }
}

#' given col_plan and current columns of mock data, generate values to be used for columns
#'
#' @importFrom rlang quo_get_expr quo_is_call
process_col_plan_dots_to_chr <- function(var_list, existing_cols, n_cols = 3){

  var_list_vals <- map_chr(var_list,function(x){

    if(rlang::quo_is_symbol(x)){
      return(as_label(x))
    }else{
      x_expr <- rlang::quo_get_expr(x)

      ## tidyselect calls
      if(is_valid_tidyselect_call(x_expr)){
        return(process_tidyselect_call_to_chr(x_expr, n_cols = n_cols))

      ## a quo/vars removing a column ie -my_column
      }else if(is_valid_quo_call(x_expr)){
        return(gsub("^-","",as_label(x_expr)))

      ## not sure how to handle this column, return NULL with a message
      }else{
        message("Unhandled col_plan value for mock: `", format(x_expr),"`")
        NULL
      }
    }

  })

  ## remove null values
  var_list_vals <- unique(var_list_vals[!sapply(var_list_vals, is.null)])

  ## remove potential duplicated columns pre-existing in data
  var_list_vals <- setdiff(var_list_vals, existing_cols)

  return(var_list_vals)

}


process_tidyselect_call_to_chr <- function(x_expr, n_cols){

  x_expr_list <- as.list(x_expr)

  func <- as.character(x_expr_list[[1]])

  ## these functions use regex. extract the string exactly. may need more nuance in the future
  if(func %in% c("starts_with","ends_with","contains","matches")){

    # these regex should be simple enough to preserve as is and pass through
    if(func %in% c("starts_with","ends_with","contains")){
      return(as.character(x_expr_list[[2]]))
    }else{
    # these regex are likely not simple.
    # dont try to return the regex, it will probably not be a valid string and
    # not work. for a mock this is probably fine
    return(NULL)
    }

  }else if(func %in% c("all_of","any_of")){
    ## these are listed out, so we can provide them
    return(do.call(as.character,x_expr_list[-1]))

  }else if(func %in% c("everything")){
    ## everything is not clear, so lets make some balues
    return(paste0("col",seq_len(n_col)))

  }else if(func %in% c("num_range")){

    ## this is a given prefix and numeric range, so can be derived
    prefix_idx <- which(names(x_expr_list) %in% "prefix")
    range_idx <- which(names(x_expr_list) %in% "range")

    if(identical(prefix_idx,integer()) & identical(range_idx,integer())){
      prefix_idx <- 2
      range_idx <- 3
    }else if(!identical(prefix_idx,integer())){
      range_idx <- setdiff(c(2,3), prefix_idx)
    }else{
      prefix_idx <- setdiff(c(2,3), range_idx)
    }
    prefix_call <- x_expr_list[[prefix_idx]]
    range_call <- x_expr_list[[range_idx]]

    return(do.call(paste0,list(prefix_call, range_call)))

  }else{

    ## any other tidyselects are not clear, so return NULL. this should be fine for a mock.
    NULL
  }

}
