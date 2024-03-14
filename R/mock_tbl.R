#' Make mock data for display shells
#'
#' @param tfrmt tfrmt object
#' @param .default Number of unique levels to create for group/label values set
#'   to ".default"
#' @param n_cols Number of columns in the output table (not including
#'   group/label variables). If not supplied it will default to using the
#'   `col_plan` from the `tfrmt`. If neither are available it will use 3.
#'
#' @return tibble containing mock data
#'
#' @importFrom tidyr crossing unnest expand
#' @importFrom dplyr rowwise mutate pull rename ungroup coalesce group_by tibble
#'   across cur_column
#' @importFrom purrr map map_dfr map_chr map_dfc reduce
#' @importFrom rlang as_name is_empty
#' @importFrom tidyselect everything all_of
#'
#' @export
#' @examples
#'
#' tfrmt_spec <- tfrmt(
#'   label = label,
#'   column = column,
#'   param = param,
#'   value=value,
#'   body_plan = body_plan(
#'     frmt_structure(group_val=".default", label_val=".default", frmt("xx.x"))
#'     )
#'   )
#'
#' make_mock_data(tfrmt_spec)
#'
make_mock_data <- function(tfrmt, .default = 1:3, n_cols = NULL){

  body_plan <- tfrmt$body_plan
  grp_vars <- tfrmt$group %>% map_chr(as_name)


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
           across(all_of(grp_vars), ~ coalesce(.x, .data$..grp))) %>%
    select(-"..grp")  %>%
    rowwise()  %>%
    mutate(across(!!tfrmt$param, ~ process_for_mock(.x, cur_column(), 1)),
           across(all_of(grp_vars), ~ process_for_mock(.x, cur_column(), .default)))

  expand_cols <- c(tfrmt$group)

  if(!quo_is_missing(tfrmt$label)){
    all_frmt_vals <- all_frmt_vals %>%
      mutate(
        across(!!tfrmt$label, ~ process_for_mock(.x, cur_column(), .default))
      )
    expand_cols <- c(expand_cols, tfrmt$label )
  }

  expand_cols <- c(expand_cols, tfrmt$param)

  output_dat <- all_frmt_vals %>%
    unnest(everything()) %>%
    group_by(.data$frmt_num) %>%
    expand(!!!expand_cols) %>%
    ungroup() %>%
    add_sorting_cols(tfrmt$sorting_cols)

  ## add `column` columns
  col_def <- make_col_df(column = tfrmt$column, group = tfrmt$group,
                         label = tfrmt$label,
                         sorting_cols = tfrmt$sorting_cols,
                         col_plan = tfrmt$col_plan,
                         n_cols = n_cols)

  output_dat <- output_dat %>%
    mutate(
      `__tfrmt__mock__columns` = list(col_def)
    ) %>%
    unnest("__tfrmt__mock__columns") %>%
  #Add big N's
    add_mock_big_ns(column = tfrmt$column,
                    param = tfrmt$param,
                    big_n_struct = tfrmt$big_n)



  # remove the frmt_num field
  output_dat %>%
    select(-"frmt_num") %>%
    unique()

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


#' Regex to remove things like contains, and - etc.
#'
#' @param names string vec
#' @param dont_inc values to remove
#'
#' @return cleaned up string vec
#' @noRd
clean_col_names <- function(names, dont_inc){
  names %>%
    map_chr(as_label) %>%
    str_remove_all('^.*\\(\\"') %>%
    str_remove_all('^-') %>%
    str_remove_all('\\"\\)') %>%
    setdiff(dont_inc)
}

# Adds the sorting columns if relevant otherwise just returns data
add_sorting_cols <- function(data, sorting_cols){
  if(!is.null(sorting_cols)){

    sorting_cols_vars <- sorting_cols %>% map_chr(as_name)
    n_sorting_cols <- length(sorting_cols_vars)

    sorting_cols_def <- map_dfc(seq_len(n_sorting_cols), function(x){
      tibble(!!sorting_cols_vars[x] := 1)
    })

    data <- data %>%
      mutate(
        `__tfrmt__mock__sorting_col` = list(sorting_cols_def)
      ) %>%
      unnest("__tfrmt__mock__sorting_col")
  }
  data
}

make_col_df <- function(column, group, label, sorting_cols, col_plan, n_cols){

  column_vars <- column %>% map_chr(as_label)
  grp_lb_vars <- c(group %>% map_chr(as_name), as_label(label), sorting_cols %>% map_chr(as_name))
  if(identical(column_vars, "__tfrmt__column")){
    column_vars <- "col"
  }

  n_spans <- length(column_vars)

  # Use provided column names if there is no spanning
  if(col_plan_test(col_plan) & n_spans == 1 & is.null(n_cols)){
    cols_to_use <- col_plan$dots %>%
      clean_col_names(dont_inc = grp_lb_vars)
    col_def <- tibble(!!column_vars[n_spans] := cols_to_use)
  } else if(col_plan_test(col_plan) & is.null(n_cols)){
    # Gets the lowest level columns only
    low_lvl_vars <- col_plan$dots %>%
      discard(is.list) %>%
      clean_col_names(dont_inc = grp_lb_vars)

    low_lvl_def <- tibble(!!column_vars[max(n_spans)] := low_lvl_vars)

    # creates a df for each span structure
    span_df <- col_plan$dots %>%
      keep(is.list) %>%
      map_dfr(function(x){
        span_df <- x %>%
          map(~clean_col_names(., c())) %>%
          reduce(crossing) %>%
          unnest(cols = everything())
        names(span_df) <- names(x)
        span_df
      })
    col_def <- bind_rows(low_lvl_def, span_df)

  } else {
    n_cols <- ifelse(is.null(n_cols), 3, n_cols)
    col_def <- tibble(!!column_vars[n_spans] := paste0(column_vars[[n_spans]], seq(1:n_cols)))
    if(n_spans > 1){
      col_spans_df <- map_dfc(seq_len(n_spans-1), function(x){
        tibble(!!column_vars[x] := rep(paste0("span_", column_vars[x]), n_cols))
      })
      col_def <- bind_cols(col_spans_df, col_def)
    }
  }
  col_def
}

add_mock_big_ns <- function(data, column, param, big_n_struct){
  if(!is.null(big_n_struct)){
    col <- column %>% last()
    col_vals <- data %>%
      pull(!!col) %>%
      unique()

    data <- tibble(!!col := col_vals,
           !!param := big_n_struct$param_val) %>%
      bind_rows(data, .)
  }
  data
}

# Check the col plan contain positive information, isn't null, and doesn't contain `everything()`
col_plan_test <- function(col_plan){
  if(is.null(col_plan)){
    out <- FALSE
  } else {
    all_names <- col_plan$dots %>%
      map_chr(as_label)
    first_chr <- all_names %>%
      str_sub(end = 1)
    out <- (!all(first_chr == "-")) && (!"everything()" %in% all_names)
  }
  out
}
