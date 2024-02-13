#' Apply the formatting to all values in the dataset
#'
#' @param .data data
#' @param table_frmt_plan styling element needed
#' @param group symbolic list of grouping
#' @param label symbolic label
#' @param param symbolic parameter
#' @param value symbolic value
#' @param mock Logical value is this is for a mock or not
#'
#' @noRd
#' @importFrom dplyr tibble mutate group_by arrange slice bind_cols group_split pull select starts_with
#' @importFrom purrr map map_dfr
#' @importFrom tidyr unnest
#' @importFrom rlang !! :=
apply_table_frmt_plan <- function(.data, table_frmt_plan, group, label, param, value, column, mock = FALSE,...){

  ## identify which formatting needs to be applied where
  .data <- .data %>%
    ungroup() %>%
    mutate(TEMP_row = row_number())

  TEMP_appl_row <- table_frmt_plan %>%
    map(fmt_test_data, .data, label, group, param)

  TEMP_fmt_to_apply <- table_frmt_plan %>% map(~.$frmt_to_apply[[1]])

  dat_plus_fmt <- tibble(
    TEMP_appl_row,
    TEMP_fmt_to_apply) %>%
    # TODO? add a warning if a format isn't applied anywhere?
    mutate(TEMP_fmt_rank = row_number()) %>%
    unnest(cols = c(TEMP_appl_row)) %>%
    group_by(TEMP_appl_row) %>%
    #TODO add warning if there are rows not covered
    arrange(TEMP_appl_row, desc(.data$TEMP_fmt_rank)) %>%
    slice(1) %>%
    left_join(.data, ., by= c("TEMP_row" = "TEMP_appl_row")) %>%
    group_by(.data$TEMP_fmt_rank) %>%
    group_split()

  ## apply formatting
  dat_plus_fmt %>%
    map_dfr(function(x){

      cur_fmt <- x %>%
        pull(.data$TEMP_fmt_to_apply) %>%
        .[1] %>%
        .[[1]]

      if(is.null(cur_fmt)){
        if(!mock){
          out <- x %>%
            mutate(!!value := as.character(!!value))
        } else {
          out <- x
        }

        # Add message
        x %>%
          pull(.data$TEMP_row) %>%
          paste0(collapse = ", ") %>%
          paste("The following rows of the given dataset have no format applied to them", .) %>%
          message()
      }else{

        ## apply the formatting based on method of cur_fmt
        out <- apply_frmt(
          frmt_def = cur_fmt,
          .data = x,
          value = value,
          param = param,
          column = column,
          label = label,
          group = group,
          mock = mock
        )
      }

      out
    }) %>%
    arrange(.data$TEMP_row) %>%
    select(-starts_with("TEMP_")) ## drop TEMP_row values

}

#' Test of the frmt of the data
#'
#' @param cur_fmt current formatting
#' @param data data to test against NOTE: `TEMP_row` must be in the dataset
#' @param label label symbol should only be one
#' @param group list of the group parameters
#' @param param param symbol should only be one
#'
#' @return vector of the rows which this format could be applied to
#'
#' @importFrom stringr str_remove_all
#' @noRd
fmt_test_data <- function(cur_fmt, .data, label, group, param){

  #get filters for each column type
  grp_expr <- expr_to_filter(group, cur_fmt$group_val)
  lbl_expr <- expr_to_filter(label, cur_fmt$label_val)
  parm_expr <- expr_to_filter(param, cur_fmt$param_val)



  filter_expr <- paste(
      c(lbl_expr,grp_expr,parm_expr),
      collapse = "&"
    ) %>%
    parse_expr()

  out <- .data %>%
    filter(!!filter_expr)

  # Protect against incomplete frmt_combines
  if(is_frmt_combine(cur_fmt$frmt_to_apply[[1]])){
    complet_combo_grps <- out %>%
      select(!!!group, !!label, !!param) %>%
      distinct() %>%
      group_by(!!!group, !!label) %>%
      mutate(test = sum(!!parse_expr(parm_expr))) %>%
      filter(.data$test == length(cur_fmt$frmt_to_apply[[1]]$frmt_ls)) %>%
      ungroup()
    join_by <- c(group, label, param) %>%
      map_chr(as_label) %>%
      keep(~. != "<empty>")

    out <- complet_combo_grps %>%
      left_join(out, by = join_by, multiple = "all")
  }
  out %>%
    pull(.data$TEMP_row)

}


#' @importFrom rlang parse_expr eval_bare
all_missing <- function(cols, .data){
  paste0("is.na(.data$",cols,")", collapse = " & ") %>%
    parse_expr() %>%
    eval_bare(env = environment())
}


#' Replace values
#'
#' based on dplyr replace_with function
#' @param x Current vector
#' @param i vector of TRUE/FALSE if should be replaced
#' @param val New value tos replace with
#'
#' @noRd
replace_val <- function(x, i, val) {
  if (is.null(val)) {
    return(x)
  }

  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}



