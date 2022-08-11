#' Define the Column Plan & Span Structures
#'
#' Using <[`tidy-select`][dplyr_tidy_select]> expressions and a series
#' span_structures, define the order of the columns. The selection follows "last
#' selected" principals, meaning columns are moved to the _last_ selection as
#' opposed to preserving the first location.
#'
#'
#' @details
#'
#'#'
#' ## Column Selection
#'
#' When col_plan gets applied and is used to create the output table, the
#' underlying logic sorts out which column specifically is being selected. If a column
#' is selected twice, the _last_ instance in which the column is selected will be
#' the location it gets rendered.
#'
#' Avoid beginning the \code{col_plan()} column selection with a deselection (ie
#' \code{col_plan(-col1)}, \code{col_plan(-starts_with("value")))}. This will
#' result in the table preserving all columns not "de-selected" in the
#' statement, and the order of the columns not changed. It is preferred when
#' creating the \code{col_plan()} to identify all the columns planned on
#' preserving in the order they are wished to appear, or if
#' <[`tidy-select`][dplyr_tidy_select]> arguments - such as
#' \code{\link[dplyr]{everything}}- are used, identify the de-selection after
#' the positive-selection.
#'
#' Alternatively, once the gt table is produced, use the
#' \code{\link[gt]{cols_hide}} function to remove un-wanted columns.
#'
#'
#' @rdname col_plan
#'
#' @param ... For a col_plan and span_structure,
#'   <[`tidy-select`][dplyr_tidy_select]> arguments, unquoted expressions
#'   separated by commas, and span_structures. span_structures must have the
#'   arguments named to match the name the column in the input data has to identify the correct columns. See the examples
#' @param .drop Boolean. Should un-listed columns be dropped from the data.
#'   Defaults to FALSE.
#'
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' ## select col_1 as the first column, remove col_last, then create spanning
#' ## structures that have multiple levels
#' ##
#' ## examples also assume the tfrmt has the column argument set to c(c1, c2, c3)
#' ##
#' spanning_col_plan_ex <- col_plan(
#'  col_1,
#'  -col_last,
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c2 = "Second Label Level 1.1",
#'    c3 = c(col_3, col_4)
#'  ),
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c2 = "Second Label Level 1.2",
#'    c3 = starts_with("B")
#'    ),
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c3 = col_5
#'  ),
#'  span_structure(
#'    c2 = "Top Label Level 2",
#'    c3 = c(col_6, col_7)
#'  )
#' )
#'
#' ## select my_col_1 as the first column, then
#' ## rename col_2 to new_col_1 and put as the
#' ## second column, then select the rest of the columns
#' renaming_col_plan_ex <- col_plan(
#'    my_col_1,
#'    new_col_1 = col_2,
#'    everything()
#'  )
#'
#' renaming_col_plan_ex2 <- col_plan(
#'    my_col_1,
#'    new_col_1 = col_2,
#'    span_structure(
#'     c1 = c(`My Favorite span name` = "Top Label Level 1"),
#'     c3 = c(`the results column` = col_5)
#'    )
#'  )
#'
col_plan <- function(..., .drop = FALSE){

  ## selectively evaluate dots (only if is a span_structure)
  ## confirm contents otherwise
  dots <- as.list(substitute(substitute(...)))[-1]
  dots <- check_col_plan_dots(dots)

  structure(
    list(
      dots = dots,
      .drop = .drop
    ),
    class = c("col_plan","plan")
  )
}

#' @rdname col_plan
#'
#' @export
span_structure <- function(...){

  span_cols <- as.list(substitute(substitute(...)))[-1]
  span_cols <- check_span_structure_dots(span_cols)

  structure(
    span_cols,
    class = c("span_structure")
  )
}

is_span_structure <- function(x){
  inherits(x, "span_structure")
}

#' @importFrom rlang eval_tidy
check_span_structure_dots <- function(x){

  x_names <- names(x)

  if(is.null(x_names) | any(x_names == "")){
    abort(
      paste0("Entries of a span_stucture must be named:\n ",format(caller_call())),
      call = caller_call()
    )
  }

  x_dots <- x %>%
    map(~lapply(trim_vars_quo_c(.x),function(x){

    if(is.name(x)){
      if(identical(as_label(x), "<empty>")){
        return(NULL)
      }else{
        return(quo(!!x))
      }
    }else if(is.call(x)){
      if(is_valid_tidyselect_call(x)){
        quo(!!x)
      } else if(is_valid_tfrmt_col_plan_call(x)){
        browser()
        quo(q)
      }else if(is_valid_quo_call(x)){
        return(eval_tidy(x))
      }else{
        abort(
          message = paste0(
            "Invalid entry: `",format(x),"`\n",
          "Only span_structures (`span_structure()`), ",
          "selection helpers (See <https://tidyselect.r-lib.org/reference>), ",
          " or unquoted expressions representing variable names ",
          " can be entered as contents.",
          " Changing the names of individual variables using new_name = old_name syntax is allowable"
          ),
          call = caller_call()
        )
      }
    }else if(is.character(x)){
      return(as_length_one_quo.character(x))
    }else{
      abort("Unexpected entry type in span_structure()",
            call = caller_call())
    }
  }))

  x_dots[!sapply(x_dots, is.null)]
}

is_valid_span_structure_call <- function(x){
  as.character(as.list(x)[[1]]) %in% c("span_structure")
}

#' @importFrom tidyselect vars_select_helpers
is_valid_tidyselect_call <- function(x){
  ## drop - from determining if
  if(as.character(as.list(x)[[1]]) == "-"){
    x <- x[[-1]]
    if(is.name(x)){
      return(TRUE)
    }
  }
  as.character(as.list(x)[[1]]) %in% c(names(vars_select_helpers))
}

is_valid_quo_call <- function(x){
  ## drop - from determining if
  if(as.character(as.list(x)[[1]]) == "-"){
    x <- x[[-1]]
    if(is.name(x)){
      return(TRUE)
    }
  }
  as.character(as.list(x)[[1]]) %in% c("vars","quo")
}

check_col_plan_dots <- function(x){

  lapply(x,function(x){
    if(is.name(x)){
      if(identical(as_label(x), "<empty>")){
        return(NULL)
      }else{
        return(quo(!!x))
      }
    }else if(is.call(x)){
      if(is_valid_tidyselect_call(x)){
        quo(!!x)
      }else if(is_valid_quo_call(x) | is_valid_span_structure_call(x)){
        return(eval_tidy(x))
      }else{
        stop(
          "Invalid entry: `",format(x),"`\n",
          "Only span_structures (`span_structure()`), ",
          "selection helpers (See <https://tidyselect.r-lib.org/reference>), ",
          " or unquoted expressions representing variable names ",
          " can be entered as contents.",
          " Changing the names of individual variables using new_name = old_name syntax is allowable",
          call. = FALSE
        )
      }
    }else if(is.character(x)){
      return(as_length_one_quo.character(x))
    }else{
      stop("Unexpected entry type in span_structure()")
    }
  })
}

## -----------------------------------------------
## get the span cols entries from a span structure
## We need to keep this
## -----------------------------------------------
get_span_structure_dots <- function(x){
  get_span_structure_dots_function <- get(paste0("get_span_structure_dots.",class(x)[1]),envir = asNamespace("tfrmt"))
  get_span_structure_dots_function(x)
}

get_span_structure_dots.quosure <- function(x){
  x
}

get_span_structure_dots.quosures <- get_span_structure_dots.quosure

get_span_structure_dots.span_structure <- function(x){
  x$span_cols %>% unlist()
}

get_span_structure_dots.span_structures <- function(x){
  do.call('c',lapply(x$span_cols, get_span_structure_dots))
}





## -----------------------------------------------
## When we have span structures in the col_plan,
## edit data to add in columns with the spanners to allow
## us to have consistent behaviour across multi-column
## dfs and span_structures
##-----------------------------------------------
#' @importFrom purrr quietly
apply_span_structures_to_data <- function(tfrmt_obj, x){

  ## create temp df with columns based on
  tmp_df_name_vec <- x %>%
    pull(!!(tfrmt_obj$column[[1]])) %>%
    unique()

  ## create df of span structures
  span_struct_df <- tfrmt_obj$col_plan$span_structures %>%
    map_dfr(span_struct_to_df, tmp_df_name_vec) %>%
    relocate(.data$.original_col, .after = last_col()) %>%
    rename(!!as_label(tfrmt_obj$column[[1]]) := ".original_col") %>%
    select(-.rename_col)

  ## merge together data and span df on column arrange
  left_join(x,
            span_struct_df,
            by = as_label(tfrmt_obj$column[[1]])) %>%
    select(starts_with(.tlang_struct_col_prefix),
           !!(tfrmt_obj$column[[1]]),
           everything())
}

##----------------------------------------------------
## convert span_structure to a data.frame representation
##----------------------------------------------------

span_struct_to_df <- function(span_struct, data_col, depth = 1){
  span_struct_to_df_function <- get(paste0("span_struct_to_df.",class(span_struct)[1]),envir = asNamespace("tfrmt"))
  span_struct_to_df_function(span_struct=span_struct, data_col = data_col, depth = depth)
}

#' @importFrom rlang %||%
span_struct_to_df.span_structure <- function(span_struct, data_col, depth = 1){

  lab <- span_struct$label

  contents <- eval_tidyselect_on_colvec(span_struct, data_col)

  new_names <- names(contents) %||% contents

  tibble(
    lab = lab,
    .original_col = contents,
    .rename_col = new_names
  ) %>%
    rename(
      !!paste0(.tlang_struct_col_prefix,depth) := lab
    )
}

span_struct_to_df.span_structures <- function(span_struct, data_col, depth = 1){


  lab <- span_struct$label
  span_across <- span_struct$span_cols

  span_across %>%
    map_dfr(function(x) {
      if (is_span_structure(x)) {
        span_struct_to_df(span_struct = x, data_col, depth = depth + 1)
      } else{
        contents <- eval_tidyselect_on_colvec(x, data_col)
        tibble(.original_col = contents, .rename_col = names(x))
      }
    }) %>%
    mutate(lab = lab) %>%
    rename(!!paste0(.tlang_struct_col_prefix, depth) := lab) %>%
    relocate(!!paste0(.tlang_struct_col_prefix, depth), .before = 1)

}











