#' Define the Column Plan
#'
#' Using a seriesspan_structures, define the spanned column names, and the label to apply.
#' span_structures can be nested to allow for layered spanning headers.
#'
#' @rdname col_plan
#'
#' @param ... For a col_plan and span_structure, <[`tidy-select`][dplyr_tidy_select]> arguments,
#'            unquoted expressions separated by commas, and span_structures. Span_structures
#'            can nest additional span_structures. To use a span_structure, there can only be one
#'            defined "column" in the tfrmt.
#' @export
#' @examples
#' library(dplyr)
#'
#' col_plan(
#'  col_1,
#'  -col_last,
#'  new_col_1 = col_2,
#'  span_structure(
#'    label = "Top Label Level 1",
#'    span_structure(
#'      label = "Second Label Level 1.1",
#'      col_3, col_4
#'    ),
#'    span_structure(
#'      label = "Second Label Level 1.2",
#'      starts_with("B")
#'    ),
#'    col_5
#'  ),
#'  span_structure(
#'    label = "Top Label Level 2",
#'    col_6, col_7
#'  )
#' )
#'
col_plan <- function(...){

  ## selectively evaluate dots (only if is a span_structure)
  ## confirm contents otherwise
  dots <- as.list(substitute(substitute(...)))[-1]
  dots <- check_col_plan_dots(dots)

  ## get columns of the span structures
  span_struct_entries_locs <- sapply(dots, is_span_structure)
  if(any(span_struct_entries_locs)){
    span_struct_entries <- dots[span_struct_entries_locs]
    span_struct_dots <- lapply(span_struct_entries, get_span_structure_dots)

    ## flatten dots
    dots[span_struct_entries_locs] <- span_struct_dots
    dots <- unlist(dots)
  }else{
    span_struct_entries <- NULL
  }

  ## convert dots into a vars list (list of quosures)
  dots_as_vars <- do.call(vars, dots)

  structure(
    list(
      dots = dots_as_vars,
      span_structures = span_struct_entries
    ),
    class = c("col_plan","plan")
  )
}

#' @rdname col_plan
#'
#' @param label text label to span across the defined columns
#'
#' @export
span_structure <- function(label, ...){

  if(!(is.character(label))){
    stop("`label` must be a character vector")
  }

  span_cols <- as.list(substitute(substitute(...)))[-1]
  span_cols <- check_span_structure_dots(span_cols)

  any_dots_span_structure <- any(sapply(span_cols, is_span_structure))

  structure(
    list(
      label = label,
      span_cols = span_cols
    ),
    class = c("span_structures"[any_dots_span_structure],"span_structure")
  )
}

is_span_structure <- function(x){
  inherits(x, "span_structure")
}

is_span_structures <- function(x){
  inherits(x, "span_structures")
}

check_span_structure_dots <- function(x, envir = parent.frame()){
  lapply(x,function(x){
    if(is.name(x)){
      return(quo(!!x))
    }else if(is.call(x)){
      if(is_valid_tidyselect_call(x)){
        quo(!!x)
      }else if(is_valid_span_structure_call(x)){
        return(rlang::eval_tidy(x))
      }else{
        browser()
        stop(
          "Invalid entry: `",format(x),"`\n",
          "Only span_structures (`span_structure()`), ",
          "selection helpers (See <https://tidyselect.r-lib.org/reference>), ",
          " or unquoted expressions representing variable names ",
          " can be entered as contents.",
          " Changes the names of individual variables using new_name = old_name syntax is allowable"
          )
      }
    }else{
      stop("Unexpected entry type")
    }
  })
}

is_valid_span_structure_call <- function(x){
  as.character(as.list(x)[[1]]) %in% c("span_structure")
}

is_valid_tidyselect_call <- function(x){
  ## drop - from determining if
  if(as.character(as.list(x)[[1]]) == "-"){
    x <- x[[-1]]
    if(is.name(x)){
      return(TRUE)
    }
  }
  as.character(as.list(x)[[1]]) %in% c("starts_with","ends_with","contains","matches","num_range","all_of","any_of","everything","last_col", "where")
}

check_col_plan_dots <- check_span_structure_dots

apply_gt_spanning_labels <- function(gt_table, spanning_lab_struct){
  if(!is.null(spanning_lab_struct)){

    # get set of tab_spanner functions to apply
    spanning_lab_grps <- apply_spanning_labels( gt_table$`_data`, spanning_lab_struct)

    #loop over the tab_spanners to add to the gt table
    for(spanning_lab_apply_idx in seq_along(spanning_lab_grps)){
      spanning_lab_func <- spanning_lab_grps[[spanning_lab_apply_idx]]
      gt_table <- spanning_lab_func(gt_table)
    }
  }
  gt_table
}

apply_spanning_labels <- function(data, spanning_lab_struct){
  span_lab_groups <- list()
  for(span_lab_grp in spanning_lab_struct){

    ## do.call('c') quickly concatenates list into vector.
    span_lab_groups <- do.call('c',
      list(create_span_group(span_lab_grp, data),
      span_lab_groups
      ))
  }
  span_lab_groups
}

create_span_group <- function(x, data){
  create_span_group_function <- get(paste0("create_span_group.",class(x)[1]),envir = asNamespace("tlang"))
  create_span_group_function(x, data)
}

#' @importFrom gt tab_spanner cols_move
create_span_group.span_structure <- function(x, data){

  cols <- span_col_select(x, data = data)
  label <- format(x$label)

  list(function(gt_tab){
    gt_tab <- tab_spanner(gt_tab,label = label, columns = cols)
    if(x$order_cols){
      gt_tab <- cols_move(
        gt_tab,
        columns = cols[-1],
        after = cols[1]
      )
    }
  });
}

create_span_group.span_structures <- function(x, data){

  ## for child span_structures, create tab_spanner funcs
  child_span_structures <- x$span_cols[sapply(x$span_cols, is_span_structure)]

  ## do.call('c') quickly concatenates list into vector.
  span_structure_span_func <- do.call('c',lapply(child_span_structures, create_span_group, data = data))

  ## for parent span_structure, create tab_spanner funcs
  span_structures_span_func <- create_span_group.span_structure(x, data)

  ## combine together
  c(
    span_structure_span_func,
    span_structures_span_func
  )
}


## determine which columns to span across
span_col_select <- function(x, data){
  span_col_select_function <- get(paste0("span_col_select.",class(x)[1]),envir = asNamespace("tlang"))
  span_col_select_function(x, data = data)
}

#' @importFrom tidyselect eval_select
span_col_select.quosures <- function(x, data){
  names(eval_select(expr(c(!!!x)), data = data))
}

span_col_select.span_structure <- function(x, data){
  do.call('c',lapply(x$span_cols, span_col_select, data = data))
}

span_col_select.span_structures <- function(x, data){
  do.call('c',lapply(x$span_cols, span_col_select, data = data))
}

get_span_structure_dots <- function(x){
  get_span_structure_dots_function <- get(paste0("get_span_structure_dots.",class(x)[1]),envir = asNamespace("tlang"))
  get_span_structure_dots_function(x)
}

get_span_structure_dots.quosure <- function(x){
  x
}

get_span_structure_dots.span_structure <- function(x){
  x$span_cols
}

get_span_structure_dots.span_structures <- function(x){
  do.call('c',lapply(x$span_cols, get_span_structure_dots))
}

check_column_and_col_plan <- function(x){.
  multi_column_defined <- length(x$column) > 1
  span_structures_defined <- if(!is.null(x$col_plan)){
    !is.null(x$col_plan$span_structures)
  }else{
    FALSE
  }

  if(multi_column_defined & span_structures_defined){
    stop(
      "Multiple columns defined in `column` argument of tfrmt ",
      "as well as span_structures in `col_plan`.\n",
      "The use of only one approach is permitted. ",
      "Select a single column or remove span_structures from `col_plan()`"
    )
  }

}


