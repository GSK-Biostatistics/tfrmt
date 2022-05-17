#' Define the Column Spanning Headers
#'
#' Using span_structures, define the spanned column names, and the label to apply.
#' span_structures can be nested to allow for layered spanning headers.
#'
#' @rdname span_plan
#'
#' @param ... For a span_plan, this is a series of span_structure. For span_structure,
#'            this can be nested span_structure, or a definition of the columns to span across within a
#'            vars().
#' @export
#' @examples
#' library(dplyr)
#'
#' span_plan(
#'  span_structure(
#'    label = "Top Label Level 1",
#'    span_structure(
#'      label = "Second Label Level 1.1",
#'      vars(col_1, col_2)
#'    ),
#'    span_structure(
#'      label = "Second Label Level 1.2",
#'      vars(starts_with("B"))
#'    ),
#'    vars(col_4)
#'  ),
#'  span_structure(
#'    label = "Top Label Level 2",
#'    span_content = vars(col_5,col_6)
#'  )
#' )
#'
span_plan <- function(...){

  dots <- list(...)

  if(any(!sapply(dots, is_span_structure))){
    stop("All entries in span_plan must be a span_structure")
  }

  structure(
    dots,
    class = "span_plan_grp"
  )
}

#' @rdname span_plan
#'
#' @param label text label to span across the defined columns
#' @param order_cols order the columns based on the order in which they are
#'              listed in the structure. Overrides the tfrmts col_select ordering.
#'              Value defaults to `TRUE`.
#'
#' @export
span_structure <- function(label, ..., order_cols = TRUE){
  if(!(is.character(label) | is_element_label(label))){
    stop("`label` must be a character vector or element_label")
  }
  stopifnot(is.logical(order_cols))
  span_cols <- list(...)
  check_span_structure_dots(span_cols)
  any_dots_span_structure <- any(sapply(span_cols, is_span_structure))
  structure(
    list(
      label = label,
      span_cols = span_cols,
      order_cols = order_cols
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

check_span_structure_dots <- function(x){
  x <- lapply(x,function(x){
    if(!inherits(x, c("quosures","span_structure"))){
      stop("Only objects of type quosures (`var()`), or span_structure (`span_structure()`)",
           " can be entered as contents to span a label across")
    }
    invisible()
  })
  invisible()
}

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





