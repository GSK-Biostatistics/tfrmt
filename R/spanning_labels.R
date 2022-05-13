#' Define the Column Spanning Headers
#'
#' Using span_frmts, define the spanned column names, and the label to apply.
#' span_frmts can be nested to allow for layered spanning headers.
#'
#' @rdname span_structure
#'
#' @param ... for a span_structure, this is a series of span_frmt. for span_frmt,
#'            this can be nested span_frmt, or a definition of the columns to span across within a
#'            vars().
#' @export
#' @examples
#'
#' span_structure(
#'  span_frmt(
#'    label = "Top Label Level 1",
#'    span_frmt(
#'      label = "Second Label Level 1.1",
#'      vars(col_1, col_2)
#'    ),
#'    span_frmt(
#'      label = "Second Label Level 1.2",
#'      vars(starts_with("B"))
#'    ),
#'    vars(col_4)
#'  ),
#'  span_frmt(
#'    label = "Top Label Level 2",
#'    span_content = vars(col_5,col_6)
#'  )
#' )
#'
span_structure <- function(...){

  dots <- list(...)

  if(any(!sapply(dots, is_span_frmt))){
    stop("All entries in span_structure must be a span_frmt")
  }

  structure(
    dots,
    class = "span_struct_grp"
  )
}

#' @rdname span_structure
#'
#' @param label text label to span across the defined columns
#'
#' @export
span_frmt <- function(label, ...){
  stopifnot(is.character(label))
  span_cols <- list(...)
  check_span_frmt_dots(span_cols)
  any_dots_span_frmt <- any(sapply(span_cols, is_span_frmt))
  structure(
    list(
      label = label,
      span_cols = span_cols
    ),
    class = c("span_frmts"[any_dots_span_frmt],"span_frmt")
  )
}

is_span_frmt <- function(x){
  inherits(x, "span_frmt")
}

is_span_frmts <- function(x){
  inherits(x, "span_frmts")
}

check_span_frmt_dots <- function(x){
  x <- lapply(x,function(x){
    if(!inherits(x, c("quosures","span_frmt"))){
      stop("Only objects of type quosures (`var()`), or span_frmt (`span_frmt()`)",
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

#' @importFrom gt tab_spanner
create_span_group.span_frmt <- function(x, data){
  cols <- span_col_select(x, data = data)
  list(function(gt_tab){
    tab_spanner(gt_tab,label = x$label, columns = cols)
  })
}

create_span_group.span_frmts <- function(x, data){

  ## for child span_frmts, create tab_spanner funcs
  child_span_frmts <- x$span_cols[sapply(x$span_cols, is_span_frmt)]
  span_frmt_span_func <- do.call('c',lapply(child_span_frmts, create_span_group, data = data))

  ## for parent span_frmt, create tab_spanner funcs
  span_frmts_span_func <- create_span_group.span_frmt(x, data)

  ## combine together
  c(
    span_frmt_span_func,
    span_frmts_span_func
  )
}


## determine which columns to span across
span_col_select <- function(x, data){
  span_col_select_function <- get(paste0("span_col_select.",class(x)[1]),envir = asNamespace("tlang"))
  span_col_select_function(x, data = data)
}

span_col_select.quosures <- function(x, data){
  tidyselect::eval_select(expr(c(!!!x)), data = data)
}

span_col_select.span_frmt <- function(x, data){
  do.call('c',lapply(x$span_cols, span_col_select, data = data))
}

span_col_select.span_frmts <- function(x, data){
  do.call('c',lapply(x$span_cols, span_col_select, data = data))
}





