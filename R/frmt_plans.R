#' Table Formatting
#'
#' frmt provide an abstracted way to approach to define formatting of table
#' contents. By defining in this way, the formats can be
#' layered to be more specific and general cell styling can be done first.
#'
#' @param expression a string representing the intended format. See details: expression for more
#' detailed description.
#' @param missing when a value is missing that is intended to be formatted, what value to place?
#' @param scientific a string representing the intended scientific notation to be appended to the expression
#' @param ...  these dots are for future extensions and must be empty.
#'
#' @export
#' @examples
#'
#' frmt("XXX %")
#'
#' frmt("XX.XXX")
#'
frmt <- function(expression, missing = NULL, scientific = NULL, ...){
  structure(
    list(expression = expression, missing = missing, scientific = scientific),
    class = c("frmt")
  )
}

#' Table Formatting - Combine
#'
#' frmt_combine provide an abstracted way to approach to define formatting of table
#' contents. By defining in this way, the formats can be
#' layered to be more specific and general cell styling can be done first.
#'
#' @param expression a string representing the intended combined format.
#' @param ... named frmts, where the name is the name of the param to apply the frmt to
#' @param missing when all values are missing that is intended to be formatted, what value to place
#' @export
#'
#' @examples
#'
#' frmt_combine(
#'  "{param1} {param2}",
#'  param1 = frmt("XXX %"),
#'  param2 = frmt("XX.XXX")
#' )
#'
frmt_combine <- function(expression, ..., missing = NULL){

  everything_but_curly <- "(?<=\\{)([^}]*)(?=\\})"

  n_vars <- str_count(expression, everything_but_curly)
  vars_to_fmt <- str_extract_all(expression, everything_but_curly, simplify = TRUE)
  fmt_ls <- list(...)

  if(n_vars != length(fmt_ls) & length(fmt_ls) > 1){
    stop("The number of formats must be 1 or match the number of parameters", call. = FALSE)
  } else if (n_vars > 1 & length(fmt_ls) == 1){
    fmt_ls <- fmt_ls[rep(1,n_vars)]
  }

  names(fmt_ls) <- vars_to_fmt

  structure(
    list(expression = expression, fmt_ls = fmt_ls, missing = missing),
    class = c("frmt_combine","frmt")
  )
}

#' Format Structure Object
#'
#' Function needed to create a frmt_structure object, which is a building block
#' of [body_plan()]
#'
#' @param group_val A string or a named list of strings which represent the
#'   value of group should be when the given frmt is implemented
#' @param label_val A string which represent the value of label should be when
#'   the given frmt is implemented
#' @param ... either a [frmt()], [frmt_combine()], or a [frmt_when()] object.
#'   This can be named to also specify the parameter value
#'
#' @importFrom tidyr expand_grid
#' @export
frmt_structure <- function(group_val = ".default", label_val = ".default", ...){
  param_frmt <- list(...)
  param_val <- names(param_frmt)

  if(length(param_frmt) > 1){
    stop("Can only handel one format per frmt_structure function. Use frmt_combine if a combination is needed")
  }

  if(is_frmt_combine(param_frmt[[1]])){
    param_val <- names(param_frmt[[1]]$fmt_ls)
  } else if(is.null(param_val)){
    param_val <- ".default"
  }

  if(!is_frmt(param_frmt[[1]])){
    stop(paste0("Entry is not an object of class `frmt`"))
  }


  if(is.list(group_val)){
    group_val_names <- names(group_val)
    if(is.null(group_val_names)){
      stop("when group_val is a list, must be a named list")
    }else if(any(group_val_names == "")){
      stop("when group_val is a list, each entry must be named")
    }
  }

  structure(
    list(
      group_val = group_val,
      label_val = label_val,
      param_val = param_val,
      frmt_to_apply = param_frmt),
    class = c("frmt_structure","frmt_table")
  )
}

#' Row Group Structure Object
#'
#' Function needed to create a row_grp_structure object, which is a building block
#' of [row_grp_plan()]
#'
#' @param group_val A string or a named list of strings which represent the
#'   value of group should be when the given frmt is implemented
#' @param ... element_block() object
#'
#' @importFrom tidyr expand_grid
#' @export
row_grp_structure <- function(group_val = ".default", ...){
  row_grp_block <- list(...)

  if(length(row_grp_block) > 1){
    stop("Can only handle one format per row_grp_structure function")
  }

  if(is.list(group_val)){
    group_val_names <- names(group_val)
    if(is.null(group_val_names)){
      stop("when group_val is a list, must be a named list")
    }else if(any(group_val_names == "")){
      stop("when group_val is a list, each entry must be named")
    }
  }

  structure(
    list(
      group_val = group_val,
      block_to_apply = row_grp_block),
    class = c("row_grp_structure","frmt_table")
  )
}

#' Table Body Plan
#'
#' @param ... list of frmt_structure
#'
#' @return body_plan object
#' @export
#'
body_plan <- function(...){

  frmt_structure_list <- list(...)

  for(struct_idx in seq_along(frmt_structure_list)){
    if(!is_frmt_structure(frmt_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `frmt_structure`."))
    }
  }

  structure(
    frmt_structure_list,
    class = c("body_plan", "frmt_table")
  )
}

#' Row Group Plan
#'
#' @param ...  Row group structure objects separated by commas
#' @param label_loc Boolean specifying whether or not the top-level group should be a spanning label
#'
#' @return row_grp_plan object
#' @export
#'
row_grp_plan <- function(..., label_loc = element_row_grp_loc(location = "indented")){

  row_grp_structure_list <- list(...)

  for(struct_idx in seq_along(row_grp_structure_list)){
    if(!is_row_grp_structure(row_grp_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `row_grp_structure`.
                  If you want specify `spanning_label` please enter 'spanning_label ='"))
    }
  }

  structure(
    list(struct_ls = row_grp_structure_list, label_loc = label_loc),
    class = c("row_grp_plan", "frmt_table")
  )
}

#' @rdname frmt
#' @export
#' @importFrom rlang list2 f_rhs f_rhs<-
#' @importFrom purrr map
frmt_when <- function(...){
  frmts <- list2(...)

  frmts_eval <-frmts %>%
    map(function(x){
      f_rhs(x) <- eval(f_rhs(x))
      x
    })

  structure(
    frmts_eval,
    class = c("frmt_when","frmt")
  )
}


#' Column Alignment Plan
#'
#' @param ... list of frmt_structure
#'
#' @return table_body_plan object
#' @export
#'
col_align_plan <- function(...){

  element_align_list <- list(...)

  for(el_idx in seq_along(element_align_list)){
    if(!is_element_align(element_align_list[[el_idx]])){
      stop(paste0("Entry number ",el_idx," is not an object of class `element_align`."))
    }
  }

  structure(
    element_align_list,
    class = c("col_align_plan", "frmt_table")
  )
}


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
  #Add the new spanning columns to the dots.

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

  ##TODO: check for duplicate variable calls?

  structure(
    list(
      dots = dots_as_vars,
      span_structures = span_struct_entries
    ),
    class = c("col_plan","plan")
  )
}


