#' Table Formatting
#'
#' frmt provide an abstracted way to approach to define formatting of table
#' contents. By defining in this way, the formats can be
#' layered to be more specific and general cell styling can be done first.
#'
#' @param expression a string representing the intended format. See details: expression for more
#' detailed description.
#' @param missing when a value is missing that is intended to be formatted, what value to place?
#' @param ...  these dots are for future extensions and must be empty.
#'
#' @export
#' @examples
#'
#' frmt("XXX %")
#'
#' frmt("XX.XXX")
#'
frmt <- function(expression, missing = NULL,...){
  structure(
    list(expression = expression, missing = missing),
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
#' of [table_body_plan()]
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
#' @return table_body_plan object
#' @export
#'
table_body_plan <- function(...){

  frmt_structure_list <- list(...)

  for(struct_idx in seq_along(frmt_structure_list)){
    if(!is_frmt_structure(frmt_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `frmt_structure`."))
    }
  }

  structure(
    frmt_structure_list,
    class = c("table_body_plan", "frmt_table")
  )
}

#' Row Group Plan
#'
#' @param ...  Row group structure objects separated by commas
#'
#' @return row_grp_plan object
#' @export
#'
row_grp_plan <- function(...){

  row_grp_structure_list <- list(...)

  for(struct_idx in seq_along(row_grp_structure_list)){
    if(!is_row_grp_structure(row_grp_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `row_grp_structure`."))
    }
  }

  structure(
    row_grp_structure_list,
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
