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

#' @importFrom tidyr expand_grid
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



#' @rdname frmt
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
