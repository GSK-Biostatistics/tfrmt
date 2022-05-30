#' Table Format
#'
#' tfrmt, or "table format" is a way to pre-define the non-data components
#' of your tables, and how the data will be handled once added: i.e. title, footers,
#' headers, span headers, and cell formats. In addition, tfrmt's can be layered,
#' building from one table format to the next. For cases where only one value
#' can be used, the newly defined tfrmt accepts the latest tfrmt
#'
#' @param tfrmt_obj a tfrmt object to base this new format off of
#' @param group what are the grouping vars of the input dataset
#' @param label what is the label column of the input dataset
#' @param param what is the param column of the input dataset
#' @param values what is the values column of the input dataset
#' @param column what is the column names column in the input dataset
#' @param title title of the table
#' @param subtitle subtitle of the table
#' @param footer footers of the table
#' @param row_grp_plan plan of the row groups blocking. Takes a `row_grp_plan`
#' @param body_plan combination and formatting of the input data
#' @param col_align how to align columns? left, right, decimal
#' @param sorting_cols which columns determine sorting of output
#' @param page_vars which column determine paging of table (splitting)
#' @param row_group which columns determine row groups
#' @param col_plan a col_plan object which is used to select, rename, and nest columns
#' @param col_widths special column widths. otherwise fits contents
#' @param ... These dots are for future extensions and must be empty.
#'
#'
#'
#' @rdname tfrmt
#'
#' @export
tfrmt <- function(
  tfrmt_obj,
  group = vars(),
  label = quo(),
  param = quo(),
  values = quo(),
  column = vars(),
  title,
  subtitle,
  footer,
  row_grp_plan, #the style between blocking
  body_plan,
  col_align,
  sorting_cols,
  page_vars,
  row_group, # col which is used to make the row grps
  col_plan,
  col_widths,
  ...
  ){

  tfrmt_el <- tfrmt_find_args(...)

  new_tfrmt <- structure(
    tfrmt_el,
    class = c("tfrmt")
  )

  if(!missing(tfrmt_obj)){
    new_tfrmt <- layer_tfrmt(
      tfrmt_obj,
      new_tfrmt
    )
  }

  ## check to confirm user has not defined multiple columns and
  ## any span_structures in col_plan
  check_column_and_col_plan(new_tfrmt)

  new_tfrmt

}

is_tfrmt <- function(x){
  inherits(x, "tfrmt")
}

tfrmt_find_args <- function(..., env = parent.frame()){

  ## get args of parent function
  arg_parent <- names(formals(sys.function(sys.parent(1))))
  ## don't try to get the tftmt obj
  args <- setdiff(arg_parent,"tfrmt_obj")

  ## get the values from the parent env. turn the
  ## as_var_args call into vars
  ## and as_quo_args into length one quo's
  vals <- quo_get(
    args,
    as_var_args = c("group","column"),
    as_quo_args = c("label","param","values"),
    envir = env
    )

  ## remove the "missing" values from vals
  vals <- vals[!sapply(vals, is_missing)]

  ## preserve the values included in ... input
  new_args <- list(..., ... = NULL)
  for (i in names(new_args)){
    vals[[i]] <- new_args[[i]]
  }

  vals
}

#' @importFrom rlang abort
#' @importFrom dplyr vars
quo_get <- function(args, as_var_args = c(), as_quo_args = c(), envir = parent.frame()){
  arg_set <- lapply(args, function(arg){
    tryCatch({
        if(arg %in% as_var_args){
          arg_val <- tryCatch(
            get(arg, envir = envir, inherits = FALSE),
            error = function(e){
              var_list <- as.list(do.call('substitute',list(as.symbol(arg)), envir = envir))
              if(length(var_list) > 1){
                var_list <- var_list[-1]
              }
              var_list_is_name <- sapply(var_list, is.name)
              if(!all(var_list_is_name)){
                new_arg_call <- paste0(
                  "vars(",paste(sapply(var_list, as.character),collapse = ","),")"
                )
                abort(
                  paste0(
                    "Entries for `",
                    arg,
                    "` argument must be vars(), a character vector, or unquoted column name.\n",
                    "  Consider updating the argument input to `",
                    arg,
                    "` to:\n\t",
                    new_arg_call
                  ),
                  class = c("group_vars_error")
                )
              }
              do.call('vars',var_list, envir = envir)
            })
          as_vars(arg_val)
        }else if(arg %in% as_quo_args){
          arg_val <- tryCatch(
            get(arg, envir = envir, inherits = FALSE),
            error = function(e){
              var_list <- as.list(do.call('substitute',list(as.symbol(arg)), envir = envir))
              if(length(var_list) > 1){
                var_list <- var_list[-1]
              }
              do.call('vars',var_list, envir = envir)
            })
          as_length_one_quo(arg_val, arg = as.character(arg))
        }else{
          get(arg, envir = envir,inherits = FALSE)
        }
      },error = function(e){
        # This might be too common, but it was stopping valid errors from getting through
        if(!str_detect(e$message, "missing")){
          stop(e)
        }else{
          quote(expr = )
        }
      }
    )

  })

  names(arg_set) <- args
  arg_set
}

is_missing <- function(x){
  identical(x, quote(expr = ))
}

as_length_one_quo <- function(x, ...){
  UseMethod("as_length_one_quo",x)
}

as_length_one_quo.quosure <- function(x, ...){
  x
}

#' @importFrom rlang warn
as_length_one_quo.quosures <- function(x, ..., arg = NULL){
  if(length(x) == 0){
    quo()
  }else{
    if(length(x) > 1){
      warn(
        paste0(
          "Passed more than one quosure to the argument `",
          arg,
          "`. Selecting the first entry."
        ),
        class = "quo_greater_length_one"
      )
    }
    x[[1]]
  }
}

as_length_one_quo.character <- function(x, ...){
  quo(!!sym(x))
}

as_vars <-  function(x){
  UseMethod("as_vars",x)
}

as_vars.quosures <- function(x){
  x
}

as_vars.character <- function(x){
  do.call(vars,lapply(x,function(x){ quo(!!sym(x))}))
}
