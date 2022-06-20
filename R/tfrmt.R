#' Table Format
#'
#' tfrmt, or "table format" is a way to pre-define the non-data components of
#' your tables, and how the data will be handled once added: i.e. title,
#' footers, headers, span headers, and cell formats. In addition, tfrmt's can be
#' layered, building from one table format to the next. For cases where only one
#' value can be used, the newly defined tfrmt accepts the latest tfrmt
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
#' @param col_plan a col_plan object which is used to select, rename, and nest
#'   columns
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
  args <- setdiff(arg_parent,c("tfrmt_obj","..."))

  ## get the values from the parent env. turn the
  ## as_var_args call into vars
  ## and as_quo_args into length one quo's
  vals <- quo_get(
    args,
    as_var_args = c("group","column","sorting_cols"),
    as_quo_args = c("label","param","values"),
    envir = env
    )

  ## remove the "missing" values from vals
  vals <- vals[!sapply(vals, is_missing)]

  dot_subs <- as.list(substitute(substitute(...)))[-1]
  for(dot_name in names(dot_subs)){
    compare_dot_args_against_formals(dot_arg = dot_name, formals = args)
    vals[[dot_name]] <- tryCatch(
      eval(dot_subs[[dot_name]], envir = env),
      error = function(e){
        stop(e$message,call. = FALSE)
      }
    )
  }

  vals
}

#' @importFrom rlang abort frame_call
#' @importFrom dplyr vars
#' @importFrom purrr safely
quo_get <- function(args, as_var_args = c(), as_quo_args = c(), envir = parent.frame()){

  arg_set <- lapply(args, function(arg){

    ## try to get arg call
    arg_call <- do.call('substitute',list(as.symbol(arg)), envir = envir)

    if(missing(arg_call)){

      ## args not defined can quietly return empty expressions.
      return(quote(expr = ))

    }else{

      ## try to safely evaluate arg call
      arg_call_results <-  safely(eval)(arg_call, envir = envir)

      ## if expression evaluation was successful, move forward to confirming is correct class and returning the value
      if(is.null(arg_call_results$error)){

        if(arg %in% c(as_var_args, as_quo_args)){
          ## for arg_var_args, we expect not a function. this means arguments can be
          ## entered such as `col`. convert into final forms respectively
          if(!(is.function(arg_call_results$result) | is_basic_list(arg_call_results$result))){
            if(arg %in% as_var_args){
              return(as_vars(arg_call_results$result))
            }else{
              return(as_length_one_quo(arg_call_results$result, arg = as.character(arg)))
            }
          }
        }else{
          ## return value as normal if not a var or quo arg
          return(arg_call_results$result)
        }
      }

      ## Now assume expression evaluation was a failure/returned incorrect value

      ## if not a var or quo and failed, return informative error message
      if(arg %in% c(as_var_args, as_quo_args)){

        arg_call_list <- as.list(arg_call)

        ## minor clean up to remove leading function call in arg_call_list
        ## if the arg_call_list is length > 1
        if(length(arg_call_list) > 1){
          arg_call_list <- arg_call_list[-1]
        }

        if(arg %in% as_var_args){
          check_var_arg_call_valid(arg_call_list, arg)
          arg_val <- as_vars(do.call('vars',arg_call_list, envir = envir))

        }else{
          arg_val <-as_length_one_quo(do.call('vars', arg_call_list, envir = envir), arg = as.character(arg))
        }

        return(arg_val)

      }else{
        abort(
          paste0(
            "Error in evaluating argument `",
            arg,
            "`:\n",
            paste0(" ", arg_call_results$error, collapse = "")
          ),
          call = frame_call(frame = envir)
        )
      }
    }

  })

  names(arg_set) <- args
  arg_set
}


check_var_arg_call_valid <- function(var_list, arg){

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
}

#' @importFrom rlang is_quosures
is_basic_list <- function(x){
  is.list(x) & !is_quosures(x)
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

as_vars.quosure <- function(x){
  vars(!!x)
}

as_vars.character <- function(x){
  do.call(vars,lapply(x,function(x){ quo(!!sym(x))}))
}

#' @importFrom rlang inform
compare_dot_args_against_formals <- function(dot_arg, formals){
  arg_message <- paste0("Argument '",dot_arg,"' passed to tfrmt is not a recognized argument.")
  fuzzy_arg_match <- agrep(dot_arg, formals, ignore.case = TRUE, value = TRUE, max.distance = .25)
  if(length(fuzzy_arg_match)){
    arg_message <- paste0(arg_message,"\n","Did you intend to use the argument `",fuzzy_arg_match[[1]],"`?")
  }
  inform(arg_message, class = "tfrmt_unrecognized_argument_inform")
}
