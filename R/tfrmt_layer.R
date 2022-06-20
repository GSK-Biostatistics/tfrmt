#' Layer tfrmt objects together
#'
#' Provide utility for layering tfrmt objects together. If both tmft's have
#' values, it will preferentially choose the second tfrmt by default. This is an
#' alternative to piping together tfrmts
#'
#' @param x,y tfrmt objects that need to be combined
#' @param ... arguments passed to layer_tfrmt_arg functions for combining different tfrmt elements
#' @param join_body_plans should the body styles be uniquely combined, or just keep styling in y
#'
#' @export
#' @examples
#'
#' tfrmt_1 <- tfrmt(title = "title1")
#'
#' tfrmt_2 <- tfrmt(title = "title2",subtitle = "subtitle2")
#'
#' layered_table_format <- layer_tfrmt(tfrmt_1, tfrmt_2)
#'
layer_tfrmt <- function(x, y, ..., join_body_plans = TRUE){

  stopifnot(is_tfrmt(x))
  stopifnot(is_tfrmt(y))

  if(missing(x)){
    return(y)
  }else if(missing(y)){
    return(x)
  }

  args <- union(names(x), names(y))

  arg_list <- lapply(args, function(argname, x, y, ..., join_body_plans){
    func <- get_layer_tfrmt_arg_method(argname)
    func(x, y, argname, ..., join_body_plans = join_body_plans)
  },x=x, y = y, ..., join_body_plans = join_body_plans)

  names(arg_list) <- args

  ## remove null values that may have made it through
  arg_list <- arg_list[!sapply(arg_list, is.null)]

  do.call(tfrmt, arg_list)
}

get_layer_tfrmt_arg_method <- function(argname){
  tryCatch(
    get(paste0("layer_tfrmt_arg.",argname),envir = asNamespace("tlang"), inherits = FALSE),
    error = function(e){ layer_tfrmt_arg.default}
  )
}

layer_tfrmt_arg.default<- function(x, y, arg_name, ...){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]

  if(is.null(y_arg_val)){
    x_arg_val
  }else{
    y_arg_val
  }
}

## if group is an empty vars, keep the original value
layer_tfrmt_arg_vars<- function(x, y, arg_name, ...){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]

  if(is.null(y_arg_val) | identical(y_arg_val, vars())){
    x_arg_val
  }else{
    y_arg_val
  }
}

## if label/param/values/column is an empty quo, keep the original value
layer_tfrmt_arg_quo<- function(x, y, arg_name, ...){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]

  if(is.null(y_arg_val) | identical(y_arg_val, quo())){
    x_arg_val
  }else{
    y_arg_val
  }
}

layer_tfrmt_arg.group <- layer_tfrmt_arg_vars
layer_tfrmt_arg.label <- layer_tfrmt_arg_quo
layer_tfrmt_arg.param <- layer_tfrmt_arg_quo
layer_tfrmt_arg.values <- layer_tfrmt_arg_quo
layer_tfrmt_arg.column <- layer_tfrmt_arg_vars
layer_tfrmt_arg.sorting_cols <- layer_tfrmt_arg_vars


layer_tfrmt_arg.body_plan <- function(x, y, ...,  join_body_plans = TRUE){
  x_body_plan <- x[["body_plan"]]
  y_body_plan <- y[["body_plan"]]

  if(join_body_plans){
    body_plan_el <- unique(c(x_body_plan, y_body_plan))
  }else{
    body_plan_el <- y_body_plan
  }

  do.call(body_plan,body_plan_el)
}


