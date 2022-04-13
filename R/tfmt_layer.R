#' Layer tfrmt objects together
#'
#' Provide utility for layering tfrmt objects together. If both tmft's have
#' values, it will preferentially choose the second tfrmt by default.
#'
#' @param x,y tfrmt objects that need to be combined
#' @param ... arguments passed to layer_tfrmt_arg functions for combining different tfrmt elements
#' @param join_body_styles should the body styles be uniquely combined, or just keep styling in y
#'
#' @examples
#'
#' tfrmt_1 <- tfrmt(title = "title1")
#'
#' tfrmt_2 <- tfrmt(title = "title2",subtitle = "subtitle2")
#'
#' layered_table_format <- layer_tfrmt(tfrmt_1, tfrmt_2)
#'
layer_tfrmt <- function(x, y, ..., join_body_styles = TRUE){

  stopifnot(is_tfrmt(x))
  stopifnot(is_tfrmt(y))

  if(missing(x)){
    return(y)
  }else if(missing(y)){
    return(x)
  }

  args <- union(names(x), names(y))

  arg_list <- lapply(args, function(argname, x, y, ..., join_body_styles){
    func <- get_layer_tfrmt_arg_method(argname)
    func(x, y, argname, ..., join_body_styles = join_body_styles)
  },x=x, y = y, ..., join_body_styles = join_body_styles)

  names(arg_list) <- args

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

layer_tfrmt_arg.body_style <- function(x, y, ...,  join_body_styles = TRUE){
  x_body_style <- x[["body_style"]]
  y_body_style <- y[["body_style"]]

  if(join_body_styles){
    body_style_el <- unique(c(x_body_style, y_body_style))
  }else{
    body_style_el <- y_body_style
  }

  do.call(table_body_plan,body_style_el)
}


