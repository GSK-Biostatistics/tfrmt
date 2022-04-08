#' Layer tfmt objects together
#'
#' Provide utility for layering tfmt objects together. If both tmft's have
#' values, it will preferentially choose the second (y) tfmt by default.
#'
#' @param x,y tfmt objects that need to be combined
#' @param ... arguments passed to layer_tfmt_arg functions for combining different tfmt elements
#' @param preferred which tfmt should be used when layering and both have values. "x" or "y", defaults to "y".
#'
#' @examples
#'
#' tfmt_1 <- tfmt(title = "title1")
#'
#' tfmt_2 <- tfmt(title = "title2",subtitle = "subtitle2")
#'
#' layered_table_format <- layer_tfmt(tfmt_1, tfmt_2)
#'
layer_tfmt <- function(x, y, ..., preferred = "y"){

  stopifnot(is_tfmt(x))
  stopifnot(is_tfmt(y))

  if(missing(x)){
    return(y)
  }else if(missing(y)){
    return(x)
  }

  preferred <- match.arg(preferred, c("y","x"))

  args <- union(names(x), names(y))

  arg_list <- lapply(args, function(argname, x, y, ..., preferred){
    func <- get_layer_tfmt_arg_method(argname)
    func(x, y, argname, ..., preferred = preferred)
  },x=x, y = y, ..., preferred = preferred)

  names(arg_list) <- args

  do.call(tfmt, arg_list)
}

get_layer_tfmt_arg_method <- function(argname){
  tryCatch(
    get(paste0("layer_tfmt_arg.",argname),envir = asNamespace("tlang"), inherits = FALSE),
    error = function(e){ layer_tfmt_arg.default}
  )
}

layer_tfmt_arg.default<- function(x, y, arg_name, ..., preferred = "y"){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]

  if(preferred == "y"){
    preferred_arg_val <- y_arg_val
    alt_arg_val <- x_arg_val
  }else{
    preferred_arg_val <- x_arg_val
    alt_arg_val <- y_arg_val
  }

  if(is.null(preferred_arg_val)){
    alt_arg_val
  }else{
    preferred_arg_val
  }
}

layer_tfmt_arg.body_style <- function(x, y, ..., preferred = "y", override_body_style = FALSE){
  x_body_style <- x[["body_style"]][["all_fmts"]]
  y_body_style <- y[["body_style"]][["all_fmts"]]

  if(preferred == "y"){
    preferred_arg_val <- y_body_style
    alt_arg_val <- x_body_style
  }else{
    preferred_arg_val <- x_body_style
    alt_arg_val <- y_body_style
  }

  if(override_body_style){
    body_style_el <- preferred_arg_val
  }else{
    body_style_el <- unique(c(alt_arg_val, preferred_arg_val))
  }

  do.call(element_style,body_style_el)
}


