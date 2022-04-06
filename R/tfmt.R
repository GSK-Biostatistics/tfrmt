#' Table Format
#'
#' tfmt, or "table format" is a way to pre-define the non-data components
#' of your tables, and how the data will be handled once added: i.e. title, footers,
#' headers, span headers, and cell formats. In addition, tfmt's can be layered,
#' building from one table format to the next. For cases where only one value
#' can be used, the newest
#'
#' @param ... values for new_tfmt, a prior tfmt
#'
#' @details
#'
#' @rdname tfmt
#'
#' @export
tfmt <- function(...){

  arg_list <- list(...)
  old_tfmt_loc <- do.call('c',lapply(arg_list, is_tfmt))

  if(any(old_tfmt_loc)){
    old_tfmt <- arg_list[[which(old_tfmt_loc)]]
    pass_through_args <- arg_list[!old_tfmt_loc]
    layer_tfmt(
      old_tfmt,
      do.call(tfmt, pass_through_args)
    )
  }else{

    tfmt(...)
  }
}

#' @rdname tfmt
#' @export
#'
new_tfmt <- function(
  group = vars(),
  label = vars(),
  param = vars(),
  values = vars(),
  column = vars(),
  title,
  title_txt_style,
  subtitle,
  subtitle_txt_style,
  col_label_txt_style,
  spanning_label_txt_style,
  footer,
  footer_txt_style,
  txt_style,
  row_grp_txt_style, # style of the grouping level
  row_txt_style,#Style of the rest of the rows
  row_grp_style, #the style between blocking
  body_style,
  body_txt_style,
  body_align,
  sorting_cols,
  page_vars,
  row_grp, # col which is used to make the row grps
  col_labels,
  col_widths,
  spanning_label_grp,
  col_select,
  ...
  ){

  elements <- tfmt_find_args(...)

  structure(
    elements,
    class = c("tfmt")
  )

}

layer_tfmt <- function(fmt1, fmt2){

  args <- union(names(fmt1), names(fmt2))

  arg_list <- lapply(args, function(argname, fmt1, fmt2){
    layer_tfmt_arg(fmt1, fmt2, argname)
  },fmt1=fmt1, fmt2 = fmt2)

  names(arg_list) <- args

  do.call(tfmt, arg_list)
}

layer_tfmt_arg <- function(x, y, argname){
  func <- get_layer_tfmt_arg(argname)
  func(x, y, argname)
}

get_layer_tfmt_arg <- function(argname){

  if(argname == "body_style"){
    layer_tfmt_arg.body_style
  }else{
    layer_tfmt_arg.default
  }
}

layer_tfmt_arg.default<- function(x, y, arg_name, ...){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]
  if(is.null(y_arg_val)){
    x_arg_val
  }else{
    y_arg_val
  }
}

layer_tfmt_arg.body_style <- function(x, y, ...){
  x_body_style <- x[["body_style"]][["all_fmts"]]
  y_body_style <- y[["body_style"]][["all_fmts"]]

  do.call(element_style,unique(c(x_body_style, y_body_style)))
}

tfmt_find_args <- function(..., env = parent.frame()){
  args <- names(formals(sys.function(sys.parent(1))))
  vals <- mget(args, envir = env)
  vals <- vals[!sapply(vals, is_missing)]
  new_args <- list(..., ... = NULL)

  for (i in names(new_args)){
    vals[[i]] <- new_args[[i]]
  }
  vals
}

is_tfmt <- function(x){
  inherits(x, "tfmt")
}

is_missing <- function(x){
  identical(x, quote(expr = ))
}
