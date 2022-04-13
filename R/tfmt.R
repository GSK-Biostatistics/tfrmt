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
#' @param title_txt_style the styling of the table title
#' @param subtitle subtitle of the table
#' @param subtitle_txt_style the styling of the table subtitle
#' @param col_label_txt_style the styling of the headers of the table
#' @param spanning_label_txt_style the styling of the column spanners of the table
#' @param footer footers of the table
#' @param footer_txt_style style of the footers of the table
#' @param txt_style style of the text contents of the table
#' @param row_grp_txt_style style of the grouping level row headers
#' @param row_txt_style style of the row headers
#' @param row_grp_style style of the row groups blocking
#' @param body_style combination and formatting of the input data [look at renaming?]
#' @param body_txt_style style of the cell text contents of the table
#' @param col_align how to align columns? left, right, decimal
#' @param sorting_cols which columns determine sorting of output
#' @param page_vars which colums determing paging of table (splitting)
#' @param row_group which columns deterimine row groups
#' @param col_labels values to display for the columns in the dataset
#' @param col_widths special column widths. otherwise fits contents
#' @param spanning_label_grp named list detailing which columns are spanned by what text
#' @param col_select which columns to display. defaults to everything(). uses tidyselect semantics
#' @param ... These dots are for future extensions and must be empty.
#'
#'
#' @details
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
  column = quo(),
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
  col_align,
  sorting_cols,
  page_vars,
  row_group, # col which is used to make the row grps
  col_labels,
  col_widths,
  spanning_label_grp,
  col_select,
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

  new_tfrmt

}

is_tfrmt <- function(x){
  inherits(x, "tfrmt")
}

tfrmt_find_args <- function(..., env = parent.frame()){

  arg_parent <- names(formals(sys.function(sys.parent(1))))

  args <- setdiff(arg_parent,"tfrmt_obj")
  vals <- mget(args, envir = env)

  vals <- vals[!sapply(vals, is_missing)]

  vals[["group"]] <- as_vars(vals[["group"]])

  sub_quosures <- intersect(c("label","param","values","column"), names(vals))
  for(sub_quo in sub_quosures){
    vals[[sub_quo]] <-as_length_one_quo(vals[[sub_quo]])
  }

  new_args <- list(..., ... = NULL)

  for (i in names(new_args)){
    vals[[i]] <- new_args[[i]]
  }
  vals
}

is_missing <- function(x){
  identical(x, quote(expr = ))
}

as_length_one_quo <- function(x){
  UseMethod("as_length_one_quo",x)
}

as_length_one_quo.quosure <- function(x){
  x
}

as_length_one_quo.quosures <- function(x){
  if(length(x) == 0){
    quo()
  }else{
    x[[1]]
  }
}

as_length_one_quo.character <- function(x){
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
