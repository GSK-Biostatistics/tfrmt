## ---------------------------------------
## determine which columns to span across
## ---------------------------------------
eval_tidyselect_on_colvec <- function(x, column_vec){
  UseMethod("eval_tidyselect_on_colvec", x)
  # span_col_select_function <- get(paste0("eval_tidyselect_on_colvec.",class(x)[1]),envir = asNamespace("tfrmt"))
  # span_col_select_function(x, column_vec = column_vec)
}

#' @importFrom tidyselect eval_select
#' @importFrom rlang !!!
#' @importFrom dplyr expr
eval_tidyselect_on_colvec.quosures <- function(x, column_vec){
  names(column_vec) <- column_vec
  avli_x <- x %>%
    keep(~as_label(.) %in% column_vec)

  names(eval_select(expr(c(!!!avli_x)), data = column_vec))
}

#' @importFrom tidyselect eval_select
#' @importFrom rlang !!
#' @importFrom dplyr expr
eval_tidyselect_on_colvec.quosure <- function(x, column_vec){

  names(column_vec) <- column_vec


  names(eval_select(expr(c(!!x)), data = column_vec))
}

