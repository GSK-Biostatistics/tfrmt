## ---------------------------------------
## determine which columns to span across
## ---------------------------------------
eval_tidyselect_on_colvec <- function(x, column_vec){
  UseMethod("eval_tidyselect_on_colvec", x)
}

#' @importFrom tidyselect eval_select
#' @importFrom rlang !!! as_label
#' @importFrom dplyr expr
#' @importFrom purrr keep
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

