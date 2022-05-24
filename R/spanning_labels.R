
## determine which columns to span across
# span_col_select <- function(x, data){
#   span_col_select_function <- get(paste0("span_col_select.",class(x)[1]),envir = asNamespace("tlang"))
#   span_col_select_function(x, data = data)
# }
#
# #' @importFrom tidyselect eval_select
# span_col_select.quosures <- function(x, data){
#   names(eval_select(expr(c(!!!x)), data = data))
# }
#
# span_col_select.span_structure <- function(x, data){
#   do.call('c',lapply(x$span_cols, span_col_select, data = data))
# }
#
# span_col_select.span_structures <- function(x, data){
#   do.call('c',lapply(x$span_cols, span_col_select, data = data))
# }
#




