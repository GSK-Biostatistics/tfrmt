
#           fmt_combine("{count} ({percent})%",
#                       count = fmt(),
#                       percent = fmt()
#           )
frmt_combine <- function(expression, ..., missing = NULL){
  everything_but_curly <- "(?<=\\{)([^}]*)(?=\\})"
  n_vars <- str_count(expression, everything_but_curly)
  vars_to_fmt <- str_extract_all(expression, everything_but_curly, simplify = TRUE)
  fmt_ls <- list(...)

  if(n_vars != length(fmt_ls) & length(fmt_ls) > 1){
    stop("The number of formats must be 1 or match the number of parameters", call. = FALSE)
  } else if (n_vars > 1 & length(fmt_ls) == 1){
    fmt_ls <- fmt_ls[rep(1,n_vars)]
  }
  names(fmt_ls) <- vars_to_fmt

  structure(
    list(expression = expression, fmt_ls = fmt_ls, missing = missing),
    class = c("fmt_combine")
  )
}


frmt_structure <- function(group_val = NULL, label_val = NULL, ...){
  fmt <- list(...)
  if(length(fmt) > 1){
    stop("Can only handel one format per fmt_str function. Use fmt_combine if a combination is needed")
  }
  structure(
    list(group_val = group_val, label_val = label_val, fmt = fmt),
    class = c("frmt_structure")
  )
}

# Do we need to add any formating for txt? like the ability to convert to sentence case?
frmt <- function(rounding = NULL, bounds = NULL, padding = "", missing = NULL){
  structure(
    list(rounding = rounding, bounds = bounds, missing = missing, padding = padding),
    class = c("frmt")
  )

}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#' @importFrom rlang list2
frmt_when <- function(...){
  structure(
    list2(...),
    class = c("fmt_when")
  )
}
