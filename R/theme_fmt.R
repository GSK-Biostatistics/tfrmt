
#           fmt_combine("{count} ({percent})%",
#                       count = fmt(),
#                       percent = fmt()
#           )
fmt_combine <- function(expression, ..., missing = NULL){
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




fmt_str <- function(group = NULL, label = NULL, fmt = NULL){
  structure(
    list(group = group, label = label, fmt = fmt),
    class = c("fmt_str")
  )
}

# Do we need to add any formating for txt? like the ability to convert to sentence case?
fmt <- function(rounding = NULL, bounds = NULL, padding = "", missing = NULL){
  structure(
    list(rounding = rounding, bounds = bounds, missing = missing, padding = padding),
    class = c("fmt")
  )

}
