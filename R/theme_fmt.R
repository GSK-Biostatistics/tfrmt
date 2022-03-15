

fmt_str <- function(expression, ..., cols = vars(), output = ".default"){
  everything_but_curly <- "(?<=\\{)([^}]*)(?=\\})"
  n_vars <- str_count(expression, everything_but_curly)
  vars_to_fmt <- str_extract_all(expression, everything_but_curly, simplify = TRUE)

  fmt_ls <- list(...)

  if(n_vars != length(fmt_ls) & length(fmt_ls) > 1){
    stop("The number of formats must be 1 or match the number of columns", call. = FALSE)
  } else if (n_vars > 1 & length(fmt_ls) == 1){
    fmt_ls <- fmt_ls[rep(1,n_vars)]
  }
  names(fmt_ls) <- vars_to_fmt

  if(output == ".default"){
    output <- vars_to_fmt[1]
  }

  structure(
    list(expression = expression, fmt_ls = fmt_ls, cols = cols, output = output),
    class = c("fmt_str")
  )
}

fmt <- function(rounding = NULL, bounds = NULL, padding = ""){
  structure(
    list(rounding = rounding, bounds = bounds, padding = padding),
    class = c("fmt")
  )

}
