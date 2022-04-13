# Do we need to add any formating for txt? like the ability to convert to sentence case?
#' @rdname frmt

frmt <- function(expression, padding = "", missing = NULL, ...){
  structure(
    list(expression = expression, missing = missing, padding = padding, ...),
    class = c("frmt")
  )

}

#' @rdname frmt
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
    class = c("frmt_combine","frmt")
  )
}

#' @importFrom tidyr expand_grid
#' @rdname frmt
frmt_structure <- function(group_val = ".default", label_val = ".default", ...){

  param_frmt_list <- list(...)
  param_val <- names(param_frmt_list)
  if(length(param_frmt_list) > 1){
    stop("Can only handel one format per fmt_str function. Use fmt_combine if a combination is needed")
  }

  if(is.null(param_val)){
    param_val <- rep("", length(param_frmt_list))
    names(param_frmt_list) <- param_val
  }


  if(sum(param_val %in% c("",".default")) > 1){
    stop(paste(c(
      "Can only handle one default frmt in frmt_structure(). Passed Values:\n",
      capture.output({print(param_frmt_list)})
      ),collapse = "\n"))
  }

  for(param_frmt_idx in seq_along(param_frmt_list)){
    if(!is_frmt(param_frmt_list[[param_frmt_idx]])){
      param_name <- param_val[[param_frmt_idx]]
      if(param_name == ""){
        param_name <- paste("number",param_frmt_idx)
      }else{
        param_name <- paste("`",param_name,"`")
      }
      stop(paste0("Entry ",param_name," is not an object of class `frmt`."))
    }
  }


  if(any(param_val == "")){
    param_val[param_val == ""] <- ".default"
    names(param_frmt_list) <- param_val
  }

  if(is.list(group_val)){
    group_val_names <- names(group_val)
    if(is.null(group_val_names)){
      stop("when group_val is a list, must be a named list")
    }else if(any(group_val_names == "")){
      stop("when group_val is a list, each entry must be named")
    }
  }

  frmt_grid <- expand_grid(
    group = list(group_val),
    label = label_val,
    param = param_val
    ) %>%
    mutate(
      fmt_applied = map(param, function(p_){param_frmt_list[[p_]]})
    )

  structure(
    frmt_grid,
    class = c("frmt_structure","frmt_table")
  )
}


#' @rdname frmt
table_body_plan <- function(...){

  frmt_structure_list <- list(...)

  for(struct_idx in seq_along(frmt_structure_list)){
    if(!is_frmt_structure(frmt_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `frmt_structure`."))
    }
  }

  structure(
    frmt_structure_list,
    class = c("table_body_plan", "frmt_table")
  )
}
