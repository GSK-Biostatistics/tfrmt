#' Check if input is a frmt
#'
#' @export
#'
#' @rdname frmt_utils
is_frmt <- function(x){
  inherits(x, "frmt")
}

#' Check if input is a frmt_combine
#'
#' @export
#'
#' @rdname frmt_utils
is_frmt_combine <- function(x){
  inherits(x, "frmt_combine")
}

#' Check if input is a frmt_structure
#'
#' @export
#'
#' @rdname frmt_utils
is_frmt_structure <- function(x){
  inherits(x, "frmt_structure")
}


#' @export
format.frmt <- function(x, ...){
  paste0(
    "<frmt | Expression: `",
    x$expression,
    "` >"
  )
}

#' @export
print.frmt <- function(x,...){
  cat(format(x),sep = "\n")
}

#' @export
format.frmt_combine <- function(x, ...){
  frmt_str <- paste0(
    "< frmt_combine | Expression: `",
    x$expression,
    "` >"
  )
  frmt_str
}

#' @export
print.frmt_combine <- function(x,...){
  cat(format(x),sep = "\n")
}

#' @export
format.frmt_structure <- function(x,...){

  groups <- unique(x$group_val)[[1]]
  labels <- unique(x$label_val)
  param <- unique(x$param_val)
  fmts <- x$frmt_to_apply[[1]]

  if(is.list(groups)){
    group_string <- paste0(
      sapply(names(groups), function(x) {
        paste0(" `",x,"` - ", paste0("\"", groups_val[[x]], "\"", collapse = ", "))
      }),
      collapse = ";"
    )
  }else{
    group_string <- paste0(" \"",groups,"\"", collapse=",")
  }


  frmt_struct_str <- c(
    "Format Structure",
    paste0("  Group Values:",group_string),
    paste0("  Label Values: ",paste0("\"",labels,"\"", collapse=", "))
  )

  if(param != ".default"){
    frmt_struct_str <- c(
      frmt_struct_str,
      paste0("  Param Value: \"",param,"\"")
    )
  }

  frmt_struct_str <- c(
    frmt_struct_str,
    paste0("  Format: ",format(fmts))
    )


  frmt_struct_str

}

#' @method print frmt_structure
#' @export
print.frmt_structure <- function(x, ...){
  cat(format(x, ...), sep = "\n")
}


format.table_body_plan <- function(x,...){

  table_body_plan_str <- c(
    "Table Body Plan",
    paste0(" ",length(x)," Format Structures:")
  )

  frmt_str_seq_len <- seq_len(length(x))
  frmt_str_num <- format(frmt_str_seq_len)

  tabin <- max(nzchar(frmt_str_num))

  for(frmt_struct_idx in frmt_str_seq_len){

    frmt_struct_fmt <- format(x[[frmt_struct_idx]])

    table_body_plan_str <- c(
      table_body_plan_str,
      "",
      paste0("  [[",frmt_str_num[frmt_struct_idx],"]] ",frmt_struct_fmt[1]),
      paste0(paste0(" ",rep("",tabin)),frmt_struct_fmt[-1])
    )

  }

  table_body_plan_str

}

print.table_body_plan <- function(x, ...){
  cat(format(x, ...), sep = "\n")
}

