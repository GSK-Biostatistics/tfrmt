#' Check if input is a frmt
#'
#' @param x Object to check
#' @export
#' @examples
#' x1 <- frmt("XXX.XX")
#' is_frmt(x1)
#'
#' @return 'TRUE' if yes, 'FALSE' if no
#'
#' @rdname frmt_utils
is_frmt <- function(x){
  inherits(x, "frmt")
}

#' Check if input is a frmt_combine
#'
#' @param x Object to check
#' @export
#' @examples
#' x2 <- frmt_combine("XXX %","XX,XXX")
#' is_frmt_combine(x2)
#'
#' @rdname frmt_utils
is_frmt_combine <- function(x){
  inherits(x, "frmt_combine")
}

#' Check if input is a frmt_when
#'
#' @param x Object to check
#' @export
#' @examples
#' x2 <- frmt_when(
#' ">3" ~ frmt("(X.X%)"),
#' "<=3" ~ frmt("Undetectable")
#' )
#' is_frmt_when(x2)
#'
#' @rdname frmt_utils
is_frmt_when <- function(x){
  inherits(x, "frmt_when")
}
#' Check if input is a frmt_structure
#'
#' @param x Object to check
#' @export
#' @examples
#' x3 <- frmt_structure(
#'  group_val = c("group1"),
#'  label_val = ".default",
#' frmt("XXX")
#' )
#'is_frmt_structure(x3)
#'
#' @rdname frmt_utils
is_frmt_structure <- function(x){
  inherits(x, "frmt_structure")
}

#' Check if input is a row_grp_structure
#'
#' @param x Object to check
#' @export
#' @examples
#' x4 <- row_grp_structure(group_val = c("A","C"), element_block(post_space = "---"))
#' is_row_grp_structure(x4)
#'
#' @rdname frmt_utils
is_row_grp_structure <- function(x){
  inherits(x, "row_grp_structure")
}

#' @export
format.frmt <- function(x, ...){
  paste0(
    "< frmt | Expression: `",
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
format.frmt_when <- function(x, ...){
  lhs <- map_chr(x$frmt_ls, f_lhs)
  rhs <- map(x$frmt_ls, f_rhs) %>% map_chr(format)

  frmt_str <- paste(
    "< frmt_when | ", "\n ",
    paste0(
      map2_chr(lhs, rhs, paste, sep = " ~ "),
      collapse = "\n  "),
    paste0("\n", "  Missing: ", x$missing),
    "\n >"
    )
  frmt_str
}

#' @export
print.frmt_when <- function(x,...){
  cat(format(x),sep = "\n")
}

#' @export
#' @importFrom purrr map
format.frmt_structure <- function(x,...){

  if (is.list(x$group_val)){
    groups <- x$group_val %>% map(unique)
  } else {
    groups <- unique(x$group_val)
  }
  labels <- unique(x$label_val)
  param <- unique(x$param_val)
  fmts <- x$frmt_to_apply[[1]]

  if(is.list(groups)){
    group_string <- paste0(
      sapply(names(groups), function(y) {
        paste0(" `",y,"` - ", paste0("\"", x$group_val[[y]], "\"", collapse = ", "))
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


  if(!identical(param,".default")){
    frmt_struct_str <- c(
      frmt_struct_str,
      paste0("  Param Values: ",paste0("\"",param,"\"", collapse=", "))
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

#' @export
#' @keywords internal
format.body_plan <- function(x,...){

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

print.body_plan <- function(x, ...){
  cat(format(x, ...), sep = "\n")
}



# Constructing frmt from strings ------------------------------------------

#' Build frmt for a given parameter
#'
#' @param param `param` value
#' @param frmt_string formatted expression
#' @param missing missing option to be included in all `frmt`s
#'
#' @return character string representing `frmt` object with `param` value as name
#' @noRd
#' @importFrom purrr map2
#' @importFrom stats setNames
frmt_builder <- function(param, frmt_string, missing = NULL) {

  if(!missing(param)){
    frmt_string <- setNames(frmt_string, param)

  }

  map(frmt_string, function(x, missing_val) {
    do.call(frmt, list(expression = x, missing = missing_val ))
  }, missing_val = missing)

}

#' Build frmt_combine for a given set of parameters
#'
#' @param param_combine character string representing how `param` values will be
#'   combined using `glue::glue()` syntax
#' @param param vector of `param` values
#' @param frmt_string vector of formatted expressions
#' @param missing missing option to be included in all `frmt`s
#'
#' @return character string representing `frmt_combine` object
#' @noRd
frmt_combine_builder <- function(param_combine, param, frmt_string, missing = NULL){

  frmts <- frmt_builder(param, frmt_string, missing)

  list(do.call(frmt_combine, c(expression = param_combine, frmts, missing = missing)))
}

#' Build format structure from a list of `frmt` and `frmt_combine` objects
#'
#' @param group_val A string or a named list of strings which represent the value of group should be when the given frmt is implemented
#' @param label_val A string which represent the value of label should be when the given frmt is implemented
#' @param frmt_vec Character vector of `frmt` and/or `frmt_combine` objects to be applied to the group_val/label_val combination
#'
#' @return list of `frmt_structure` objects
#' @noRd
#' @importFrom purrr pmap
#' @importFrom rlang `%||%`
frmt_structure_builder <- function(group_val, label_val, frmt_vec){

  grp_lbl_list <- list(list(group_val = group_val, label_val = label_val))
  frmt_vec_list <- map2(names(frmt_vec), frmt_vec, ~list(param = .x %||% "", frmt = .y))

  crossing(frmt_vec_list,
           grp_lbl_list) %>%
    pmap(function(frmt_vec_list, grp_lbl_list){

      if(is.list(grp_lbl_list$group_val) & length(grp_lbl_list$group_val) == 1 & is.null(names(grp_lbl_list$group_val))){
        grp_lbl_list$group_val <- grp_lbl_list$group_val[[1]]
      }

      if(is.list(grp_lbl_list$label_val) & length(grp_lbl_list$label_val) == 1& is.null(names(grp_lbl_list$label_val))){
        grp_lbl_list$label_val <- grp_lbl_list$label_val[[1]]
      }

      arg_list <- list(grp_lbl_list$group_val,  grp_lbl_list$label_val, frmt_vec_list$frmt)
      names(arg_list) <- c("group_val","label_val",frmt_vec_list$param)

      do.call(frmt_structure, arg_list)
    }) %>%
    unname()

}


missing_to_chr <- function(x){
  if(!is.null(x) ){
    paste0("'", x, "'")
  } else {
    x
  }
}


#' @method as.character frmt
#' @export
as.character.frmt <- function(x, ...){
  paste0("frmt('", x$expression, "'",
         if_else(!is.null(x$missing), paste0(", missing = ", missing_to_chr(x$missing)), ""),
         if_else(!is.null(x$scientific), paste0(", scientific = ", x$scientific), ""),
         if_else(!is.null(x$transform), paste0(", transform = ", deparse(x$transform) %>% str_c(collapse = "")), ""),
         ")"
         )
}

#' @method as.character frmt_when
#' @importFrom rlang quo `!!` f_rhs f_lhs eval_tidy as_label
#' @importFrom stringr str_c
#' @importFrom dplyr if_else
#' @export
as.character.frmt_when <- function(x, ...){

  right <- x$frmt_ls %>%
    map_chr(function(x){
      val <- quo(!!f_rhs(x))
      val_eval <- eval_tidy(val)
      if (!is_frmt(val_eval)){
        as_label(val)
      } else {
        as.character(val_eval)
      }

    })

  left <- x$frmt_ls %>%
    map_chr(~f_lhs(.x)) %>%
    str_c("'", ., "'")
  params <- str_c(left, " ~ ", right) %>%
    str_c(collapse = ", ")

  paste0("frmt_when(",
         params,
         if_else(!is.null(x$missing), paste0(", missing = ", missing_to_chr(x$missing)), ""),
         ")"
  )
}


#' @method as.character frmt_combine
#' @export
as.character.frmt_combine <- function(x, ...){
  params <- x$frmt_ls %>%
    map_chr(~as.character(.x)) %>%
    str_c(names(x$frmt_ls), " = ", .) %>%
    str_c(collapse = ", ")
  paste0("frmt_combine('", x$expression, "', ",
         params,
         if_else(!is.null(x$missing), paste0(", missing = ", missing_to_chr(x$missing)), ""),
         ")"
  )
}


#' @method as.character span_structure
#' @export
as.character.span_structure <- function(x, ...){
  values <- x %>%
    map(function(val){
      elements <- map_chr(val, as_label) %>%
        str_replace_all("\\\"", "'")

      not_fxs <-elements %>%
        str_which("\\(.+\\)", negate = TRUE)
      elements[not_fxs] <- elements[not_fxs] %>%
        str_c("'", ., "'")

      if(rlang::is_named(val)){
        elements = str_c("`", names(val), "`", " = ", elements)
      }

      elements %>%
        str_c(collapse = ", ") %>%
        str_c("c(", ., ")")
      }
      )

  paste0("span_structure(",
         str_c(names(values), " = ", values) %>% str_c(collapse = ", "),
         ")"
  )
}
