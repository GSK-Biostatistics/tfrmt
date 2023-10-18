#' Format Structure Object
#'
#' Function needed to create a frmt_structure object, which is a building block
#' of [body_plan()]. This specifies the rows the format will be applied to.
#'
#' @seealso [body_plan()] combines the frmt_structures to be applied to the
#'   table body, and [frmt()], [frmt_combine()], and [frmt_when()] define the
#'   format semantics.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/body_plan.html}{Link to related article}
#'
#' @param group_val A string or a named list of strings which represent the
#'   value of group should be when the given frmt is implemented
#' @param label_val A string which represent the value of label should be when
#'   the given frmt is implemented
#' @param ... either a [frmt()], [frmt_combine()], or a [frmt_when()] object.
#'   This can be named to also specify the parameter value
#'
#' @section Images:
#' Here are some example outputs:
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-frmts.jpg\" alt = \"Example comparing fmt, frmt_combine, and frmt_when\" style=\"width:100\\%;\">"`
#' }}
#'
#' @examples
#'
#' sample_structure <- frmt_structure(
#'           group_val = c("group1"),
#'           label_val = ".default",
#'           frmt("XXX")
#'         )
#' ## multiple group columns
#' sample_structure <- frmt_structure(
#'           group_val = list(grp_col1 = "group1", grp_col2 = "subgroup3"),
#'           label_val = ".default",
#'           frmt("XXX")
#'         )
#'
#' @returns frmt_structure object
#' @export
frmt_structure <- function(group_val = ".default", label_val = ".default", ...){
  param_frmt <- list(...)
  param_val <- names(param_frmt)

  if(length(param_frmt) > 1){
    stop("Can only handle one format per frmt_structure function. Use frmt_combine if a combination is needed")
  }

  if(is_frmt_combine(param_frmt[[1]])){
    param_val <- names(param_frmt[[1]]$frmt_ls)
  } else if(is.null(param_val)){
    param_val <- ".default"
  }

  if(!is_frmt(param_frmt[[1]])){
    stop(paste0("Entry is not an object of class `frmt`"))
  }


  if(is.list(group_val)){
    group_val_names <- names(group_val)
    if(is.null(group_val_names)){
      stop("when group_val is a list, must be a named list")
    }else if(any(group_val_names == "")){
      stop("when group_val is a list, each entry must be named")
    }
  }

  structure(
    list(
      group_val = group_val,
      label_val = label_val,
      param_val = param_val,
      frmt_to_apply = param_frmt),
    class = c("frmt_structure","frmt_table")
  )
}



#' Table Value Formatting
#'
#' @description
#' These functions provide an abstracted way to approach to define formatting of table
#' contents. By defining in this way, the formats can be
#' layered to be more specific and general cell styling can be done first.
#'
#' `frmt()` is the base definition of a format. This defines spacing, rounding,
#' and missing behaviour.
#'
#' `frmt_combine()` is used when two or more rows need to be combined into a
#' single cell in the table. Each of the rows needs to have a defined `frmt()`
#' and need to share a label.
#'
#' `frmt_when()` is used when a rows format behaviour is dependent on the value itself and is written similarly to [dplyr::case_when()].
#'  The left hand side of the equation is a `"TRUE"`for the default case or the right hand side of a boolean expression `">50"`.
#'
#' @seealso [body_plan()] combines the frmt_structures to be applied to the
#'   table body, and [frmt_structure()] defines which rows the formats will be applied
#'   to.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/body_plan.html}{Link to related article}
#'
#' @param expression this is the string representing the intended format. See details: expression for more a detailed description.
#' @param missing when a value is missing that is intended to be formatted, what value to place. See details: missing for more a detailed description.
#' @param scientific a string representing the intended scientific notation to be appended to the expression. Ex. "e^XX" or " x10^XX".
#' @param transform this is what should happen to the value prior to formatting,
#'   It should be a formula or function. Ex. `~.*100`if you want to convert a
#'   percent from a decimal prior to rounding
#' @param ...  See details: `...` for a detailed description.
#'
#'
#' @details
#'
#' ## expression
#'  - `frmt()` All numbers are represented by "x". Any additional character are
#'  printed as-is. If additional X's present to the left of the decimal point
#'  than the value, they will be represented as spaces.
#'  - `frmt_combine()` defines how the parameters will be combined as a
#'  `glue::glue()` statement. Parameters need to be equal to the values in the
#'  param column and defined in the expression as \code{"{param1} {param2}"}.
#'
#' ## missing
#'  - `frmt()` Value to enter when the value is missing. When NULL, the value
#'  is "".
#'  - `frmt_combine()` defines how when all values to be combined are missing.
#'  When NULL the value is "".
#'
#' ## ...
#'    - `frmt()` These dots are for future extensions and must be
#'    empty.
#'    - `frmt_combine()` accepts named arguments defining the `frmt()` to
#'    be applied to which parameters before being combined.
#'    - `frmt_when()`accepts a series of equations separated by commas, similar
#'    to [dplyr::case_when()]. The left hand side of the equation is a `"TRUE"`for the
#'    default case or the right hand side of a boolean expression `">50"`. The
#'    right hand side of the equation is the `frmt()` to apply when the left
#'    side evaluates to `TRUE`.
#'
#' @export
#' @return frmt object
#' @examples
#'
#' frmt("XXX %")
#'
#' frmt("XX.XXX")
#'
#' frmt("xx.xx", scientific = "x10^xx")
#'
#' frmt_combine(
#'  "{param1} {param2}",
#'  param1 = frmt("XXX %"),
#'  param2 = frmt("XX.XXX")
#' )
#'
#' frmt_when(
#'   ">3" ~ frmt("(X.X%)"),
#'   "<=3" ~ frmt("Undetectable")
#'   )
#'
#' frmt_when(
#'   "==100"~ frmt(""),
#'   "==0"~ "",
#'   "TRUE" ~ frmt("(XXX.X%)")
#'   )
#'
#' @rdname frmt
#'
frmt <- function(expression, missing = NULL, scientific = NULL, transform = NULL, ...){
  structure(
    list(expression = expression, missing = missing, scientific = scientific,
         transform = transform),
    class = c("frmt")
  )
}


#' @export
#' @rdname frmt
#' @importFrom stringr str_extract_all str_count str_replace str_detect
frmt_combine <- function(expression, ..., missing = NULL){

  everything_but_curly <- "(?<=\\{)([^}]*)(?=\\})"

  n_vars <- str_count(expression, everything_but_curly)
  vars_to_fmt <- str_extract_all(expression, everything_but_curly, simplify = TRUE)
  vars_to_fmt <- as.vector(vars_to_fmt)
  frmt_ls <- list(...)

  if(n_vars != length(frmt_ls) & length(frmt_ls) > 1){
    stop("The number of formats must be 1 or match the number of parameters", call. = FALSE)
  } else if (n_vars > 1 & length(frmt_ls) == 1){
    frmt_ls <- frmt_ls[rep(1,n_vars)]
  }

  names(frmt_ls) <- vars_to_fmt

  # Adding ` to expression if not there and there is a space/symbol
  replace_val <-case_when(
    str_detect(vars_to_fmt, "^[a-zA-Z0-9_.]*$") ~ vars_to_fmt,
    !str_detect(vars_to_fmt, "^[a-zA-Z0-9_.]*$") & !str_detect(vars_to_fmt, "`") ~ paste0("`", vars_to_fmt, "`"),
    TRUE ~ vars_to_fmt)

  exp_new <- expression
  for(i in 1:length(replace_val)){
    exp_new <- str_replace(exp_new, vars_to_fmt[i], replace_val[i])
  }

  structure(
    list(expression = exp_new, frmt_ls = frmt_ls, missing = missing),
    class = c("frmt_combine","frmt")
  )
}


#' @rdname frmt
#' @export
#' @importFrom rlang list2 f_rhs f_rhs<-
#' @importFrom purrr map
frmt_when <- function(..., missing = NULL){
  frmts <- list2(...)

  frmt_ls <- frmts %>%
    map(function(x){
      f_rhs(x) <- eval(f_rhs(x))
      x
    })

  structure(
    list(frmt_ls = frmt_ls, missing = missing),
    class = c("frmt_when","frmt")
  )
}
