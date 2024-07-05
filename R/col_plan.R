#' Define the Column Plan & Span Structures
#'
#' Using <[`tidy-select`][dplyr::dplyr_tidy_select]> expressions and a series
#' span_structures, define the order of the columns. The selection follows "last
#' selected" principals, meaning columns are moved to the _last_ selection as
#' opposed to preserving the first location.
#'
#'
#' @details
#'
#' ## Column Selection
#'
#' When col_plan gets applied and is used to create the output table, the
#' underlying logic sorts out which column specifically is being selected. If a column
#' is selected twice, the _last_ instance in which the column is selected will be
#' the location it gets rendered.
#'
#' Avoid beginning the \code{col_plan()} column selection with a deselection (i.e.
#' \code{col_plan(-col1)}, \code{col_plan(-starts_with("value")))}. This will
#' result in the table preserving all columns not "de-selected" in the
#' statement, and the order of the columns not changed. It is preferred when
#' creating the \code{col_plan()} to identify all the columns planned on
#' preserving in the order they are wished to appear, or if
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> arguments - such as
#' \code{\link[dplyr]{everything}}- are used, identify the de-selection after
#' the positive-selection.
#'
#' Alternatively, once the gt table is produced, use the
#' \code{\link[gt]{cols_hide}} function to remove un-wanted columns.
#'
#'
#' @rdname col_plan
#'
#' @param ... For a col_plan and span_structure,
#'   <[`tidy-select`][dplyr::dplyr_tidy_select]> arguments, unquoted expressions
#'   separated by commas, and span_structures. span_structures must have the
#'   arguments named to match the name the column in the input data has to identify the correct columns. See the examples
#' @param .drop Boolean. Should un-listed columns be dropped from the data.
#'   Defaults to FALSE.
#'
#' @seealso \href{https://gsk-biostatistics.github.io/tfrmt/articles/col_plan.html}{Link to related article}
#'
#' @return col_plan object
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' ## select col_1 as the first column, remove col_last, then create spanning
#' ## structures that have multiple levels
#' ##
#' ## examples also assume the tfrmt has the column argument set to c(c1, c2, c3)
#' ##
#' spanning_col_plan_ex <- col_plan(
#'  col_1,
#'  -col_last,
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c2 = "Second Label Level 1.1",
#'    c3 = c(col_3, col_4)
#'  ),
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c2 = "Second Label Level 1.2",
#'    c3 = starts_with("B")
#'    ),
#'  span_structure(
#'    c1 = "Top Label Level 1",
#'    c3 = col_5
#'  ),
#'  span_structure(
#'    c2 = "Top Label Level 2",
#'    c3 = c(col_6, col_7)
#'  )
#' )
#'
#' ## select my_col_1 as the first column, then
#' ## rename col_2 to new_col_1 and put as the
#' ## second column, then select the rest of the columns
#' renaming_col_plan_ex <- col_plan(
#'    my_col_1,
#'    new_col_1 = col_2,
#'    everything()
#'  )
#'
#' renaming_col_plan_ex2 <- col_plan(
#'    my_col_1,
#'    new_col_1 = col_2,
#'    span_structure(
#'     c1 = c(`My Favorite span name` = "Top Label Level 1"),
#'     c3 = c(`the results column` = col_5)
#'    )
#'  )
#'
##' @section Images:
#' Here are some example outputs:
#'
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-span_structure-cropped.jpg\" alt =\"Example of a dataset being turned into a table with spanning columns\" style=\"width:100\\%;\">"`
#' }}
#'
col_plan <- function(..., .drop = FALSE){

  ## selectively evaluate dots (only if is a span_structure)
  ## confirm contents otherwise
  dots <- as.list(substitute(substitute(...)))[-1]
  dots <- check_col_plan_dots(dots)

  structure(
    list(
      dots = dots,
      .drop = .drop
    ),
    class = c("col_plan","plan")
  )
}

#' @rdname col_plan
#'
#' @return span_structure object
#'
#' @export
span_structure <- function(...){

  span_cols <- as.list(substitute(substitute(...)))[-1]
  span_cols <- check_span_structure_dots(span_cols)

  structure(
    span_cols,
    class = c("span_structure")
  )
}

is_span_structure <- function(x){
  inherits(x, "span_structure")
}

#' @importFrom rlang eval_tidy
#' @noRd
check_span_structure_dots <- function(x){

  x_names <- names(x)

  if(is.null(x_names) | any(x_names == "")){
    abort(
      paste0("Entries of a span_stucture must be named:\n ",format(caller_call())),
      call = caller_call()
    )
  }

  x_dots <- x %>%
    map(~lapply(trim_vars_quo_c(.x),function(x){

    if(is.name(x)){
      if(identical(as_label(x), "<empty>")){
        return(NULL)
      }else{
        return(quo(!!x))
      }
    }else if(is.call(x)){
      if(is_valid_tidyselect_call(x)){
        quo(!!x)
      }else if(is_valid_quo_call(x)){
        return(eval_tidy(x))
      }else{
        abort(
          message = paste0(
            "Invalid entry: `",format(x),"`\n",
          "Only selection helpers (See <https://tidyselect.r-lib.org/reference>), ",
          " or unquoted expressions representing variable names ",
          " can be entered as contents.",
          " Changing the names of individual variables using new_name = old_name syntax is allowable"
          ),
          call = caller_call()
        )
      }
    }else if(is.character(x)){
      return(as_length_one_quo.character(x))
    }else{
      abort("Unexpected entry type in span_structure()",
            call = caller_call())
    }
  }))

  x_dots[!sapply(x_dots, is.null)]
}

is_valid_span_structure_call <- function(x){
  as.character(as.list(x)[[1]]) %in% c("span_structure")
}

#' @importFrom tidyselect vars_select_helpers
is_valid_tidyselect_call <- function(x){
  ## drop - from determining if
  if(as.character(as.list(x)[[1]]) == "-"){
    x <- x[[-1]]
    if(is.name(x)){
      return(TRUE)
    }
  }
  as.character(as.list(x)[[1]]) %in% c(names(vars_select_helpers))
}

is_valid_quo_call <- function(x){
  ## drop - from determining if
  if(as.character(as.list(x)[[1]]) == "-"){
    x <- x[[-1]]
    if(is.name(x)){
      return(TRUE)
    }
  }
  as.character(as.list(x)[[1]]) %in% c("vars","quo")
}

check_col_plan_dots <- function(x){

  lapply(x,function(x){
    if(is.name(x)){
      if(identical(as_label(x), "<empty>")){
        return(NULL)
      }else{
        return(quo(!!x))
      }
    }else if(is.call(x)){
      if(is_valid_tidyselect_call(x)){
        quo(!!x)
      }else if(is_valid_quo_call(x) | is_valid_span_structure_call(x)){
        return(eval_tidy(x))
      }else{
        stop(
          "Invalid entry: `",format(x),"`\n",
          "Only span_structures (`span_structure()`), ",
          "selection helpers (See <https://tidyselect.r-lib.org/reference>), ",
          " or unquoted expressions representing variable names ",
          " can be entered as contents.",
          " Changing the names of individual variables using new_name = old_name syntax is allowable",
          call. = FALSE
        )
      }
    }else if(is.character(x)){
      return(as_length_one_quo.character(x))
    }else{
      stop("Unexpected entry type in span_structure()")
    }
  })
}

