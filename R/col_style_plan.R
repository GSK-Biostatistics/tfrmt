#' Column Style Plan
#'
#' Define how the columns of the table body should be aligned, whether left,
#' right or on a specific character(s).
#'
#' @param ... series of col_style_structure objects
#'
#' @return col_style_plan object
#'
#' @examples
#'
#'  plan <- col_style_plan(
#'     col_style_structure(align = "left", width = 100, col = "my_var"),
#'     col_style_structure(align = "right", col = vars(four)),
#'     col_style_structure(align = c(".", ",", " "), col = vars(two, three))
#'    )
#'
#'
#'
#' @seealso [col_style_structure()] for more information on how to specify how to and which columns to align.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/col_style_plan.html}{Link to related article}
#'
#' @export
#'
col_style_plan <- function(...){

  structure_list <- list(...)

  for(el_idx in seq_along(structure_list)){
    if(!is_col_style_structure(structure_list[[el_idx]])){
      stop(paste0("Entry number ",el_idx," is not an object of class `col_style_structure`."))
    }
  }

  structure(
    structure_list,
    class = c("col_style_plan", "frmt_table")
  )
}


#' Column Style Structure
#'
#' @param col Column value to align on from `column` variable. May be a quoted
#'   or unquoted column name, a tidyselect semantic, or a span_structure.
#' @param align Alignment to be applied to column. Acceptable values: "left" for
#'   left alignment, "right" for right alignment", or supply a vector of
#'   character(s) to align on. For the case of character alignment, if more than
#'   one character is provided, alignment will be based on the first occurrence
#'   of any of the characters. For alignment based on white space, leading white
#'   spaces will be ignored.
#' @param width Width to apply to the column in number of characters. Acceptable values include a
#'   numeric value, or a character string of a number.
#' @param ... These dots are for future extensions and must be empty
#'
#'
#' @details Supports alignment and width setting of data value columns (values found in the `column` column). Row group and label
#'   columns are left-aligned by default.
#'
#' @importFrom purrr map
#' @importFrom rlang check_dots_empty0
#'
#' @seealso [col_style_plan()] for more information on how to combine
#'   col_style_structure()'s together to form a plan.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/col_style_plan.html}{Link to related article}
#'
#' @return col_style_structure object
#' @export
#' @examples
#'
#'  plan <- col_style_plan(
#'     col_style_structure(align = "left", width = 100, col = "my_var"),
#'     col_style_structure(align = "right", width = 200, col = vars(four)),
#'     col_style_structure(align = c(".", ",", " "), col = vars(two, three)),
#'     col_style_structure(width = 25, col = c(two, three)),
#'     col_style_structure(width = 25, col = two),
#'     col_style_structure(width = 25, col = span_structure(span = value, col = val2))
#'    )
#'
#' @rdname theme_element
col_style_structure <- function(col, align = NULL, width = NULL, ...){

  check_dots_empty0(...)

  if(missing(col)){
    abort(
      "Column element is missing from col_style_structure. Note: col here refers to the values within the column variable in your data, rather than the variable name itself",
      class = "missing_col_style_structure_value"
    )
  }

  cols <- as.list(substitute(substitute(col)))[-1] %>%
    map(trim_vars_quo_c) %>%
    do.call('c',.) %>%
    check_col_plan_dots()

  if(is.null(width) & is.null(align)){
    abort("`align` or `width` must be applied to create this col_style_structure",
          class = "missing_col_style_structure_value")
  }

  if(!is.null(align)){
    if(!is.character(align) & length(align) > 0){
      abort("`align` must be an character vector", class = "invalid_col_style_structure_value")
    }

    if(!all(align %in% c("left","right"))){

      if (!all(nchar(align)==1)){
        message( "Alignment specified contains strings with >1 characters. Only the first character will be used.")
        align <- str_sub(align, start=1, end=1)
      }

      if (any(str_detect(align, "[[:alnum:]]"))){
        message( "Alignment specified contains one or more alphanumeric characters. Results may not be as expected.")
      }
    }

  }

  if(!is.null(width)){
    if(is.character(width)){
      suppressWarnings(width <- as.numeric(width))
      if(is.na(width)){
        abort("`width` must be a value that can be converted into a number greater than 0", class = "invalid_col_style_structure_value")
      }
    }
    if(any(!is.numeric(width),width < 1, length(width) > 1)){
      abort("`width` must be a valid number greater than 0", class = "invalid_col_style_structure_value")
    }
  }


  structure(
    list(
      cols = cols,
      align = align,
      width = width
    ),
    class = c("col_style_structure", "structure")
  )
}


#' Check if input is an col_style_structure object
#'
#' @param x Object to check
#'
#' @noRd
is_col_style_structure <- function(x){
  inherits(x, "col_style_structure")
}


