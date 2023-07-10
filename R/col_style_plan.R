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
#'     col_style_structure(col = "my_var", align = "left", width = 100),
#'     col_style_structure(col = vars(four), align = "right"),
#'     col_style_structure(col = vars(two, three), align = c(".", ",", " "))
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
#' @param align Alignment to be applied to column. Defaults to `left` alignment. See details for acceptable values.
#' @param type Type of alignment: "char" or "pos", for character alignment (default), and positional alignment, respectively. Positional alignment allows for aligning over multiple positions in the column.
#' @param width Width to apply to the column in number of characters. Acceptable values include a
#'   numeric value, or a character string of a number.
#' @param ... These dots are for future extensions and must be empty
#'
#'
#' @details Supports alignment and width setting of data value columns (values found in the `column` column). Row group and label columns are left-aligned by default. Acceptable input values for `align` differ by type = "char" or "pos":
#'
#' ## Character alignment (type = "char"):
#'   - "left" for left alignment
#'   - "right" for right alignment"
#'   - supply a vector of character(s) to align on. If more than
#'   one character is provided, alignment will be based on the first occurrence
#'   of any of the characters. For alignment based on white space, leading white
#'   spaces will be ignored.
#'
#' ## Positional alignment (type = "pos"):
#'  supply a vector of strings covering all formatted cell values, with numeric values represented as x's. These values can be created manually or obtained by utilizing the helper `display_val_frmts()`. Alignment positions will be represented by vertical bars. For example, with starting values: c("12.3", "(5%)", "2.35 (10.23)") we can align all of the first sets of decimals and parentheses by providing align = c("xx|.x", "||(x%)", "x|.xx |")
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
#'     col_style_structure(col = "my_var",
#'                         align = c("xx| |(xx%)",
#'                                   "xx|.x |(xx.x - xx.x)"),
#'                         type = "pos", width = 100),
#'     col_style_structure(col = vars(four), align = "right", width = 200),
#'     col_style_structure(col = vars(two, three), align = c(".", ",", " ")),
#'     col_style_structure(col = c(two, three), width = 25),
#'     col_style_structure(col = two, width = 25),
#'     col_style_structure(col = span_structure(span = value, col = val2),
#'                         width = 25)
#'    )
#'
#' @rdname theme_element
col_style_structure <- function(col, align = NULL, type = c("char", "pos"), width = NULL, ...){

  check_dots_empty0(...)

  type <- match.arg(type)

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

    if (type=="char"){
      if(!is.character(align) & length(align) > 0){
        abort("`align` must be a character vector", class = "invalid_col_style_structure_value")
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
      type = type,
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


