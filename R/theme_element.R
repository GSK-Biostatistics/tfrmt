
#' Element Row Group Location
#'
#'
#' @param location Location of the row group labels. Specifying 'indented'
#'  combines all group and label variables into a single column with each
#'  sub-group indented under its parent. 'spanning' and 'column' retain the
#'  highest level group variable in its own column and combine all remaining
#'  group and label variables into a single column with sub-groups indented. The
#'  highest level group column will either be printed as a spanning header or in
#'  its own column in the gt. The 'noprint' option allows the user to suppress
#'  group values from being printed. Finally, the 'gtdefault' option allows
#'  users to use the {gt} defaults for styling multiple group columns.
#' @param indent A string of the number of spaces you want to indent
#'
#' @seealso [row_grp_plan()] for more details on how to group row group
#'  structures, [row_grp_structure()] for more details on how to specify row
#'  group structures, [element_block()] for more details on how to specify
#'  spacing between each group.
#'
#'  \href{https://gsk-biostatistics.github.io/tfrmt/articles/row_grp_plan.html}{Link
#'  to related article}
#'
#' @returns element_row_grp_loc object
#' @export
#' @examples
#'
#' tfrmt_spec <- tfrmt(
#'   group = c(grp1, grp2),
#'   label = label,
#'   param = param,
#'   value = value,
#'   column = column,
#'   row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "noprint")),
#'   body_plan = body_plan(
#'     frmt_structure(group_val = ".default", label_val = ".default", frmt("xx"))
#'   )
#' )
#'
#' @section Images: Here are some example outputs:
#'
#'  \if{html}{\out{ `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-row_group_plan-cropped.jpg\" alt=\"Examples showing the difference between the row group locations\" style=\"width:100\\%;\">"` }}
#'
element_row_grp_loc <- function(location = c("indented", "spanning", "column", "noprint", "gtdefault"),
                                indent = "  "){
  location = match.arg(location)
  structure(
    list(location = location, indent = indent),
    class = c("element_row_grp_loc", "element")
  )
}

is_element_row_grp_loc <- function(x){
  inherits(x, "element_row_grp_loc")
}


#' Element block
#'
#' @param post_space Option to create a new line after group block; specified characters will fill the cells
#' @param border Option to add a solid border to group block (rectangle or just bottom border)
#'
#' @return element block object
#'
#' @seealso [row_grp_plan()] for more details on how to group row group
#'   structures, [row_grp_structure()] for more details on how to specify row group
#'   structures, [element_row_grp_loc()] for more details on how to
#'   specify whether row group titles span the entire table or collapse.
#'
#' @export
#' @examples
#'
#' tfrmt_spec <- tfrmt(
#'   group = grp1,
#'   label = label,
#'   param = param,
#'   value = value,
#'   column = column,
#'   row_grp_plan = row_grp_plan(
#'     row_grp_structure(group_val = ".default", element_block(post_space = "   "))
#'   ),
#'   body_plan = body_plan(
#'     frmt_structure(group_val = ".default", label_val = ".default", frmt("xx"))
#'   )
#' )
element_block <- function(post_space = c(NULL, " ", "-"),
                          border = c(NULL, "outline", "bottom")){
  structure(
    list(post_space = post_space, border = border),
    class = c("element_block", "element")
  )

}

is_element_block <- function(x){
  inherits(x, "element_block")
}

#' Element Column
#'
#' @param col Column value to align on from `column` variable.
#' @param align Alignment to be applied to column. Acceptable values: "left" for
#'   left alignment, "right" for right alignment", or supply a vector of
#'   character(s) to align on. For the case of character alignment, if more than
#'   one character is provided, alignment will be based on the first occurrence
#'   of any of the characters. For alignment based on white space, leading white
#'   spaces will be ignored.
#' @param width Width to apply to the column. Acceptable values include a
#'   numeric value, or a character string of numbers ending with either "px" or
#'   "%", indicating the column width is either n pixels across or % of the
#'   total table width
#'
#'
#' @details Supports alignment and width setting of data value columns (values found in the `column` column). Row group and label
#'   columns are left-aligned by default.
#'
#' @importFrom purrr map
#'
#' @seealso [col_style_plan()] for more information on how to combine
#'   element_col()'s together to form a plan.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/col_style_plan.html}{Link to related article}
#'
#' @return element_col object
#' @export
#' @examples
#'
#'  plan <- col_style_plan(
#'     element_col(align = "left", width = 100, col = "my_var"),
#'     element_col(align = "right", width = "200px", col = vars(four)),
#'     element_col(align = c(".", ",", " "), col = vars(two, three)),
#'     element_col(width = "25%", col = c(two, three))
#'    )
#'
#' @rdname theme_element
element_col <- function( col = vars(),
                         align = NULL,
                         width = NULL
){
  cols <- quo_get("col", as_var_args = "col", allow_tidy_select = TRUE)$col

  width <- validate_width_units(width)

  if(is.null(width) & is.null(align)){
    abort("Alignment or column width definition must be applied to create this element_col",
          class = "missing_element_col_value")
  }


  structure(
    list(
      col = cols,
      align = align,
      width = width
    ),
    class = c("element_col", "element")
  )
}


#' Check if input is an element_col object
#'
#' @param x Object to check
#'
#' @noRd
is_element_col <- function(x){
  inherits(x, "element_col")
}


element_stub <- function(collapse_ord = vars(), collapse_into = vars(), remove_dups = NULL){
  structure(
    list(collapse_ord = collapse_ord, collapse_into = collapse_into, remove_dups = remove_dups),
    class = c("element_stub", "element")
  )
}

#col_labels = element_label(newCol = "Hello World", wrap_txt = 30)
element_label <- function(..., wrap_txt = 30){
  structure(
    list(...) %>%
      c(wrap_txt = wrap_txt),
    class = c("element_label", "element")
  )
}

is_element_label <- function(x){
  inherits(x, "element_label")
}
