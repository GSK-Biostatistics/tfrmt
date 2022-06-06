
#'Element Row Group Location
#'
#'
#'@param location Location of the row group labels. Specifying 'indented'
#'  combines all group and label variables into a single column with each
#'  sub-group indented under its parent. 'spanning' and 'column' retain the
#'  highest level group variable in its own column and combine all remaining
#'  group and label variables into a single column with sub-groups indented. The
#'  highest level group column will either be printed as a spanning header or in
#'  its own column in the gt. Finally, the 'noprint' option allows the user to
#'  suppress group values from being printed.
#'@param indent A string of the number of spaces you want to indent
#'
#' @seealso [row_grp_plan()] for more details on how to group row group
#'   structures, [row_grp_stucture()] for more details on how to specify row group
#'   structures, [element_block()] for more details on how to specify spacing
#'   between each group.
#'
#'@export
element_row_grp_loc <- function(location = c("indented", "spanning", "column", "noprint"),
                                indent = "  "){
  location = match.arg(location)
  structure(
    list(location= location, indent = indent),
    class = c("element_row_grp_loc", "element")
  )
}

is_element_row_grp_loc <- function(x){
  inherits(x, "element_row_grp_loc")
}


#' Element block
#'
#' @param post_space Option to create a new line after group block; specified characters will fill the cells
#' @param background_fill Option to fill background for group block
#' @param border Option to add a solid border to group block (rectangle or just bottom border)
#'
#' @return element block object
#'
#' @seealso [row_grp_plan()] for more details on how to group row group
#'   structures, [row_grp_stucture()] for more details on how to specify row group
#'   structures, [element_row_grp_loc()] for more details on how to
#'   specify whether row group titles span the entire table or collapse.
#'
#' @export
element_block <- function(post_space = c(NULL, " ", "-"),
                          background_fill = NULL,
                          border = c(NULL, "outline", "bottom")){
  structure(
    list(post_space = post_space, background_fill = background_fill, border = border),
    class = c("element_block", "element")
  )

}


#' Element Align
#'
#' @param align Alignment to be applied to column. Acceptable values: "left" for
#'   left alignment, "right" for right alignment", or supply a vector of
#'   character(s) to align on. For the case of character alignment, if more than
#'   one character is provided, alignment will be based on the first occurrence
#'   of any of the characters. For alignment based on white space, leading white
#'   spaces will be ignored.
#' @param col Variable to align on
#'
#' @importFrom purrr map
#'
#' @seealso [col_align_plan()] for more information on how to combine
#'   element_align()'s together to form a plan.
#'
#' @export
#' @rdname theme_element
element_align <- function(align = "left",
                          col = vars()){

  cols <- quo_get("col", as_var_args = "col") %>% map(~as_vars(.x))

  structure(
    c(
      list(align = align),
      cols
      ),
    class = c("element_align", "element")
  )
}


#' Check if input is an element_align object
#'
#' @param x Object to check
#'
#' @noRd
is_element_align <- function(x){
  inherits(x, "element_align")
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

# element_bounds(upper_exp = ">5", lower_exp, upper_lab, lower_lab)
element_bounds <- function(upper_exp = NULL, lower_exp = NULL, upper_lab = NULL, lower_lab = NULL){
  if(!is.null(upper_exp) & is.null(upper_lab)){
    upper_lab = upper_exp
  }

  if(!is.null(lower_exp) & is.null(lower_lab)){
    lower_lab = lower_exp
  }
  structure(
    list(upper_exp = upper_exp, lower_exp = lower_exp,
         upper_lab = upper_lab, lower_lab = lower_lab),
    class = c("element_bounds", "element")
  )
}

