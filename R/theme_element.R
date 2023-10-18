
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
#'  users to use the 'gt' defaults for styling multiple group columns.
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
