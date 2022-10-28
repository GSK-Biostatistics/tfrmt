#' Column Style Plan
#'
#' Define how the columns of the table body should be aligned, whether left,
#' right or on a specific character(s).
#'
#' @param ... series of element_col objects
#'
#' @return col_style_plan object
#'
#' @examples
#'
#'  plan <- col_style_plan(
#'     element_col(align = "left", width = 100, col = "my_var"),
#'     element_col(align = "right", col = vars(four)),
#'     element_col(align = c(".", ",", " "), col = vars(two, three))
#'    )
#'
#'
#'
#' @seealso [element_col()] for more information on how to specify how to and which columns to align.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/col_style_plan.html}{Link to related article}
#'
#' @export
#'
col_style_plan <- function(...){

  element_col_list <- list(...)

  for(el_idx in seq_along(element_col_list)){
    if(!is_element_col(element_col_list[[el_idx]])){
      stop(paste0("Entry number ",el_idx," is not an object of class `element_col`."))
    }
  }

  structure(
    element_col_list,
    class = c("col_style_plan", "frmt_table")
  )
}

