#' Column Alignment Plan
#'
#' Define how the columns of the table body should be aligned, whether left,
#' right or on a specific character(s).
#'
#' @param ... list of frmt_structure
#'
#' @return table_body_plan object
#'
#' @examples
#'
#'  plan <- col_align_plan(
#'     element_align(align = "left", col = "my_var"),
#'     element_align(align = "right", col = vars(four)),
#'     element_align(align = c(".", ",", " "), col = vars(two, three))
#'    )
#'
#' @seealso [element_align()] for more information on how to specify how to and which columns to align.
#'
#' @export
#'
col_align_plan <- function(...){

  element_align_list <- list(...)

  for(el_idx in seq_along(element_align_list)){
    if(!is_element_align(element_align_list[[el_idx]])){
      stop(paste0("Entry number ",el_idx," is not an object of class `element_align`."))
    }
  }

  structure(
    element_align_list,
    class = c("col_align_plan", "frmt_table")
  )
}
