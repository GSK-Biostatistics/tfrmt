#' Table Body Plan
#'
#' Define the formatting of the body contents of the table through a series of
#' frmt_structures. Structures get applied in order from bottom up, so the last
#' added structure is the first applied.
#'
#' @seealso [frmt_structure()] defines which rows the formats will be applied
#'   to, and [frmt()], [frmt_combine()], and [frmt_when()] define the format
#'   semantics.
#'
#' @param ... list of frmt_structures defining the body formatting
#'
#' @return body_plan object
#'
#' @examples
#'
#'   example_tfrmt<- tfrmt(
#'       title = "Table Title",
#'       body_plan = body_plan(
#'         frmt_structure(
#'           group_val = c("group1"),
#'           label_val = ".default",
#'           frmt("XXX")
#'         )
#'       )
#'      )
#'
#' @export
#'
body_plan <- function(...){

  frmt_structure_list <- list(...)

  for(struct_idx in seq_along(frmt_structure_list)){
    if(!is_frmt_structure(frmt_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `frmt_structure`."))
    }
  }

  structure(
    frmt_structure_list,
    class = c("body_plan", "frmt_table")
  )
}
