#' Column Style Plan
#'
#' Define how the columns of the table body should be aligned, whether left,
#' right or on a specific character(s).
#'
#' @param ... series of element_align objects
#'
#' @return col_style_plan  object
#'
#' @examples
#'
#'  plan <- col_style_plan(
#'     element_style(align = "left", width = 100, col = "my_var"),
#'     element_style(align = "right", col = vars(four)),
#'     element_style(align = c(".", ",", " "), col = vars(two, three))
#'    )
#'
#' @seealso [element_style()] for more information on how to specify how to and which columns to align.
#'
#' @export
#'
col_style_plan <- function(...){

  element_style_list <- list(...)

  for(el_idx in seq_along(element_style_list)){
    if(!is_element_style(element_style_list[[el_idx]])){
      stop(paste0("Entry number ",el_idx," is not an object of class `element_style`."))
    }
  }

  structure(
    element_style_list,
    class = c("col_style_plan", "frmt_table")
  )
}


#' @importFrom stringr str_remove
#' @importFrom rlang abort
validate_width_units <- function(width){

  if(is.null(width)){
    return(NULL)
  }

  if (is.numeric(width)) {
    width <- paste0(as.character(width), "px")
  }

  width_units <- str_remove(width,"\\d+")
  if(!width_units %in% c("px","%")){
    abort(paste0(
      "Invalid Units provided for column width: `",width_units,"`.\n",
      "Only `px` (`px()`) or `%` (`pct()`) are accepted."
    ))
  }

  return(width)

}
