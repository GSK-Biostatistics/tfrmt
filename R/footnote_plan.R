#' Footnote Plan
#'
#' @param ...  footnote structure objects separated by commas
#' @param marks type of marks required for footnotes. available options are "numbers", "letters", "standard" and "extended".
#' The default option is set to "numeric".
#'
#' @return footnote plan object
#' @export
#'
#' @examples
footnote_plan <- function(...,marks="numbers"){
  footnote_structure_list <- list(...)


  structure(
    list(struct_list=footnote_structure_list, marks=marks),
    class = c("footnote_plan", "frmt_table")
  )
}

#' Footnote Structure
#'
#' @param footnote_text string with text for footnote
#' @param footnote_loc named list indicating location of footnote mark
#'
#' @return footnote structure object
#' @export
#'
#' @examples
footnote_structure <- function(footnote_text, footnote_loc){

  list("text" = footnote_text, "location"= as.list(footnote_loc))


}

