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
footnote_plan <- function(...,marks=c("numbers","letters","standard","extended")){
  footnote_structure_list <- list(...)
  marks = match.arg(marks)

  structure(
    list(struct_list=footnote_structure_list, marks=marks),
    class = c("footnote_plan", "plan")
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
  structure(
  list("text" = footnote_text, "location"= as.list(footnote_loc)),
  class = c("footnote_structure","structure")
  )


}

