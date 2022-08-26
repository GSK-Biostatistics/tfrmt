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
#'
#' footnote_plan <- footnote_plan(
#'     footnote_structure("Source Note"),
#'     marks="letters")
#'
#' footnote_plan <- footnote_plan(
#'     footnote_structure(column_val = "Placebo"),
#'     marks="numbers")
#'
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
#' @param column_val string or a named list of strings which represent the column to apply the footnote to
#' @param group_val string or a named list of strings which represent the value of group to apply the footnote to
#' @param label_val string which represents the value of label to apply the footnote to
#'
#' @return footnote structure object
#' @export
#'
#' @examples
#'
#' footnote_structure <- footnote_structure("Source Note")
#'
#' footnote_structure <- footnote_structure("Text",column_val = "Placebo")
#'
#' footnote_structure <- footnote_structure("Text",column_val = list(col1 = "Placebo", col2= "Treatment groups"))
#'
#' footnote_structure <- footnote_structure("Text",label_val = "Adverse Event")
footnote_structure <- function(footnote_text, column_val = NULL, group_val = NULL, label_val = NULL){

  # force column_val and group_val to list
  if(length(column_val)>1 && is.list(column_val)==FALSE){
    column_val <- as.list(column_val)
  }else if(length(column_val)==1 && !is.null(names(column_val))){
    column_val<-as.list(column_val)
  }

  if(length(group_val)>1 && is.list(group_val)==FALSE){
    group_val <- as.list(group_val)
  }else if(length(group_val)==1 && !is.null(names(group_val))){
    group_val<-as.list(group_val)
  }

  # warnings if elements arent named

  if(is.list(column_val)){
    column_val_names <- names(column_val)
    if(is.null(column_val_names)){
      stop("when column_val is a list, must be a named list")
    }else if(any(column_val_names == "")){
      stop("when column_val is a list, each entry must be named")
    }
  }

  if(is.list(group_val)){
    group_val_names <- names(group_val)
    if(is.null(group_val_names)){
      stop("when group_val is a list, must be a named list")
    }else if(any(group_val_names == "")){
      stop("when group_val is a list, each entry must be named")
    }
  }

   structure(
    list(
      column_val = column_val,
      group_val = group_val,
      label_val = label_val,
      footnote_text = footnote_text),
    class = c("footnote_structure","structure")
  )


}
