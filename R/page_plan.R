#' Page Plan
#'
#' Defining the location and/or frequency of page splits with a series of
#' page_structure's and the row_every_n argument, respectively.
#'
#' @param ... a series of [page_structure()] separated by commas
#' @param note_loc Location of the note describing each table's subset value(s).
#'   Useful if the `page_structure` contains only ".default" values (meaning the
#'   table is split by every unique level of a grouping variable), and that
#'   variable is dropped in the col_plan. `preheader` only available for rtf output.
#' @param max_rows Option to set a maximum number of rows per page. Takes a numeric value.
#'
#' @return page_plan object
#' @export
#'
#' @examples
#'  # use of page_struct
#'  page_plan(
#'     page_structure(group_val = "grp1", label_val = "lbl1")
#'  )
#'
#'  # use of #  rows
#'  page_plan(
#'     max_rows = 5
#'  )
#'
#'
page_plan <- function(...,
                      note_loc = c("noprint","preheader","subtitle","source_note"),
                      max_rows = NULL){

  page_structure_list <- list(...)
  note_loc <- match.arg(note_loc)

  structure(
    list(struct_list = page_structure_list, note_loc=note_loc, max_rows=max_rows),
    class = c("page_plan", "plan")
  )
}

#' Page structure
#'
#' @param group_val string or a named list of strings which represent the value of group to split after.
#' Set to ".default" if the split should occur after every unique value of the variable.
#' @param label_val string which represents the value of label to split after.
#'   Set to ".default" if the split should occur after every unique value of
#'   the variable.

#'
#' @return page structure object
#' @export
#'
#' @examples
#' # split page after every unique level of the grouping variable
#'  page_structure(group_val = ".default", label_val = NULL)
#'
#'  # split page after specific levels
#'  page_structure(group_val = "grp1", label_val = "lbl3")
page_structure <- function(group_val = NULL, label_val = NULL){


  if(length(group_val)>1 && is.list(group_val)==FALSE && !is.null(names(group_val))){
    group_val <- as.list(group_val)
  }else if(length(group_val)==1 && !is.null(names(group_val))){
    group_val<-as.list(group_val)
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
      group_val = group_val,
      label_val = label_val),
    class = c("page_structure","structure")
  )

}
