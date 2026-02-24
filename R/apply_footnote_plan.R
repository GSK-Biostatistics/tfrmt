#' Apply Footnote Plan
#'
#' @param gt gt object  to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param footnote_loc list containing footnote location
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks tab_options
apply_footnote_plan <- function(gt, tfrmt,footnote_loc){
  if(is.null(tfrmt$footnote_plan)){
    gt
  } else {
    for (i in 1:length(tfrmt$footnote_plan$struct_list)) {

      gt <- gt %>%
        apply_general_footnote(footnote_loc[[i]]) %>%
        apply_cells_column_labels(footnote_loc[[i]]) %>%
        apply_cells_column_spanners(footnote_loc[[i]]) %>%
        apply_cells_stub(tfrmt,footnote_loc[[i]]) %>%
        apply_cells_row_groups(tfrmt,footnote_loc[[i]]) %>%
        apply_cells_body(footnote_loc[[i]])

    }
    gt %>%
      opt_footnote_marks(marks = tfrmt$footnote_plan$marks ) %>%
      tab_options(footnotes.order = tfrmt$footnote_plan$order)

  }
}


#' Apply footnote
#'
#' @param gt gt object  to potentially add a footnote to
#' @param loc  list containing footnote text
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote
apply_general_footnote <- function(gt,loc){
  if(length(loc$row)==0 && length(loc$col)==0){
    gt <- gt %>%
      tab_footnote(
        footnote =loc$note)


  }
  gt

}

#' Apply Cells Column Labels
#'
#' @param gt gt object to potentially add a footnote to
#' @param loc list containing location of footnote and footnote text
#'
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_column_labels <- function(gt,loc){
  # check row is empty - therefore a column footnote, and not a spanning column

  if(is.null(loc$row) && loc$spanning ==FALSE){

      gt<- gt %>%
        tab_footnote(
          footnote = loc$note,
          locations = cells_column_labels(columns = loc$col)
        )

  }
  gt


}


#' Apply Cells Column Spanners
#'
#' @param gt gt object to potentially add a footnote to
#' @param loc list containing location of footnote and footnote text
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_column_spanners <- function(gt,loc){
  # check row is empty - therefore a column footnote
  if(!is.null(loc) && is.null(loc$row) && loc$spanning ==TRUE){

    gt<- gt %>%
      tab_footnote(
        footnote = loc$note,
        locations = cells_column_spanners(spanners = loc$col
        )
      )


  }
  gt
}


#' Apply Cells Stub
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param loc list containing location of footnote and footnote text
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
#' @importFrom rlang quo_get_expr
apply_cells_stub <-  function(gt,tfrmt,loc){
  if(length(loc$col)>0){

  in_stub <- (
    #column row grp label (all stubs)
    (!is.null(tfrmt$row_grp_plan) &&
                tfrmt$row_grp_plan$label_loc$location=="column") &&
    all(loc$col %in% map_chr(c(tfrmt$group, tfrmt$label), as_label)) ||
      # in the label
      (all(loc$col==as_label(tfrmt$label)))
    )

  if(in_stub){

    gt<- gt %>%
      tab_footnote(
        footnote = loc$note,
        locations = cells_stub(rows = loc$row, columns = loc$col)
      )


  }

  }
  gt

}



#' Apply Cells Row Groups
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param loc list containing location of footnote and footnote text
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_row_groups <- function(gt,tfrmt,loc){
  if(length(loc$col)>0){
    if(all(loc$col %in% map_chr(tfrmt$group, as_label) )){

      gt<- gt %>%
        tab_footnote(
          footnote = loc$note,
          locations = cells_row_groups(groups = loc$row)
          )

     }

  }
  gt
}


#' Apply Cells Body
#'
#' @param gt gt object to potentially add a footnote to
#' @param loc list containing location of footnote and footnote text
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_body<- function(gt,loc){
  if(!is.null(loc$col) && !is.null(loc$row)){
    gt<- gt %>%
      tab_footnote(
        footnote = loc$note,
        locations = cells_body(columns = loc$col, rows = loc$row
        )
      )
  }
  gt

}
