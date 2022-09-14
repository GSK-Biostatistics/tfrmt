#' Apply Footnote Plan
#'
#' @param gt gt object  to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param footnote_loc list containing footnote location
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_footnote_plan <- function(gt, tfrmt,footnote_loc){
  if(is.null(tfrmt$footnote_plan)){
    gt
  } else {
    for (i in 1:length(tfrmt$footnote_plan$struct_list)) {

      gt <- gt %>%
        apply_source_note(footnote_loc[[i]]) %>%
        apply_cells_column_labels(footnote_loc[[i]]) %>%
        apply_cells_column_spanners(footnote_loc[[i]]) %>%
        apply_cells_stub(tfrmt,footnote_loc[[i]]) %>%
        apply_cells_row_groups(tfrmt,footnote_loc[[i]]) %>%
        apply_cells_body(footnote_loc[[i]])

    }
    gt %>%
      opt_footnote_marks(marks = tfrmt$footnote_plan$marks )



  }
}


#' Apply Source Note
#'
#' @param gt gt object  to potentially add a source note to
#' @param loc  list containing source note text
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_source_note
apply_source_note <- function(gt,loc){
  if(length(loc$row)==0 && length(loc$col)==0){
    gt <- gt %>%
      tab_source_note(loc$note)


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
          locations = cells_column_labels(columns = all_of(
            as.character(loc$col)))
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
  if(is.null(loc$row) && loc$spanning ==TRUE){

    gt<- gt %>%
      tab_footnote(
        footnote = loc$note,
        locations = cells_column_spanners(spanners = all_of(
          loc$col
        ))
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
  if(all(loc$col == as_label(tfrmt$label))){

    gt<- gt %>%
      tab_footnote(
        footnote = loc$note,
        locations = cells_stub(rows = all_of(loc$row))
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
          locations = cells_row_groups(groups = all_of(all_of(loc$row))
          )
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
        locations = cells_body(columns = all_of(loc$col), rows = all_of(all_of(loc$row))
        )
      )
  }
  gt

}
