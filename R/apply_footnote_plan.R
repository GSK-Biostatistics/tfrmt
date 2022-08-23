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
        apply_cells_column_labels(tfrmt,footnote_loc[[i]],tfrmt$footnote_plan$struct_list[[i]])
    }
    gt %>%
      opt_footnote_marks(marks = tfrmt$footnote_plan$marks )



  }
}

#' Apply Cells Column Labels
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param loc list containing location of footnote and footnote text
#' @param footnote footnote structure information
#'
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_column_labels <- function(gt,tfrmt,loc,footnote){
  # check row is empty - therefore a column footnote
  if(is.null(loc$row) && loc$spanning ==FALSE){
    # check lowest level column
    if(length(footnote$column_val)==1 || as_label(tfrmt$column[[length(tfrmt$column)]]) %in% names(tfrmt$footnote_plan$struct_list[[i]]$location)){
      # no spanning
      if(length(tfrmt$column)==1){
        col_name <- loc$col
      }

      gt<- gt %>%
        tab_footnote(
          footnote = loc$note,
          locations = cells_column_labels(columns = all_of(
            as.character(col_name)))
        )
      gt

    }

  }else{
    gt
  }


}


#' Apply Cells Column Spanners
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param loc list containing location of footnote and footnote text
#' @param footnote footnote structure information
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_column_spanners <- function(gt,tfrmt,i){
  # check row is empty - therefore a column footnote
  if(is.null(loc$row)){


  }
}
