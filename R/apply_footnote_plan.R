#' Apply Footnote Plan
#'
#' @param gt gt object  to potentially add a footnote to
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_footnote_plan <- function(gt, tfrmt){

  if(is.null(tfrmt$footnote_plan)){
    gt
  } else {
    for (i in 1:length(tfrmt$footnote_plan$struct_list)) {

     gt <- gt %>%
       apply_cells_column_labels(tfrmt,i) %>%
       apply_cells_column_spanners(tfrmt,i)
       #apply_cells_stub(gt,tfrmt,i)
     #apply_cells_row_groups(gt,tfrmt,i)
     #

    }
    gt %>%
      opt_footnote_marks(marks = tfrmt$footnote_plan$marks )



  }
}

#' Apply Cells Stub
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_stub <- function(gt,tfrmt,i){

}

#' Apply Cells Row Groups
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_row_groups <- function(gt,tfrmt,i){

}

#' Apply Cells Column Labels
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_column_labels <- function(gt,tfrmt,i){

  # are all locations supplied in the column list
  # is lowest level column in location
  if (all(names(tfrmt$footnote_plan$struct_list[[i]]$location) %in% str_remove(as.character(tfrmt$column),"~"))&&
      as_label(tfrmt$column[[length(tfrmt$column)]]) %in% names(tfrmt$footnote_plan$struct_list[[i]]$location)){

  # standard column labels
    if(length(tfrmt$column)==1){
      col_name <- tfrmt$footnote_plan$struct_list[[i]]$location[[1]]
  # spanned column labels
    }else{
    # need to create string with __delim for spanned headers
    delim_list<- gt$`_boxhead`$var[str_detect(gt$`_boxhead`$var,"delim")]
    delim_string=""
    # find one mentioned by user

    # if user has specified the spanning col and spanned
    if (length(tfrmt$footnote_plan$struct_list[[i]]$location)>1){
      for(k in 1:(length(tfrmt$footnote_plan$struct_list[[i]]$location)-1)){
        delim_string<-paste0(delim_string,tfrmt$footnote_plan$struct_list[[i]]$location[k],"___tlang_delim___")
      }}
    # add on end of delim string and search in variables
    delim_string<-paste0(delim_string,tfrmt$footnote_plan$struct_list[[i]]$location[length(tfrmt$footnote_plan$struct_list[[i]]$location)])
    col_name<-delim_list[str_detect(delim_list,delim_string)]
    }

    gt<- gt %>%
      tab_footnote(
        footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
        locations = cells_column_labels(columns = all_of(
          as.character(col_name)
        ))
      )
    gt

  }else{
    gt
  }
}

#' Apply Cells Column Spanners
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_column_spanners <- function(gt,tfrmt,i){

  # more than one column in tfrmt, only one value in location list, location in column list
  if(length(tfrmt$column)>1 && length(names(tfrmt$footnote_plan$struct_list[[i]]$location))==1 && names(tfrmt$footnote_plan$struct_list[[i]]$location) %in% str_remove(as.character(tfrmt$column),"~")[1:(length(as.character(tfrmt$column))-1)]){

    gt<- gt %>%
      tab_footnote(
        footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
        locations = cells_column_spanners(spanners = all_of(
          as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
        ))
      )
    gt

  }else{
    gt
  }

}

