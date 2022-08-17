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
       apply_cells_column_spanners(tfrmt,i) %>%
       apply_cells_stub(tfrmt,i) #%>%
      # apply_cells_row_groups(tfrmt,i)


    }
    gt %>%
      opt_footnote_marks(marks = tfrmt$footnote_plan$marks )



  }
}

#' Apply Cells Stub
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param i the numberof the current footnote structure within the footnote plan
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
#' @importFrom rlang quo_get_expr
apply_cells_stub <- function(gt,tfrmt,i){
  # check label in location
  # check location contains only groups and labels

  if((as_label(tfrmt$label[[2]]) %in% names(tfrmt$footnote_plan$struct_list[[i]]$location)) && (all(names(tfrmt$footnote_plan$struct_list[[i]]$location) %in% c(str_remove(as.character(tfrmt$group),"~"),as_label(tfrmt$label[[2]]))))){
    if(length(as_label(tfrmt$group[[1]]))==0 ||  tfrmt$row_grp_plan$label_loc$location != "indented"){
    # get dataframe of groups and labels
    data<-gt$`_data` %>%
      mutate(n = 1:n()) %>%
      filter(!!parse_expr(paste0(paste0(names(tfrmt$footnote_plan$struct_list[[i]]$location),"== '",tfrmt$footnote_plan$struct_list[[i]]$location,"'") ,collapse = " & ")))
    }

    gt<- gt %>%
      tab_footnote(
        footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
        locations = cells_stub(rows = all_of(data$n)
        ))
    gt

    # need to add indented labels and also groups here.

  }else{
      gt
    }

}

#' Apply Cells Row Groups
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param i the numberof the current footnote structure within the footnote plan
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_cells_row_groups <- function(gt,tfrmt,i){
  # only top level group treated as row_group, lower levels treated as labels
  if(length(tfrmt$footnote_plan$struct_list[[i]]$location)==1 && names(tfrmt$footnote_plan$struct_list[[i]]$location)== as_label(tfrmt$group[[1]])){
  # spanning
  if(tfrmt$row_grp_plan$label_loc$location == "spanning"){
    gt<- gt %>%
      tab_footnote(
        footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
        locations = cells_row_groups(groups = all_of(
          as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
        ))
      )

  }
  }
}

#' Apply Cells Column Labels
#'
#' @param gt gt object to potentially add a footnote to
#' @param tfrmt tfrmt object
#' @param i the numberof the current footnote structure within the footnote plan
#'
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
    # find one mentioned by user
    delim_string <- paste0(tfrmt$footnote_plan$struct_list[[i]]$location,collapse="___tlang_delim___")
    # search in variables
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
#' @param i the numberof the current footnote structure within the footnote plan
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


#' Create Indent Filter
#'
#' @param tfrmt tfrmt object
#' @param footnote_loc list of footnote location from footnote structure
#'
#' @return
#' @export
#'
#' @examples
create_indent_filter <- function(tfrmt,footnote_loc){

  filter_statement <- list()

  for (i in 1:length(footnote_loc)){

    # if indented
    if(tfrmt$row_grp_plan$label_loc$location=="indented"){

      # if label
      if(names(footnote_loc[i])==as_label(tfrmt$label)){
        # one indent for each group
        filter_statement[i] <- paste0(names(footnote_loc[i]), " == '",paste0(rep(tfrmt$row_grp_plan$label_loc$indent,length(tfrmt$group)),collapse=""),footnote_loc[i],"'")
      # if group
      }else if(names(footnote_loc[i]) %in% str_remove(as.character(tfrmt$group),"~")){
        # if x level group, x-1 indents
        x<- match(names(footnote_loc[i]),str_remove(as.character(tfrmt$group),"~"))
        filter_statement[i] <- paste0(names(footnote_loc[i]), " == '",paste0(rep(tfrmt$row_grp_plan$label_loc$indent,x-1),collapse=""),footnote_loc[i],"'")
      }

    }else{
      # if label
      if(names(footnote_loc[i])==as_label(tfrmt$label)){
        filter_statement[i] <- paste0(names(footnote_loc[i]), " == '",paste0(rep(tfrmt$row_grp_plan$label_loc$indent,length(tfrmt$group)-1),collapse=""),footnote_loc[i],"'")
      # if group
      }else if(names(footnote_loc[i]) %in% str_remove(as.character(tfrmt$group),"~")){
        # if x level group, x-2 indents
        # turn negatives into 0
        x<- match(names(footnote_loc[i]),str_remove(as.character(tfrmt$group),"~"))
        filter_statement[i] <- paste0(names(footnote_loc[i]), " == '",paste0(rep(tfrmt$row_grp_plan$label_loc$indent,pmax(x-2,0)),collapse=""),footnote_loc[i],"'")

    }

  }


}
  filter_statement <- paste0(filter_statement,collapse = " & ")
  filter_statement
}


