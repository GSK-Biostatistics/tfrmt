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

#' Apply gt Footnote
#'
#' @param gt gt object  to potentially add a footnote to
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_footnote md opt_footnote_marks
apply_gt_footnote <- function(gt, tfrmt){

  if(is.null(tfrmt$footnote_plan)){
    gt
  } else {
    for (i in 1:length(tfrmt$footnote_plan$struct_list)) {

      # column headers
      if (length(tfrmt$column)==1 && names(tfrmt$footnote_plan$struct_list[[i]]$location) == as_label(tfrmt$column[[1]])) {
        gt<- gt %>%
          tab_footnote(
            footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
            locations = cells_column_labels(columns = all_of(
              as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
            ))
          )


        # spanning headers
        # check if mentioned column is in list of spanning columns
      }else if (length(tfrmt$column)>1 && length(names(tfrmt$footnote_plan$struct_list[[i]]$location))==1 && names(tfrmt$footnote_plan$struct_list[[i]]$location) %in% str_remove(as.character(tfrmt$column),"~")[1:(length(as.character(tfrmt$column))-1)]){

        gt<- gt %>%
          tab_footnote(
            footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
            locations = cells_column_spanners(spanners = all_of(
              as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
            ))
          )

        # spanned columns
        # check multiple columns and check column names are listed in footnote plan
      }else if (length(tfrmt$column)>1 && names(tfrmt$footnote_plan$struct_list[[i]]$location[length(tfrmt$footnote_plan$struct_list[[i]]$location)]) == as_label(tfrmt$column[[length(tfrmt$column)]])){

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
        delim_string_final<-delim_list[str_detect(delim_list,delim_string)]

        gt<- gt %>%
          tab_footnote(
            footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
            locations = cells_column_labels(columns = all_of(
              as.character(delim_string_final)
            ))
          )
        # labels
      }else if (names(tfrmt$footnote_plan$struct_list[[i]]$location[1]) == as_label(quo_get_expr(tfrmt$label))){

        gt<- gt %>%
          tab_footnote(
            footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
            locations = cells_stub(rows = all_of(
              as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
            ))
          )

        # groups
      }else if(length(tfrmt$footnote_plan$struct_list[[i]]$location)==1 && names(tfrmt$footnote_plan$struct_list[[i]]$location[1]) %in% str_remove(as.character(tfrmt$group),"~")){
        # different scenarios depending on location of grouping

        # spanning
        if(tfrmt$row_grp_plan$label_loc$location == "spanning"){

          gt<- gt %>%
            tab_footnote(
              footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
              locations = cells_row_groups(groups = all_of(
                as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
              ))
            )
        }else if(tfrmt$row_grp_plan$label_loc$location == "indented"){

          gt<- gt %>%
            tab_footnote(
              footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
              locations = cells_stub(rows = all_of(
                as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
              ))
            )
        # label for specific group
        }}else if(all(names(tfrmt$footnote_plan$struct_list[[i]]$location) %in% c(as_label(tfrmt$group[[1]]),as_label(tfrmt$label[[2]])))){
          group<-as_label(tfrmt$group[[1]])
          label<-as_label(tfrmt$label[[2]])
          gt_data <- gt$`_data`

          # get matching row numbers
          rows <- which((gt_data[eval(group)] == as.character(tfrmt$footnote_plan$struct_list[[i]]$location[eval(group)])))
          rows2 <-which(gt_data[eval(label)] == as.character(tfrmt$footnote_plan$struct_list[[i]]$location[eval(label)]))

          match(rows,rows2)
          gt<- gt %>%
            tab_footnote(
              footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
              locations = cells_stub(rows = rows[rows==rows2]
            ))


        }

    }
    gt %>%
      opt_footnote_marks(marks = tfrmt$footnote_plan$marks )



  }
}
