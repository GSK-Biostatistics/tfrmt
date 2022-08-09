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
      }else if (length(tfrmt$column)>1 && length(names(tfrmt$footnote_plan$struct_list[[i]]$location))==1 && names(tfrmt$footnote_plan$struct_list[[i]]$location) == as_label(tfrmt$column[[1]])){

        gt<- gt %>%
          tab_footnote(
            footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
            locations = cells_column_spanners(spanners = all_of(
              as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
            ))
          )
        # spanned columns
      }else if (length(tfrmt$column)>1 && is.na(names(tfrmt$footnote_plan$struct_list[[i]]$location[2])) == FALSE){
        if (names(tfrmt$footnote_plan$struct_list[[i]]$location[2]) == as_label(tfrmt$column[[2]])){

        # need to create string with __delim for spanned headers
        delim_string <- paste0(tfrmt$footnote_plan$struct_list[[i]]$location[1],"___tlang_delim___",tfrmt$footnote_plan$struct_list[[i]]$location[2])

        gt<- gt %>%
          tab_footnote(
            footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
            locations = cells_column_labels(columns = all_of(
              as.character(delim_string)
            ))
          )
        # labels
      }}else if (names(tfrmt$footnote_plan$struct_list[[i]]$location[1]) == as_label(tfrmt$label[[2]])){

        gt<- gt %>%
          tab_footnote(
            footnote = as.character(tfrmt$footnote_plan$struct_list[[i]]$text),
            locations = cells_stub(rows = all_of(
              as.character(tfrmt$footnote_plan$struct_list[[i]]$location[[1]])
            ))
          )

        # groups
      }else if(names(tfrmt$footnote_plan$struct_list[[i]]$location[1]) == as_label(tfrmt$group[[1]])){
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

      }}

    }
    gt %>%
      opt_footnote_marks(marks = tfrmt$footnote_plan$marks )



  }
}
