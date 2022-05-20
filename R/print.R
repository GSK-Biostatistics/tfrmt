
#' Print mock table to GT
#'
#' @param tfrmt tfrmt the mock table will be based off of
#' @param .data Optional data. If this is missing, group values, labels values
#'   and parameter values will be estimated based on the tfrmt
#' @param .default sequence to replace the default values if a dataset isn't
#'   provided
#' @param n_cols the number of columns this will only be used if mock data isn't
#'   provided
#'
#' @return a stylized gt object
#' @export
#' @importFrom gt gt tab_header tab_style cell_text cells_body
#' @importFrom tidyselect everything
#' @importFrom rlang quo_is_missing
print_mock_gt <- function(tfrmt, .data = NULL, .default = 1:3, n_cols = 3) {
  if(is.null(.data)){
    .data <- make_mock_data(tfrmt, .default, n_cols)
  }

  if(quo_is_missing(tfrmt$values)){
    tfrmt$values <- vars(val)[[1]]
  }

  apply_tfrmt(.data, tfrmt, mock = TRUE) %>%
    cleaned_data_to_gt(tfrmt)

}

#' Print to gt
#'
#' @param tfrmt tfrmt object that will dictate the structure of the table
#' @param .data Data to style in order to make the table
#'
#' @return a stylized gt object
#' @export
#' @importFrom gt gt tab_header tab_style cell_text cells_body tab_options
#' @importFrom tidyselect everything
print_to_gt <- function(tfrmt, .data){

  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    cleaned_data_to_gt(tfrmt)

}


#' Do all the formatting for the GT
#'
#' @param .data cleaned dataset
#' @param tfrmt tfrmt
#'
#' @return GT object
#' @noRd
cleaned_data_to_gt <- function(.data, tfrmt){
  if(is.null(tfrmt$row_grp_style) && length(tfrmt$group) > 0){
    .data <- .data %>%
      group_by(!!!tfrmt$group)
  }

  gt_out <- .data %>%
    gt(
      rowname_col = as_label(tfrmt$label)) %>%
    tab_header(title = tfrmt$title,
               subtitle = tfrmt$subtitle) %>%
    apply_gt_footnote(tfrmt$footer) %>%
    apply_gt_spanning_labels(col_plan = tfrmt_spec$col_plan)%>%
    tab_style(
      style = cell_text(whitespace = "pre"),
      locations = cells_body(columns = everything())
    )

  if(!is.null(tfrmt$row_grp_style) && tfrmt$row_grp_style$label_loc == "column"){
    gt_out <- gt_out %>%
      tab_options(row_group.as_column = TRUE)
  }
  gt_out

}


#' Apply gt Footnote
#'
#' @param gt gt object  to potentially add a footnote to
#' @param footer footnote text (should become a footnote element at somepoint )
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_source_note md
apply_gt_footnote<- function(gt, footer){
  if(is.null(footer)){
    gt
  } else {
    gt %>%
      tab_source_note(
        source_note = md(footer)
      )

  }
}

apply_gt_spanning_labels <- function(gt_table, spanning_lab_struct){
  if(!is.null(spanning_lab_struct)){

    # get set of tab_spanner functions to apply
    spanning_lab_grps <- apply_spanning_labels( gt_table$`_data`, spanning_lab_struct)

    #loop over the tab_spanners to add to the gt table
    for(spanning_lab_apply_idx in seq_along(spanning_lab_grps)){
      spanning_lab_func <- spanning_lab_grps[[spanning_lab_apply_idx]]
      gt_table <- spanning_lab_func(gt_table)
    }
  }
  gt_table
}

