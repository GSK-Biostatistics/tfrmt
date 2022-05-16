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
print_mock_gt <- function(tfrmt, .data = NULL, .default = 1:3, n_cols = 3) {
  if(is.null(.data)){
    .data <- make_mock_data(tfrmt, .default, n_cols)
  }

  apply_tfrmt(.data, tfrmt, mock = TRUE) %>%
    gt(
      rowname_col = as_label(tfrmt$label)) %>%
    tab_header(title = tfrmt$title,
               subtitle = tfrmt$subtitle) %>%
    apply_gt_footnote(tfrmt$footer)%>%
    tab_style(
      style = cell_text(whitespace = "pre"),
      locations = cells_body(columns = everything())
    )

}

#' Print to gt
#'
#' @param tfrmt tfrmt object that will dictate the structure of the table
#' @param .data Data to style in order to make the table
#'
#' @return a stylized gt object
#' @export
#' @importFrom gt gt tab_header tab_style cell_text cells_body
#' @importFrom tidyselect everything
print_to_gt <- function(tfrmt, .data){
  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    gt(
      rowname_col = as_label(tfrmt$label)) %>%
    tab_header(title = tfrmt$title,
               subtitle = tfrmt$subtitle) %>%
    apply_gt_footnote(tfrmt$footer) %>%
    tab_style(
      style = cell_text(whitespace = "pre"),
      locations = cells_body(columns = everything())
    )
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
