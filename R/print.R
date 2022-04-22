# print_mock_gt <- function(tfmt) {
#
# }

#' Print to gt
#'
#' @param tfrmt_spec tfrmt object that will dictate the structure of the table
#' @param .data Data to style in order to make the table
#'
#' @return a stykized GT object
#' @export
#' @importFrom gt gt tab_header
print_to_gt <- function(tfrmt_spec, .data){
  apply_tfrmt(.data, tfrmt_spec) %>%
    gt(
      groupname_col = as_label(tfrmt_spec$group[[1]]),
      rowname_col = as_label(tfrmt_spec$label)) %>%
    tab_header(title = tfrmt_spec$title,
               subtitle = tfrmt_spec$subtitle) %>%
    apply_gt_footnote(tfrmt_spec$footer)
}


#' Apply gt Footnote
#'
#' @param gt gt object  to potentially add a footnote to
#' @param footer footnote text (should become a footnote element at somepoint )
#'
#' @return gt objest
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
