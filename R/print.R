print_mock_gt <- function(tfmt) {

}

print_to_gt <- function(tfrmt_spec, .data){
  apply_tfrmt(.data, tfrmt_spec) %>%
    gt(
      groupname_col = as_label(tfrmt_spec$group[[1]]),
      rowname_col = as_label(tfrmt_spec$label)) %>%
    tab_header(title = tfrmt_spec$title,
               subtitle = tfrmt_spec$subtitle) %>%
    apply_gt_footnote(tfrmt_spec$footer)
}


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
