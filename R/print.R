print_mock_gt <- function(tfmt) {

}

print_to_gt <- function(tfmt, .data){
  apply_tfmt(.data, tfmt) %>%
    gt() %>%
    tab_header(title = tfmt$title,
               subtitle = tfmt$subtitle) %>%
    apply_gt_footnote(tfmt$footer)

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
