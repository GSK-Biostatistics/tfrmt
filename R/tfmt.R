
tfmt <- function(
  title,
  title_txt_style,
  subtitle,
  subtitle_txt_style,
  col_label_txt_style,
  spanning_label_txt_style,
  footer,
  footer_txt_style,
  txt_style,
  row_grp_txt_style, # style of the grouping level
  row_txt_style,#Style of the rest of the rows
  row_grp_style, #the style between blocking
  body_style,
  body_txt_style,
  body_align,
  sorting_cols,
  page_vars,
  row_grp, # col which is used to make the row grps
  col_labels,
  spanning_label_grp,
  col_select ,
  ...
){
  #TODO rewrite find_args for our package
  elements <- ggplot2:::find_args(...)


  structure(
    elements,
    class = c("tfmt"),

  )

}


