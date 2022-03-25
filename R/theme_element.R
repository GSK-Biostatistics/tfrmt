
element_block <- function(background_fill,
                          outline = element_line(),
                          post_space = c(NULL, " ", "", "------")){
  structure(
    list(background_fill = background_fill, outline = outline, post_space = post_space),
    class = c("element_block", "element")
  )

}



element_style <- function(...){
  structure(
    list(all_fmts = list(...)),
    class = c("element_style", "element")
  )
}

#' Element Align
#'
#' @param left Variables to align to the left
#' @param right Variables to align to the right
#' @param dec_pl Variable to align around decimal places
#'
#' @export
element_align <- function(left = vars(), right = vars(), dec_pl = vars()){
  structure(
    list(left = left, right = right, dec_pl = dec_pl),
    class = c("element_align", "element")
  )
}


element_grp <- function(grp_ord = vars(), collapse_into = vars(), remove_dups = NULL){
  structure(
    list(grp_ord = grp_ord, collapse_into = collapse_into, remove_dups = remove_dups),
    class = c("element_grp", "element")
  )
}

#col_labels = element_label(newCol = "Hello World", wrap_txt = 30)
element_label <- function(..., wrap_txt = 30){
  structure(
    list(...) %>%
      c(wrap_txt = wrap_txt),
    class = c("element_label", "element")
  )
}


# element_bounds(upper_exp = ">5", lower_exp, upper_lab, lower_lab)
element_bounds <- function(upper_exp = NULL, lower_exp = NULL, upper_lab = NULL, lower_lab = NULL){
  if(!is_null(upper_exp) & is_null(upper_lab)){
    upper_lab = upper_exp
  }

  if(!is_null(lower_exp) & is_null(lower_lab)){
    lower_lab = lower_exp
  }
  structure(
    list(upper_exp = upper_exp, lower_exp = lower_exp,
         upper_lab = upper_lab, lower_lab = lower_lab),
    class = c("element_bounds", "element")
  )
}
