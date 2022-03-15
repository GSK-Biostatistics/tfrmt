
element_block <- function(background_fill,
                          outline = element_line(),
                          post_space = c(NULL, " ", "", "------")){

}

element_style <- function(){

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
    class = c("eleement_align", "element")
  )
}

#row_grp = element_grp(newCol = vars(hello, world), newCol2 = vars(hello, world))
element_grp <- function(...){
  structure(
    list(...),
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
element_bounds <- function(upper_exp = "", lower_exp = "", upper_lab = "", lower_lab = ""){
  structure(
    list(upper_exp = upper_exp, lower_exp = lower_exp,
         upper_lab = upper_lab, lower_lab = lower_lab),
    class = c("element_bounds", "element")
  )
}
