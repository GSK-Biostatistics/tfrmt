
element_block <- function(background_fill,
                          outline,
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
#' @param align Alignment to be applied to column. Acceptable values: "left" for left alignment, "right" for right alignment",
#' or supply a vector of character(s) to align on. For the case of character alignment, if more
#' than one character is provided, alignment will be based on the first occurrence of any of the characters. For alignment based on white space, leading white spaces will be ignored.
#' @param col Variable to align on
#'
#' @importFrom purrr map
#'
#' @export
#' @rdname theme_element
element_align <- function(align = "left",
                          col = vars()){

  cols <- quo_get("col", as_var_args = "col") %>% map(~as_vars(.x))

  structure(
    c(
      list(align = align),
      cols
      ),
    class = c("element_align", "element")
  )
}


#' Check if input is an element_align object
#'
#' @param x Object to check
#' @export
#'
#' @rdname theme_element
is_element_align <- function(x){
  inherits(x, "element_align")
}


element_stub <- function(collapse_ord = vars(), collapse_into = vars(), remove_dups = NULL){
  structure(
    list(collapse_ord = collapse_ord, collapse_into = collapse_into, remove_dups = remove_dups),
    class = c("element_stub", "element")
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

is_element_label <- function(x){
  inherits(x, "element_label")
}

# element_bounds(upper_exp = ">5", lower_exp, upper_lab, lower_lab)
element_bounds <- function(upper_exp = NULL, lower_exp = NULL, upper_lab = NULL, lower_lab = NULL){
  if(!is.null(upper_exp) & is.null(upper_lab)){
    upper_lab = upper_exp
  }

  if(!is.null(lower_exp) & is.null(lower_lab)){
    lower_lab = lower_exp
  }
  structure(
    list(upper_exp = upper_exp, lower_exp = lower_exp,
         upper_lab = upper_lab, lower_lab = lower_lab),
    class = c("element_bounds", "element")
  )
}

