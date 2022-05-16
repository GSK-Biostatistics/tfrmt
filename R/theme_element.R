
element_block <- function(spanning_header,
                          background_fill,
                          outline,
                          post_space = c(NULL, " ", "", "------")){
  structure(
    list(background_fill = background_fill, outline = outline, post_space = post_space),
    class = c("element_block", "element")
  )

}

# PROBABLY NEEDS DELETING
# element_style <- function(...){
#   structure(
#     list(all_fmts = list(...)),
#     class = c("element_style", "element")
#   )
# }

#' Element Align
#'
#' @param left Variables to align to the left
#' @param right Variables to align to the right
#' @param char Variable to align on a provided character
#' @param char_val Vector of one or more characters to align on. If NULL, data values in `char` variable(s) will be aligned on the first occurrence of a decimal place or space. If more than one
#' character is provided, alignment will be based on the first occurrence of any of the characters. For alignment based on white space, leading white spaces will be ignored.
#'
#' @importFrom purrr map
#'
#' @export
element_align <- function(left = vars(), right = vars(), char = vars(), char_val = "."){

  args <- c("left", "right", "char")

  structure(
    c(
      quo_get(args, as_var_args = args) %>% map(~as_vars(.x)),
      char_val = list(char_val)),
    class = c("element_align", "element")
  )
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

