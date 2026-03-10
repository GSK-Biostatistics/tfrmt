#' Extract underlying data from tfrmt output
#'
#' Following a call to `print_to_gt`, this function extracts the underlying
#' data frame(s) from the resulting `gt` or `gt_group` object
#'
#' @param x A `gt_tbl` or `gt_group` object (usually the output of `print_to_gt()`).
#' @return If `gt_tbl`, a single data frame. If `gt_group`, a list of data frames
#'   (one per table).
#' @importFrom purrr map
#' @export
extract_data <- function(x) {

  #  single gt table
  if (inherits(x, "gt_tbl")) {
    return(x[["_data"]])
  }

  #  Grouped gt object (created when using `page_plan`)
  if (inherits(x, "gt_group")) {

    # Extract the internal list of gt_tbl objects
    tbl_list <- x$gt_tbls$gt_tbl

    # Map over the list to pull the '_data' slot from each page
    extracted_list <- map(tbl_list, ~ .x[["_data"]])

    return(extracted_list)
  }

  # Fallback for unsupported types
  stop("Input must be a 'gt_tbl' or 'gt_group' object (the result of calling `print_to_gt()`).")
}
