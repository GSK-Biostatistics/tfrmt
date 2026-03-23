#' Clean Column Names and Remove Internal tfrmt Columns
#'
#' Internal helper to replace the internal tlang delimiter with a user-specified string.
#'
#' @param df A data frame.
#' @param delim Character string to replace the internal "tlang_delim".
#' @return A data frame with updated column names.
#' @noRd
clean_names <- function(df, delim) {
  #  Drop internal tfrmt columns (starting with ..tfrmt)
  df <- df[, !grepl("^\\.\\.tfrmt", colnames(df)), drop = FALSE]

  if (!is.null(delim)) {
    # Replace the internal tlang_delim pattern with user preference
    colnames(df) <- gsub("___tlang_delim___", delim, colnames(df))
  }
  return(df)
}


#' Extract underlying data from tfrmt output
#'
#' Following a call to `print_to_gt`, this function extracts the underlying
#' data frame(s) from the resulting `gt` or `gt_group` object.
#'
#' @param x A `gt_tbl` or `gt_group` object (usually the output of `print_to_gt()`).
#' @param col_delim Character string to replace the internal "tlang_delim"
#'   separator in column names only for tables with spanning headers. Defaults to "_".
#' @return If `gt_tbl`, a single data frame. If `gt_group`, a list of data frames.
#' @importFrom purrr map
#' @export
extract_data <- function(x, col_delim = "_") {

  # Single gt table
  if (inherits(x, "gt_tbl")) {
    return(clean_names(x[["_data"]], col_delim))
  }

  # Grouped gt object (created when using `page_plan`)
  if (inherits(x, "gt_group")) {

    # Extract the internal list of gt_tbl objects
    tbl_list <- x$gt_tbls$gt_tbl

    # Map over the list to pull the '_data' slot and clean names
    extracted_list <- map(tbl_list, ~ clean_names(.x[["_data"]], col_delim))

    return(extracted_list)
  }

  # Fallback
  stop("Input must be a 'gt_tbl' or 'gt_group' object.")
}
