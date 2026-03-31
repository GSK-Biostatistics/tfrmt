#' Clean Column Names and Remove Internal tfrmt Columns
#'
#' Internal helper to replace the internal tlang delimiter with a user-specified string.
#'
#' @param df A data frame.
#' @param delim Character string to replace the internal "tlang_delim".
#' @param stubhead The _stubhead slot from the gt object.
#' @importFrom dplyr select starts_with rename_with pull filter rename
#' @importFrom stringr str_replace_all
#' @importFrom tidyselect everything
#' @return A data frame with updated column names.
#' @noRd
clean_data <- function(df, delim, boxhead = NULL, stubhead = NULL) {

  # Update Stub/Group Column Names
  # Boxhead tells us which columns are 'stub' columns
  if (!is.null(boxhead) && !is.null(stubhead)) {

    # Identify the variable names that are marked as 'stub'
    stub_vars <- boxhead %>%
      filter(type == "stub") %>%
      pull(var)

    # Get the new labels from stubhead
    new_stub_labels <- as.character(unlist(stubhead$label))

    if (length(stub_vars) > 0 && length(stub_vars) == length(new_stub_labels)) {

      lookup <- setNames(stub_vars, new_stub_labels)

      #  Filter out entries where the new name is empty or NA
      valid_names <- names(lookup) != "" & !is.na(names(lookup))
      lookup <- lookup[valid_names]

      if (length(lookup) > 0) {
        df <- df %>% dplyr::rename(dplyr::any_of(lookup))
      }
    }
  }



  df %>%
    # Drop internal tfrmt columns (e.g., ..tfrmt_row_grp_lbl)
    select(-starts_with("..tfrmt")) %>%
    # Replace the internal tlang_delim pattern in column names
    rename_with(
      ~ str_replace_all(.x, "___tlang_delim___", delim),
      .cols = everything()
    )
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

  #Fallback
  if (!inherits(x, c("gt_tbl", "gt_group"))) {
    stop("Input must be a 'gt_tbl' or 'gt_group' object.")
  }

  # Single gt table
  if (inherits(x, "gt_tbl")) {
   return(clean_data(
      df = x[["_data"]],
      delim = col_delim,
      boxhead = x[["_boxhead"]],
      stubhead = x[["_stubhead"]]
    ))
  }

  # Grouped gt object (created when using `page_plan`)
  if (inherits(x, "gt_group")) {

    # Extract the internal list of gt_tbl objects
    tbl_list <- x$gt_tbls$gt_tbl

    # Map over the list to pull the '_data' slot and clean names
    extracted_list <- map(tbl_list, ~ clean_data(.x[["_data"]],
                                                 delim = col_delim,
                                                 boxhead = .x[["_boxhead"]],
                                                 stubhead = .x[["_stubhead"]]
    ))

    return(extracted_list)
  }


}
