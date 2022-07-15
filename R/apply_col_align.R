#' Apply alignment to a column
#'
#' @param col Character vector containing data values
#' @param align Vector of one or more strings to align on. Strings should be 1
#'   character in length and not alphanumeric (i.e. spaces or punctuation). If
#'   NULL, data values will be aligned on the first occurrence of a decimal
#'   place or space. If more than one character is provided, alignment will be
#'   based on the first occurrence of any of the characters. For alignment based
#'   on white space, leading white spaces will be ignored.
#'
#' @return Character vector containing aligned data values
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across pull tibble
#' @importFrom stringr str_dup str_c str_trim
#' @noRd
apply_col_alignment <- function(col, align){

  if (!all(align %in% c("left","right"))){

    if (!all(nchar(align)==1)){
      message("`align` specified in `element_style` contains strings with >1 characters. Only the first character will be used.")
      align <- str_sub(align, start=1, end=1)
    }

    if (any(str_detect(align, "[[:alnum:]]"))){
      warning("`align` specified in `element_style` contains one or more alphanumeric characters. Results may not be as expected.")
    }

    align <- ifelse(str_detect(align, "[[:alnum:]]"), paste0("\"", align, "\""), paste0("\\", align))
    align <- paste(align, collapse = "|")
    align <- paste0("(?=[", align, "])")

    tbl_dat <-  tibble(col = trimws(col)) %>%
      separate(col, c("add_left","add_right"), sep = align, extra = "merge", fill = "right", remove = FALSE) %>%
      mutate(across(c(.data$add_left, .data$add_right), function(x) {
               replace_na(x, "") %>%
                 nchar() %>%
                 {max(.)-.} %>%
                 {str_dup(" ", .)}
             }))
  } else {

    tbl_dat <-  tibble(col = str_trim(col, side = "right")) %>%
      mutate(string_col = nchar(.data$col),
             string_tot = max(.data$string_col),
             space_to_add = str_dup(" ", .data$string_tot-.data$string_col))
    if(align == "left"){
      tbl_dat <- tibble(add_left = "",
             add_right = tbl_dat$space_to_add) %>%
        bind_cols(tbl_dat, .)
    } else {
      tbl_dat <- tibble(add_left = tbl_dat$space_to_add,
             add_right = "") %>%
        bind_cols(tbl_dat, .)
    }

  }

    str_c(tbl_dat$add_left,
        tbl_dat$col,
        tbl_dat$add_right)

}


#' Apply column style plan - alignment
#'
#' @param .data data
#' @param tfrmt_obj tfrmt object
#' @importFrom dplyr mutate across select tibble group_by slice n filter cur_column pull ungroup
#' @importFrom tidyr unnest
#' @importFrom tidyselect everything
#' @importFrom purrr map map_dfr discard
#' @importFrom rlang as_name
#' @importFrom tibble as_tibble_row
#' @importFrom forcats fct_inorder
#'
#' @noRd
apply_col_style_plan_alignment_values <- function(.data, tfrmt_obj){


  style_plan <- tfrmt_obj$col_style_plan

  if(is.null(style_plan)){
    return(.data)
  }

  alignment_style_plan <- style_plan %>%
    discard(function(x)is.null(x$align))

  if(length(alignment_style_plan) == 0){
    return(.data)
  }

  column <- tfrmt_obj$column
  values <- tfrmt_obj$values

  last_col <- column[[length(column)]]

  col_vals <- .data %>%
    pull(!!last_col) %>%
    unique()

  valid_data_col_vals <- setdiff(names(.data),c(column, values))

  ## allow identify alignment to which columns (where assigned)

  selections <-  alignment_style_plan %>%
    map(function(x) {

      ## check if selection applies to col vars or alternate columns
      col_vals <- map(x$col, function(ex) {

        val <- try(eval_tidyselect_on_colvec(ex, col_vals), silent = TRUE)
        if (inherits(val, "try-error")) {
          abort(
            paste0(
              "Variable `",
              as_label(ex),
              "` specified in element_style doesn't exist in the supplied dataset. Please check the tfrmt and try again."
            ),
            .call = FALSE
          )
        }
        return(val)
      }) %>%
        discard(is.null) %>%
        unlist() %>%
        unique()

      if(length(col_vals) > 0){
        x$col_eval <- col_vals
        return(x)
      }
    }) %>%
    discard(is.null)

  # keep the last col align for each col
  align_spec <- selections %>%
    map_dfr(~tibble(align = list(.x$align), column = list(.x$col_eval))) %>%
    unnest(.data$column) %>%
    ungroup() %>%
    group_by(.data$column) %>%
    slice(n()) %>%
    rename(!!last_col := .data$column)

  .data %>%
    left_join(align_spec, by = as_label(last_col)) %>%
    mutate(across(c(!!!column), fct_inorder)) %>%
    group_by(!!!column) %>%
    group_split() %>%
    map_dfr(function(x){
      if(!is.null(x$align[[1]])){
      x <-  x %>%
          mutate(!!values := apply_col_alignment(!!values, x$align[[1]]))
      }
      x
    }) %>%
    select(-.data$align) %>%
    mutate(across(c(!!!column), as.character))
}


#' Apply column style plan - alignment
#'
#' @param gt_table gt object
#' @param style_plan col_style_plan object
#'
#' @importFrom gt cols_width
#' @importFrom rlang is_empty
#' @importFrom stats as.formula
apply_gt_col_style_plan_widths <- function(gt_table, style_plan){

  for(el_style in style_plan){
    if(!is_empty(el_style$width)){
      cols <- map_chr(el_style[["col"]], as_label)
      col_width_formula_list <- list()

      for(col in cols){
        col_width_formula_list[[length(col_width_formula_list) + 1]] <-
          as.formula(paste0("`",col,"` ~ '",el_style$width,"'"))
      }
      gt_table <- cols_width(.data = gt_table,.list = col_width_formula_list)
    }
  }

  gt_table

}

