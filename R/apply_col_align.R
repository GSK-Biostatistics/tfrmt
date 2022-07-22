#' Apply alignment to a column
#'
#' @param col Character vector containing data values
#' @param align Vector of one or more strings to align on. Strings should be 1
#'   character in length and not alphanumeric (i.e. spaces or punctuation). If
#'   NULL, data values will be aligned on the first occurrence of a decimal
#'   place or space. If more than one character is provided, alignment will be
#'   based on the first occurrence of any of the characters. For alignment based
#'   on white space, leading white spaces will be ignored.
#' @param column_name name alignment being applied to. Defaults to NULL
#'
#' @return Character vector containing aligned data values
#'
#' @importFrom tidyr separate replace_na
#' @importFrom dplyr mutate across pull tibble
#' @importFrom stringr str_dup str_c str_trim
#' @importFrom rlang warn
#' @noRd
apply_col_alignment <- function(col, align, column_name = NULL){

  if (!all(align %in% c("left","right"))){

    if (!all(nchar(align)==1)){
      message(paste0(
        "Alignment specified",
        if(!is.null(column_name)){
          paste0(" to apply to column `",column_name,"`")
        },
        " contains strings with >1 characters. Only the first character will be used."
      ))
      align <- str_sub(align, start=1, end=1)
    }

    if (any(str_detect(align, "[[:alnum:]]"))){
      warn(
        paste0(
          "Alignment specified",
          if(!is.null(column_name)){
            paste0(" to apply to column `",column_name,"`")
          },
          " contains one or more alphanumeric characters. Results may not be as expected."
        )
      )
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


#' Apply column style plan - alignment - value cols
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

  valid_data_col_vals <- setdiff(names(.data),c(column, values) %>% map_chr(as_label))

  ## allow identify alignment to which columns (where assigned)

  selections <-  alignment_style_plan %>%
    map(function(x) {

      ## check if selection applies to col vars or alternate columns
      col_vals <- map(x$col, function(ex) {

        val <- try(eval_tidyselect_on_colvec(ex, col_vals), silent = TRUE)
        if (inherits(val, "try-error")) {
          valid_data_cols <- try(eval_tidyselect_on_colvec(ex, valid_data_col_vals), silent = TRUE)
          if(inherits(valid_data_cols, "try-error")){
            abort(
                paste0(
                  "Variable `",
                  as_label(ex),
                  "` specified in element_col doesn't exist in the supplied dataset. Please check the tfrmt and try again."
                ),
                .call = FALSE
              )
          }else{
            return(NULL)
          }
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
        column_name <- x %>%
          pull(!!last_col) %>%
          unique()

        x <-  x %>%
          mutate(!!values := apply_col_alignment(!!values, x$align[[1]], column_name = column_name))
      }
      x
    }) %>%
    select(-.data$align) %>%
    mutate(across(c(!!!column), as.character))
}

#' Apply column style plan - alignment - non-value cols
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
apply_col_style_plan_alignment_non_values <- function(.data, tfrmt_obj, non_data_cols, data_cols){

  style_plan <- tfrmt_obj$col_style_plan

  if(is.null(style_plan)){
    return(.data)
  }

  alignment_style_plan <- style_plan %>%
    discard(function(x)is.null(x$align))

  if(length(alignment_style_plan) == 0){
    return(.data)
  }

  ## allow identify alignment to which columns (where assigned)

  selections <-  alignment_style_plan %>%
    map(function(x) {

      ## check if selection applies to col vars or alternate columns
      col_vals <- map(x$col, function(ex) {
        val <- try(eval_tidyselect_on_colvec(ex, non_data_cols), silent = TRUE)

        if (!inherits(val, "try-error")) {
          val <- setdiff(val, data_cols)
          return(val)
        }else{
          valid_data_cols <- try(eval_tidyselect_on_colvec(ex, data_cols), silent = TRUE)
          if(inherits(valid_data_cols, "try-error")){
            abort(
              paste0(
                "Variable `",
                as_label(ex),
                "` specified in element_col doesn't exist in the supplied dataset. Please check the tfrmt and try again."
              ),
              .call = FALSE
            )
          }
        }
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

  if(length(selections) > 0){

    # keep the last col align for each col
    align_spec <- selections %>%
      map_dfr(~tibble(align = list(.x$align), column = list(.x$col_eval))) %>%
      unnest(.data$column) %>%
      ungroup() %>%
      group_by(.data$column) %>%
      slice(n())

    for(align_idx in seq_len(nrow(align_spec))){

      align_def_align <- align_spec[align_idx, "align"][[1]][[1]]
      align_def_col <- as_length_one_quo(align_spec[align_idx, "column"][[1]])

      .data <- .data %>%
        mutate(
          !!align_def_col := apply_col_alignment(
              !!align_def_col,
              align_def_align,
              as_label(align_def_col)
            )
        )

    }
  }

  return(.data)

}


#' Apply column style plan - Widths
#'
#' @param gt_table gt object
#' @param style_plan col_style_plan object
#'
#' @importFrom gt cols_width
#' @importFrom rlang is_empty
#' @importFrom stats as.formula
#'
#' @noRd
apply_col_style_plan_widths <- function(.data, style_plan){

  style_plan <- style_plan %>%
    discard(function(x)is.null(x$width))

  if(length(style_plan) == 0){
    # if no col width changes, return value directly
    return(.data)
  }

  style_plan <- seq_along(style_plan) %>%
    map(function(i){
      style_plan[[i]]$style_num <- i
      style_plan[[i]]
    })

  .data_names <- names(.data)

  width_to_apply_to_col <- style_plan %>%
    map_dfr(function(el_style) {
      if (!is_empty(el_style$width)) {
        cols_to_apply <- el_style[["col"]] %>%
          map_chr( ~ eval_tidyselect_on_colvec(.x, .data_names))
        tibble(cols = cols_to_apply,
               width = el_style$width,
               style_idx = el_style$style_num)
      } else{
        NULL
      }
    }) %>%
    group_by(cols) %>%
    arrange(cols, desc(.data$style_idx)) %>%
    slice(1) %>%
    group_split()

  for(col_width in width_to_apply_to_col){
    col_sym <- quo(!!sym(col_width$cols))
    .data <- .data %>%
      mutate(!!col_sym := construct_wrapped_string(!!col_sym, col_width$width))
  }

  .data

}


#' Split strings into n_char wide
#'
#' @param x string vector to wrap
#' @param n_char width to split x to
#'
#' @importFrom purrr map_chr
#'
#' @noRd
construct_wrapped_string <- function(x, n_char = Inf){
  x %>%
    strsplit("\\n") %>%
    map_chr(function(z){
      word_list <- stringi::stri_wrap(z, width = n_char, normalize = FALSE)
      do.call("paste0", list(word_list, collapse ="\n"))
    })
}

