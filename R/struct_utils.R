
# Utilities ---------------------------------------------------------------

expr_to_filter <- function(cols, val){
  UseMethod("expr_to_filter", cols)
}

#' @importFrom stringr str_detect str_sub
#' @importFrom rlang as_label
expr_to_filter.quosure <- function(cols, val){
  ## If is missing a quosure, nothing to filter
  if(quo_is_missing(cols)){
    return("TRUE")
  }

  # This is all so it works when there is a list
  if(all(val == ".default")){
    out <- "TRUE"
  } else {
    val <- ifelse(str_detect(val, "^`.*`$"), str_sub(val, 2, -2), val)
    out <- as_label(cols) %>%
      paste0("`", ., "`") %>%
      paste0(" %in% c('",
             paste0(val, collapse = "', '"),
             "')")
  }
  out
}



#' @importFrom purrr map2_chr map_chr
expr_to_filter.quosures <- function(cols, val){

  if (is.null(val)){
    out <- "TRUE"
  } else if(!is.list(val) & length(cols) == 1){
    cols <- cols[[1]]
    out <- expr_to_filter(cols,val)
  } else if(!is.list(val) && all(val == ".default")){
    out <- "TRUE"
  }else if(!is.list(val)){
    stop("If multiple cols are provided, val must be a named list")
  }else{
    if(!all(names(val) %in% map_chr(cols, as_label))){
      stop("Names of val entries do not all match col values")
    }
    out <- map2_chr(cols, val[map_chr(cols, as_label)], ~ expr_to_filter(.x, .y)) %>%
      paste0(collapse = " & ")
  }
  out
}

#' Given a *_structure with specific group/label value(s) (i.e. non-default),
#' return the row indices corresponding to each unique value
#'
#' @param cur_struct current structure object
#' @param .data data to test against
#' @param group list of the group parameters
#' @param label label symbol should only be one
#'
#' @return list of row indices
#' @noRd
#'
#' @importFrom dplyr filter pull select mutate group_by group_split
#' @importFrom rlang parse_expr
#' @importFrom purrr map_lgl map
struct_val_idx <- function(cur_struct, .data, group, label){

  grp_expr <- "TRUE"
  lbl_expr <- "TRUE"
  keep_vars <- NULL

  # only do this if cur_struct contains a non-default value
  if (detect_non_default(cur_struct$group_val)){
    grp_expr <- expr_to_filter(group, cur_struct$group_val)

    if (!is.list(cur_struct$group_val)){
      keep_vars <- group
    } else {
      keep_vars <- group[map_lgl(cur_struct$group_val, ~!all(.x==".default"))]
    }
  }

  if (detect_non_default(cur_struct$label_val)){
    lbl_expr <- expr_to_filter(label, cur_struct$label_val)
    keep_vars <- c(keep_vars, label)
  }

  if (!is.null(keep_vars)){
    filter_expr <- paste(
      c(lbl_expr,grp_expr),
      collapse = "&"
    ) %>%
      parse_expr()

    .data %>%
      filter(!!filter_expr) %>%
      select(any_of(c(map_chr(keep_vars, as_label), "TEMP_row"))) %>%
      # split only after non-consecutive sequence
      mutate(breaks = .data$TEMP_row==lag(.data$TEMP_row, default = 0)+1,
             breaks = cumsum(!.data$breaks)) %>%
      group_by(.data$breaks) %>%
      group_split() %>%
      map(function(x) pull(x, .data$TEMP_row))

  } else {
    .data %>%
      pull(.data$TEMP_row) %>%
      list()
  }

}

# detect use of .default in a *_structure object
#' @importFrom purrr map_lgl
#' @noRd
detect_default <- function(struct){

  map_lgl(struct, ~ any(!is.null(.x) && any(.x==".default"))) %>% any()
}

# detect use of non-default in a  *_structure object entry
detect_non_default <- function(struct_val){

  any(!is.null(struct_val) && any(!struct_val==".default"))

}

#' Create the group_by expression for the data
#'
#' @param cur_struct current structure object
#' @param group list of the group parameters
#' @param label label symbol should only be one
#'
#' @return character vector of variable names to group by
#' @noRd
#'
#' @importFrom rlang as_label
#' @importFrom purrr map_lgl map_chr
expr_to_grouping <- function(cur_struct, group, label){

  grouping <- NULL

  if (!is.null(cur_struct$group_val)){
    if(!is.list(cur_struct$group_val) && all(cur_struct$group_val==".default")){
      grp_to_add <- map_chr(group, as_label)
      grouping <- c(grouping, grp_to_add)
    } else if (is.list(cur_struct$group_val) && any(cur_struct$group_val==".default")){
      grp_to_add <- names(cur_struct$group_val)[map_lgl(cur_struct$group_val, ~all(.x==".default"))]
      grouping <- c(grouping, grp_to_add)
    }
  }
  if (!is.null(cur_struct$label_val) && cur_struct$label_val==".default"){
    grouping <- c(grouping, as_label(label))
  }

  grouping %>% unname()
}
