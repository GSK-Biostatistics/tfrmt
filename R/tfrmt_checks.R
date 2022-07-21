
#' check in tfrmt that the column and col_plan are compatable
#' @noRd
#' @param x tfrmt to be checked
#' @importFrom rlang caller_call abort

#'
check_column_and_col_plan <- function(x){
  multi_column_defined <- length(x$column) > 1
  span_structures_defined <- if(!is.null(x$col_plan)){
    !is.null(x$col_plan$span_structures)
  }else{
    FALSE
  }

  if(multi_column_defined & span_structures_defined){
    abort(
      paste0(
        "Multiple columns defined in `column` argument of tfrmt ",
        "as well as span_structures in `col_plan`.\n",
        "The use of only one approach is permitted. ",
        "Select a single column or remove span_structures from `col_plan()`"
      ),
      class = "_tfrmt_invalid_col_plan"
    )
  }

}

#' check that in tfrmt that the group var and body_plan is consistent
#' @noRd
#' @param x tfrmt to be checked
#'
#' @importFrom rlang caller_call is_empty as_label abort
#' @importFrom purrr map_chr
#'
check_group_var_consistency <- function(x){

  if(!is.null(x$body_plan) & !is_empty(x$group)){

    is_invalid_body_plan <- FALSE
    is_invalid_body_plan_message <- "Inconsistencies between group and body_plan"
    group_as_char <- map_chr(x$group, as_label)

    for(idx in seq_along(x$body_plan)){
      frmt_struct <- x$body_plan[[idx]]
      ## if the group_vars is a list, check that the names are matching group vars
      if(is.list(frmt_struct$group_val)){

        struct_groups <- names(frmt_struct$group_val)
        invalid_group_idx <- !struct_groups %in% group_as_char

        if(any(invalid_group_idx)){
          is_invalid_body_plan <- TRUE
          invalid_groups <- struct_groups[invalid_group_idx]
          is_invalid_body_plan_message <- c(
            is_invalid_body_plan_message,
            paste0(
              paste0("Invalid Format Structure in body_plan at position `",idx,"`:\n"),
              paste0("  Malformed Group: ",paste0(invalid_groups, collapse = ", "),"\n"),
              paste0("  ", format(frmt_struct), collapse = "\n")
              )
          )
        }
      }
    }


    if(is_invalid_body_plan){
      abort(
        is_invalid_body_plan_message ,
        class = "_tfrmt_invalid_body_plan"
        )
    }
  }

}


#' check that in tfrmt that only the top level group gets styling if
#' row_grp_plan has and body_plan is consistent
#' @noRd
#' @param x tfrmt to be checked
#'
#' @importFrom rlang caller_call is_empty as_label abort
#' @importFrom purrr map_chr
#'
check_col_style_row_grp_consistency <- function(x){

  if(!is_empty(x$group) & !is.null(x$col_style_plan) & !is_empty(x$row_grp_plan)){

    is_invalid_plan <- FALSE
    is_invalid_plan_message <- "Invalid Entries based on col_align_plan and row_grp_plan"
    group_as_char <- map_chr(x$group, as_label)

    r_grp_plan_col_loc <- x$row_grp_plan$label_loc$location

    col_align_plan_as_char <- x$col_style_plan %>%
      lapply(function(x) x$col %>% map_chr(as_label))

    for(cap_vars_idx in seq_along(col_align_plan_as_char)){
      grp_in_cap <- group_as_char %in% col_align_plan_as_char[[cap_vars_idx]]
      if(r_grp_plan_col_loc == "column" & any(grp_in_cap[-1])){
        is_invalid_plan <- TRUE
        invalid_groups <- setdiff(group_as_char[grp_in_cap], group_as_char[1])
        is_invalid_plan_message <- c(
          is_invalid_plan_message,
          paste0(
            paste0("Invalid element_col in row_grp_plan at position `",cap_vars_idx,"`:\n"),
            paste0("  `col` value",ifelse(length(invalid_groups) > 1,"s",""),": ",paste0(invalid_groups, collapse = ", "),"\n"),
            paste0("  When row_grp_plan label location is `column`, only the only valid group col to style is `",group_as_char[1],"`\n")
          )
        )
      }else if(any(grp_in_cap[-1])){
        is_invalid_plan <- TRUE
        invalid_groups <- group_as_char[grp_in_cap]
        is_invalid_plan_message <- c(
          is_invalid_plan_message,
          paste0(
            paste0("Invalid element_col in row_grp_plan at position `",cap_vars_idx,"`:\n"),
            paste0("  `col` value",ifelse(length(invalid_groups) > 1,"s",""),": ",paste0(invalid_groups, collapse = ", "),"\n")
          )
        )
      }
    }

    if(is_invalid_plan){
      abort(
        is_invalid_plan_message ,
        class = "_tfrmt_invalid_row_grp_col_style_plan"
      )
    }

  }
}
