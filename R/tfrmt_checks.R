
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
      "Multiple columns defined in `column` argument of tfrmt ",
      "as well as span_structures in `col_plan`.\n",
      "The use of only one approach is permitted. ",
      "Select a single column or remove span_structures from `col_plan()`",
      call = caller_call()
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
    is_invalid_body_plan_message <- ""
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
              "\n",
              paste0("Invalid Format Structure in body_plan at position `",idx,"`:\n"),
              paste0("  Malformed Group: ",paste0(invalid_groups, collapse = ", "),"\n"),
              paste0("  ", format(frmt_struct), collapse = "\n")
              )
          )
        }
      }
    }

    if(is_invalid_body_plan){
      abort(is_invalid_body_plan_message, call = caller_call())
    }
  }

}
