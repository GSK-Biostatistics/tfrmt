
#' check in tfrmt that the column and col_plan are compatable
#' @noRd
#' @param x tfrmt to be checked
#' @importFrom rlang caller_call abort

#'
check_column_and_col_plan <- function(x){

  multi_column_defined <- length(x$column) > 1
  if(!is.null(x$col_plan)){
    span_strucs_idx <- sapply(x$col_plan$dots, is_span_structure)
    span_structures_defined <- any(span_strucs_idx)
    span_structs <- x$col_plan$dots[span_strucs_idx]
  }else{
    span_structures_defined <-  FALSE
    span_structs <- NULL
  }

  if(!multi_column_defined & span_structures_defined){

    if(length(x$column) == 1){
      n_col_desc <- "A single column defined in `column` argument of tfrmt "
    }else{
      n_col_desc <- "No columns defined in the `column` argument of tfrmt "
    }

    abort(
      paste0(
        n_col_desc,
        "but provided a span_structure() in `col_plan`.\n",
        "span_structure()'s are only valid when multiple `column` values are provided.\n",
        "Add values to `column` or remove span_structures from `col_plan()`"
      ),
      class = "_tfrmt_invalid_col_plan"
    )
  }

  if(span_structures_defined){
    column_strings <- map_chr(x$column, as_label)

    for(struct in span_structs){
      if(!all(vals <- names(struct) %in% column_strings)){
        abort(
          "Columns defined in `span_structure` are not defined columns in the tfrmt",
          body = paste0(
            "Column Values: ", paste0("`",column_strings,"`", collapse = ", " ),
            "\n",
            "Invalid Column Names in Span Structure: ",  paste0("`",names(struct)[!vals],"`", collapse = ", " )
          ),
          class = "_tfrmt_invalid_span_structure_col"
        )
      }
    }
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
  if(!is_empty(x$group)){

    group_var_consistency_message <- trimws(
      paste0(c(
        check_group_var_consistency_body_plan(x),
        check_group_var_consistency_row_grp_plan(x),
        check_group_var_consistency_footnote_plan(x)
      ),
      collapse = "\n"
      )
    )

    if(!identical(group_var_consistency_message,"")){
      abort(
        group_var_consistency_message ,
        class = "_tfrmt_mismatched_group_vals"
      )
    }

  }

}

check_group_var_consistency_body_plan <- function(x){
  if(!is.null(x$body_plan)){

    is_invalid_body_plan <- FALSE
    is_invalid_body_plan_message <- "Inconsistencies between group and body_plan"
    group_as_char <- map_chr(x$group, as_label)

    for(idx in seq_along(x$body_plan)){
      struct <- x$body_plan[[idx]]
      ## if the group_vars is a list, check that the names are matching group vars
      if(is.list(struct$group_val)){

        struct_groups <- names(struct$group_val)
        invalid_group_idx <- !struct_groups %in% group_as_char

        if(any(invalid_group_idx)){
          is_invalid_body_plan <- TRUE
          invalid_groups <- struct_groups[invalid_group_idx]
          is_invalid_body_plan_message <- c(
            is_invalid_body_plan_message,
            paste0(
              paste0("Invalid Format Structure in body_plan at position `",idx,"`:\n"),
              paste0("  Malformed Group: ",paste0(invalid_groups, collapse = ", "),"\n")
            )
          )
        }
      }
    }


    if(is_invalid_body_plan){
      is_invalid_body_plan_message
    }
  }
}

check_group_var_consistency_row_grp_plan <- function(x){

  if(!is.null(x$row_grp_plan)){

    is_invalid_row_grp_plan <- FALSE
    is_invalid_row_grp_plan_message <- "Inconsistencies between group and row_grp_plan row_grp_structures"
    group_as_char <- map_chr(x$group, as_label)

    for(idx in seq_along(x$row_grp_plan$struct_ls)){
      struct <- x$row_grp_plan$struct_ls[[idx]]
      ## if the group_vars is a list, check that the names are matching group vars
      if(is.list(struct$group_val)){

        struct_groups <- names(struct$group_val)
        invalid_group_idx <- !struct_groups %in% group_as_char

        if(any(invalid_group_idx)){
          is_invalid_row_grp_plan <- TRUE
          invalid_groups <- struct_groups[invalid_group_idx]
          is_invalid_row_grp_plan_message <- c(
            is_invalid_row_grp_plan_message,
            paste0(
              paste0("Invalid Format Structure in row_grp_plan for row_grp_structure `",idx,"`:\n"),
              paste0("  Malformed Group: ",paste0(invalid_groups, collapse = ", "),"\n")
            )
          )
        }
      }
    }


    if(is_invalid_row_grp_plan){
        is_invalid_row_grp_plan_message
    }
  }
}

check_group_var_consistency_footnote_plan <- function(x){

  if(!is.null(x$footnote_plan)){

    is_invalid_footnote_plan <- FALSE
    is_invalid_footnote_plan_message <- "Inconsistencies between group and footnote_plan footnote_structures"
    group_as_char <- map_chr(x$group, as_label)

    for(idx in seq_along(x$footnote_plan$struct_list)){
      struct <- x$footnote_plan$struct_list[[idx]]
      ## if the group_vars is a list, check that the names are matching group vars
      if(is.list(struct$group_val)){

        struct_groups <- names(struct$group_val)
        invalid_group_idx <- !struct_groups %in% group_as_char

        if(any(invalid_group_idx)){
          is_invalid_footnote_plan <- TRUE
          invalid_groups <- struct_groups[invalid_group_idx]
          is_invalid_footnote_plan_message <- c(
            is_invalid_footnote_plan_message,
            paste0(
              paste0("Invalid Format Structure in footnote_plan for footnote_structure `",idx,"`:\n"),
              paste0("  Malformed Group: ",paste0(invalid_groups, collapse = ", "),"\n")
            )
          )
        }
      }
    }


    if(is_invalid_footnote_plan){
      is_invalid_footnote_plan_message
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
      if(length(x$col_style_plan[[cap_vars_idx]]$col) == 0){
        stop("Column element is missing from col_style_structure. Note: col here refers to the values within the column variable in your data, rather than the variable name itself")
      }

      if(r_grp_plan_col_loc == "column" & any(grp_in_cap[-1])){
        is_invalid_plan <- TRUE
        invalid_groups <- setdiff(group_as_char[grp_in_cap], group_as_char[1])
        is_invalid_plan_message <- c(
          is_invalid_plan_message,
          paste0(
            paste0("Invalid col_style_structure in row_grp_plan at position `",cap_vars_idx,"`:\n"),
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
            paste0("Invalid col_style_structure in row_grp_plan at position `",cap_vars_idx,"`:\n"),
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





check_footnote_plan <- function(x){




  if(!is_empty(x$footnote_plan)){
  for(i in 1:length(x$footnote_plan$struct_list)){
    # if multiple columns then column_val must be a named list
    if(length(x$column)>1 && is.list(x$footnote_plan$struct_list[[i]]$column_val)==FALSE && is.null(x$footnote_plan$struct_list[[i]]$column_val)==FALSE){
      stop("when tfrmt contains multiple columns, column_val must be a named list")
    }

    # if multiple groups then group_val must be a named list
    if(length(x$group)>1 && is.list(x$footnote_plan$struct_list[[i]]$group_val)==FALSE && is.null(x$footnote_plan$struct_list[[i]]$group_val)==FALSE){
      stop("when tfrmt contains multiple groups, group_val must be a named list")
    }
  }
    }


}

