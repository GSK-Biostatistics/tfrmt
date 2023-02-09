#' Row Group Plan
#'
#' Define the look of the table groups on the output. This function allows you to
#' add spaces after blocks and allows you to control how the groups are viewed
#' whether they span the entire table or are nested as a column.
#'
#' @seealso [row_grp_structure()] for more details on how to specify row group
#'   structures, [element_block()] for more details on how to specify spacing
#'   between each group, [element_row_grp_loc()] for more details on how to
#'   specify whether row group titles span the entire table or collapse.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/row_grp_plan.html}{Link to related article}
#'
#' @param ...  Row group structure objects separated by commas
#' @param label_loc [element_row_grp_loc()] object specifying location
#'
#'
#' @return row_grp_plan object
#'
#' @examples
#'
#'
#'   ## single grouping variable example
#'   sample_grp_plan <- row_grp_plan(
#'     row_grp_structure(group_val = c("A","C"), element_block(post_space = "---")),
#'     row_grp_structure(group_val = c("B"), element_block(post_space = " ")),
#'     label_loc = element_row_grp_loc(location = "column")
#'   )
#'
#'   ## example with multiple grouping variables
#'   sample_grp_plan <- row_grp_plan(
#'      row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " ")),
#'      label_loc = element_row_grp_loc(location = "spanning")
#'      )
#'
#' @export
#'
row_grp_plan <- function(..., label_loc = element_row_grp_loc(location = "indented")){

  row_grp_structure_list <- list(...)

  for(struct_idx in seq_along(row_grp_structure_list)){
    if(!is_row_grp_structure(row_grp_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `row_grp_structure`.
                  If you want specify `spanning_label` please enter 'spanning_label ='"))
    }
  }

  structure(
    list(struct_ls = row_grp_structure_list, label_loc = label_loc),
    class = c("row_grp_plan", "frmt_table")
  )
}

#' Row Group Structure Object
#'
#' Function needed to create a row_grp_structure object, which is a building block
#' of [row_grp_plan()]
#'
#' @seealso [row_grp_plan()] for more details on how to group row group
#'   structures, [element_block()] for more details on how to specify spacing
#'   between each group.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/row_grp_plan.html}{Link to related article}
#'
#' @param group_val A string or a named list of strings which represent the
#'   value of group should be when the given frmt is implemented
#' @param element_block element_block() object to define the block styling
#'
#' @returns row_grp_structure object
#' @export
#' @examples
#'
#' ## single grouping variable example
#' row_grp_structure(group_val = c("A","C"), element_block(post_space = "---"))
#'
#' ## example with multiple grouping variables
#' row_grp_structure(group_val = list(grp1 = "A", grp2 = "b"), element_block(post_space = " "))
#'
row_grp_structure <- function(group_val = ".default", element_block){

  if(!is_element_block(element_block)){
    stop("element_block, must be an element_block type")
  }

  if(is.list(group_val)){
    group_val_names <- names(group_val)
    if(is.null(group_val_names)){
      stop("when group_val is a list, must be a named list")
    }else if(any(group_val_names == "")){
      stop("when group_val is a list, each entry must be named")
    }
  }

  structure(
    list(
      group_val = group_val,
      block_to_apply = element_block),
    class = c("row_grp_structure","frmt_table")
  )
}
