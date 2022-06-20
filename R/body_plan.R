#' Table Body Plan
#'
#' Define the formatting of the body contents of the table through a series of
#' frmt_structures. Structures get applied in order from bottom up, so the last
#' added structure is the first applied.
#'
#' @seealso [frmt_structure()] defines which rows the formats will be applied
#'   to, and [frmt()], [frmt_combine()], and [frmt_when()] define the format
#'   semantics.
#'
#' @param ... list of frmt_structures defining the body formatting
#'
#' @return body_plan object
#'
#' @examples
#'
#'   example_tfrmt<- tfrmt(
#'       title = "Table Title",
#'       body_plan = body_plan(
#'         frmt_structure(
#'           group_val = c("group1"),
#'           label_val = ".default",
#'           frmt("XXX")
#'         )
#'       )
#'      )
#'
#' @export
#'
body_plan <- function(...){

  frmt_structure_list <- list(...)

  for(struct_idx in seq_along(frmt_structure_list)){
    if(!is_frmt_structure(frmt_structure_list[[struct_idx]])){
      stop(paste0("Entry number ",struct_idx," is not an object of class `frmt_structure`."))
    }
  }

  structure(
    frmt_structure_list,
    class = c("body_plan", "frmt_table")
  )
}


#' Build contents of body (group/label value-specific) plan based on significant digits specifications
#'
#' @param data significant digits data for a given set of group/label values
#' @param tfrmt tfrmt object
#' @param param_defaults parameter-level significant digits specifications
#' @param missing missing option to be included in all `frmt`s
#'
#' @return list of `frmt_structure` objects
#' @noRd
#' @importFrom stringr str_detect str_extract_all
#' @importFrom purrr map_dfr map map_chr quietly pmap_chr
#' @importFrom dplyr mutate group_by filter group_split select across
#' @importFrom tidyr unnest
#' @importFrom tidyselect everything
#' @importFrom rlang as_name quo_is_missing
body_plan_builder <- function(data, group, label, param_defaults, missing = NULL){

  # prep params for frmt functions
  param_tbl <- seq_along(param_defaults) %>%
    map_dfr(~tibble(param_display = names(param_defaults)[.x],
                    sigdig = list(param_defaults[[.x]] + data$sigdig[[1]]),
                    pos = .x)) %>%
    mutate(contains_glue = str_detect(.data$param_display, "\\{.*\\}"),  # is this to be a frmt_combine
           param = map2(.data$param_display, .data$contains_glue, ~ if(.y==TRUE){
             str_extract_all(.x, "(?<=\\{)[^\\}]+(?=\\})") %>% unlist
           } else {.x}),
           single_glue_to_frmt = pmap_chr(list(.data$contains_glue, .data$param, .data$param_display), function(a,b,c){
             if(a==TRUE & length(b) == 1) c else NA_character_
           } )) %>%
    unnest(everything()) %>%
    mutate(frmt_string = map2_chr(.data$sigdig, .data$single_glue_to_frmt, sigdig_frmt_string))

  frmt_vec <- param_tbl %>%
    group_by(.data$pos) %>%
    group_split() %>%
    map(function(x){
      if(sum(x$contains_glue)>1){
        frmt_combine_builder(x$param_display[[1]], x$param, x$frmt_string, missing)
      } else{
        frmt_builder(x$param, x$frmt_string, missing)
      }
    })

  frmt_vec <-do.call(c, frmt_vec)


  # group/label names from tfrmt
  grp_names <- if (length(group)==0) character(0) else group %>% map_chr(as_name)
  lbl_names <- if(quo_is_missing(label)) character(0) else as_name(label)

  # significant digits data spec
  grp_data <- data %>% select(-.data$sigdig)
  grp_data_names <- names(grp_data)
  sigdig <- data$sigdig[[1]]

  # check if names of data are group or label vars in the trfrmt
  if (!all(grp_data_names %in% c(grp_names, lbl_names))){
    message("Group/label variable names in sig. digits spec do not match tfrmt. Formatting may not be as expected.")

    grp_data_names <- intersect(grp_data_names, c(grp_names, lbl_names))
    grp_data <- grp_data[, grp_data_names]
  }

  if(length(grp_data_names)>0){

    which_grp <- which(grp_data_names %in% grp_names)
    which_lbl <- which(grp_data_names %in% lbl_names)

    if(length(which_grp)>0){
      group_val <- grp_data[,which_grp] %>%
        as.list() %>%
        map(unique) %>%
        map(~if (any(.x==".default")){ ".default"} else { .x})
    } else {
      group_val <- ".default"
    }


    if(length(which_lbl)>0){
      label_val <- grp_data[,which_lbl, drop = TRUE] %>% unique()
      label_val <- if(any(label_val==".default")){".default"} else {label_val}
    } else {
      label_val <- ".default"
    }

    frmt_structure_builder(group_val, label_val, frmt_vec)
  }
}
