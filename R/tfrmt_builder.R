
#' Create frmt character string from significant digits spec
#'
#' @param sigdig Number of significant digits to add to default setting for that param
#'
#' @return formatted spec as character string
#' @noRd
#'
#' @importFrom stringr str_dup
sigdig_frmt_string <- function(sigdig = 2) {
  paste0("xxx.",str_dup("x", sigdig))
}


#' Build frmt for a given parameter
#'
#' @param param `param` value
#' @param frmt_string formatted expression
#'
#' @return character string representing `frmt` object with `param` value as name
#' @noRd
#' @importFrom purrr map2
frmt_builder <- function(param, frmt_string) {
  map2(param, frmt_string, ~ paste0(as_name(.x), " = frmt('", .y, "')")) %>%
    paste(., collapse = ", ")
}

#' Build frmt_combine for a given set of parameters
#'
#' @param param_combine character string representing how `param` values will be
#'   combined using `glue::glue()` syntax
#' @param param vector of `param` values
#' @param frmt_string vector of formatted expressions
#'
#' @return character string representing `frmt_combine` object
#' @noRd
frmt_combine_builder <- function(param_combine, param, frmt_string){
  frmts <- frmt_builder(param, frmt_string)

  paste0("frmt_combine('", param_combine, "', ", frmts, ")")
}

#' Build format structure from a list of `frmt` and `frmt_combine` objects
#'
#' @param group_val A string or a named list of strings which represent the value of group should be when the given frmt is implemented
#' @param label_val A string which represent the value of label should be when the given frmt is implemented
#' @param frmt_list List of `frmt` and/or `frmt_combine` objects to be applied to the group_val/label_val combination
#'
#' @return list of `frmt_structure` objects
#' @noRd
#' @importFrom purrr map_chr map
#' @importFrom rlang parse_expr
frmt_structure_builder <- function(group_val, label_val, frmt_list){
  map_chr(frmt_list, ~ paste0("frmt_structure(group_val = ", group_val, ", label_val = ", label_val, ", ", .x, ")")) %>%
    map(parse_expr)
}

#' Set custom parameter-level significant digits rounding
#'
#' @param ... Series of name-value pairs, optionally formatted using
#'   `glue::glue()` syntax (note `glue` syntax is required for combined
#'   parameters). For combined parameters (e.g., "{min}, {max}"), value should
#'   be a vector of the same length (e.g., c(1,1)).
#'
#' @details Type `param_set()` in console to view package defaults. Use of the
#'   function will add to the defaults and/or override included defaults of the
#'   same name.
#'
#' @examples
#' # view included defaults
#' param_set()
#'
#' # update the defaults
#' param_set("{mean} ({sd})" = c(2,3), "pct" = 1)
#'
#' @return list of default parameter-level significant digits rounding
#' @export
#' @importFrom purrr map_lgl
param_set <- function(...){
  args <-  list(...)

  if (length(args)>0){
    all_numeric_args <- map_lgl(args, is.numeric) %>% all()
    all_named_args <- names(args) %>% nchar() %>% all(.>0)
    if (!all_numeric_args || !all_named_args){
      stop("`param_set` entry must be named numeric vector.")
    }
  }

  # make list of all params: default + user specified
  param_list <- list(
    "max" = 0,
    "median" = 1,
    "{mean} ({sd})" = c(1,2)
  )

  c(param_list[which(!names(param_list) %in% names(args))],
    args)

}


#' Build contents of body (group/label value-specific) plan based on significant digits specifications
#'
#' @param data significant digits data for a given set of group/label values
#' @param tfrmt tfrmt object
#' @param param_set parameter-level significant digits specifications
#'
#' @return list of character strings representing `frmt_structure` objects
#' @noRd
#' @importFrom stringr str_detect str_extract_all
#' @importFrom purrr map_dfr map map_chr quietly
#' @importFrom dplyr mutate group_by filter group_split select across
#' @importFrom tidyr unnest
#' @importFrom tidyselect everything
body_plan_builder <- function(data, group, label, param_set){


  # prep params for frmt functions
  param_tbl <- seq_along(param_set) %>%
    map_dfr(~tibble(param_combine = names(param_set)[.x],
                    sig_dig = list(param_set[[.x]] + data$sig_dig),
                    pos = .x)) %>%
    mutate(combined = str_detect(param_combine, "\\{.*\\}"),  # is this to be a frmt_combine
           param = map2(param_combine, combined, ~ if(.y==TRUE){
             str_extract_all(.x, "(?<=\\{)[^\\}]+(?=\\})") %>% unlist
           } else {.x})) %>%
    unnest(everything()) %>%
    mutate(frmt_string = sigdig_frmt_string(sig_dig))

  frmt_list <- param_tbl %>%
    group_by(pos) %>%
    group_split() %>%
    map(function(x){
      if(all(x$combined==TRUE)){
        frmt_combine_builder(x$param_combine[[1]], x$param, x$frmt_string)
      } else{
        frmt_builder(x$param, x$frmt_string)
      }
    })

  # group/label names from tfrmt
  grp_names <- group %>% map_chr(as_name)
  lbl_names <- label %>% map_chr(as_name)

  # significant digits data spec
  grp_data <- data %>% select(-sig_dig)
  grp_data_names <- names(grp_data)
  sig_dig <- data$sig_dig

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
        {quietly(dput)(.)$result} %>% # so it won't print to console
        deparse()
    } else {
      group_val <- "'.default'"
    }

    if(length(which_lbl)>0){
      label_val <- grp_data[,which_lbl, drop = TRUE] %>%
        mutate(across(everything(), ~ paste0("'", .x, "'")))
    } else {
      label_val <- "'.default'"
    }

    frmt_structure_builder(group_val, label_val, frmt_list)
  }
}

#' Create tfrmt object from data formatting spec
#'
#' @param tfrmt_obj a tfrmt object to base this new format off of
#' @param data data formatting spec with 1 record per group/label value, and
#'   columns for relevant group and/or label variables, and supported
#'   formatting-specific variables. Currently only supports significant digits
#'   formatting - supply a numeric column `sigdig` containing the signficant
#'   digits rounding to be applied in addition to the default.
#' @param group what are the grouping vars of the input dataset
#' @param label what is the label column of the input dataset
#' @param param_set Option to override or add to default parameters.
#' @param missing missing option to be included in all `frmt`s
#' @param ... dots are for future extensions and must be empty
#'
#' @return `tfrmt` object
#' @noRd
#' @importFrom dplyr rowwise group_split
#' @importFrom purrr map
tfrmt_builder <- function(tfrmt_obj = NULL, data, group, label, param_set = param_set(), missing = NULL, ...){

  tfrmt_el <- tfrmt_find_args(...)

  frmt_structure_list <- data %>%
    rowwise() %>%
    group_split() %>%
    map(body_plan_builder, tfrmt_el$group, tfrmt_el$label, tfrmt_el$param_set)

  bp <- frmt_structure_list %>%
    do.call("c",.) %>%
    do.call("body_plan", .)

  new_tfrmt <- tfrmt(group = tfrmt_el$group,
        label = tfrmt_el$label,
        body_plan = bp)

  # combine with previous tfrmt, if applicable
  if(!missing(tfrmt_obj)){
    new_tfrmt <- layer_tfrmt(
      tfrmt_obj,
      new_tfrmt
    )
  }

  new_tfrmt
}
