
#' Create frmt character string from significant digits spec
#'
#' @param sigdig Number of significant digits to add to default setting for that param
#' @param single_glue_to_frmt optional glue syntax for standalone (non-combined) parameters. NA if not applicable
#'
#' @return formatted spec as character string
#' @noRd
#'
#' @importFrom stringr str_dup str_replace
sigdig_frmt_string <- function(sigdig = 2, single_glue_to_frmt) {

  if (is.na(sigdig)){
    frmted_string <- "xxx"
  } else {
    frmted_dec <- str_dup("x", sigdig)
    if (!frmted_dec==""){
      frmted_dec <- paste0(".", frmted_dec)
    }
    frmted_string <- paste0("xxx", frmted_dec)
  }

  if (is.na(single_glue_to_frmt)){
     frmted_string
  } else {
    str_replace(single_glue_to_frmt, "\\{.*\\}", frmted_string)
  }
}


#' Build frmt for a given parameter
#'
#' @param param `param` value
#' @param frmt_string formatted expression
#' @param missing missing option to be included in all `frmt`s
#'
#' @return character string representing `frmt` object with `param` value as name
#' @noRd
#' @importFrom purrr map2
frmt_builder <- function(param, frmt_string, missing = NULL) {

  if(!missing(param)){
    frmt_string <- setNames(frmt_string, param)
  }

  map(frmt_string, function(x, missing_val) {
    do.call(frmt, list(expression = x, missing = missing_val ))
  }, missing_val = missing)

}

#' Build frmt_combine for a given set of parameters
#'
#' @param param_combine character string representing how `param` values will be
#'   combined using `glue::glue()` syntax
#' @param param vector of `param` values
#' @param frmt_string vector of formatted expressions
#' @param missing missing option to be included in all `frmt`s
#'
#' @return character string representing `frmt_combine` object
#' @noRd
frmt_combine_builder <- function(param_combine, param, frmt_string, missing = NULL){

  frmts <- frmt_builder(param, frmt_string, missing)

  list(do.call(frmt_combine, c(expression = param_combine, frmts, missing = missing)))
}

#' Build format structure from a list of `frmt` and `frmt_combine` objects
#'
#' @param group_val A string or a named list of strings which represent the value of group should be when the given frmt is implemented
#' @param label_val A string which represent the value of label should be when the given frmt is implemented
#' @param frmt_vec Character vector of `frmt` and/or `frmt_combine` objects to be applied to the group_val/label_val combination
#'
#' @return list of `frmt_structure` objects
#' @noRd
#' @importFrom purrr pmap
frmt_structure_builder <- function(group_val, label_val, frmt_vec){

  grp_lbl_list <- list(list(group_val = group_val, label_val = label_val))
  frmt_vec_list <- map2(names(frmt_vec), frmt_vec, ~list(param = .x %||% "", frmt = .y))

  crossing(frmt_vec_list,
           grp_lbl_list) %>%
    purrr::pmap(function(frmt_vec_list, grp_lbl_list){

      if(is.list(grp_lbl_list$group_val) & length(grp_lbl_list$group_val) == 1 & is.null(names(grp_lbl_list$group_val))){
        grp_lbl_list$group_val <- grp_lbl_list$group_val[[1]]
      }

      if(is.list(grp_lbl_list$label_val) & length(grp_lbl_list$label_val) == 1& is.null(names(grp_lbl_list$label_val))){
        grp_lbl_list$label_val <- grp_lbl_list$label_val[[1]]
      }

      arg_list <- list(grp_lbl_list$group_val,  grp_lbl_list$label_val, frmt_vec_list$frmt)
      names(arg_list) <- c("group_val","label_val",frmt_vec_list$param)

      do.call(frmt_structure, arg_list)
    }) %>%
    unname()

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
    all_numeric_args <- map_lgl(args, is.numeric)  %>% all()
    all_named_args <- names(args) %>% nchar() %>% all(.>0)
    if (!all_numeric_args || !all_named_args){
      stop("`param_set` entry must be named numeric vector.")
    }
  }

  # make list of all params: default + user specified
  param_list <- list(
    "min" = 1,
    "max" = 1,
    "median" = 1,
    "{mean} ({sd})" = c(1,2),
    "n" = NA
  )

  c(param_list[which(!names(param_list) %in% names(args))],
    args)

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

#' Create tfrmt object from significant digits spec
#'
#' @param data data formatting spec with 1 record per group/label value, and
#'   columns for relevant group and/or label variables, as well as a numeric
#'   column `sigdig` containing the significant digits rounding to be applied in
#'   addition to the default. If unique group/label values are represented in
#'   multiple rows, this will result in only one of the `sigdig` values being
#'   carried through in implementation.
#' @param group what are the grouping vars of the input dataset
#' @param label what is the label column of the input dataset
#' @param param_defaults Option to override or add to default parameters.
#' @param missing missing option to be included in all `frmt`s
#' @param ... These dots are for future extensions and must be empty.
#'
#' @return `tfrmt` object with a `body_plan` constructed based on the
#'   significant digits data spec and param-level significant digits defaults.
#'
#' @details Currently covers specifications for `frmt` and `frmt_combine`.
#'   `frmt_when` not supported and must be supplied in additional `tfrmt` that
#'   is layered on.
#' @noRd
#' @importFrom dplyr rowwise group_split
#' @importFrom purrr map
tfrmt_sigdig <- function(data, group, label, param_defaults = param_set(), missing = NULL, ...){

  tfrmt_inputs <-  quo_get(c("group","label"), as_var_args = "group", as_quo_args = "label")

  frmt_structure_list <- data %>%
    group_by(.data$sigdig) %>%
    group_split() %>%
    map(body_plan_builder, tfrmt_inputs$group, tfrmt_inputs$label, param_defaults, missing = NULL)

  bp <- frmt_structure_list %>%
    do.call("c",.)   %>%
    do.call("body_plan", .)


  tfrmt(
    group = tfrmt_inputs$group,
    label = tfrmt_inputs$label,
    body_plan = bp)

}
