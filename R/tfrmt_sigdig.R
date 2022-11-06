
#' Create frmt character string from significant digits spec
#'
#' @param sigdig Number of significant digits to add to default setting for that param
#' @param single_glue_to_frmt optional glue syntax for stand-alone (non-combined) parameters. NA if not applicable
#'
#' @return formatted spec as character string
#' @noRd
#'
#' @importFrom stringr str_dup str_replace
sigdig_frmt_string <- function(sigdig = 2, single_glue_to_frmt) {

  if (is.na(sigdig)){
    frmted_string <- "x"
  } else {
    frmted_dec <- str_dup("x", sigdig)
    if (!frmted_dec==""){
      frmted_dec <- paste0(".", frmted_dec)
    }
    frmted_string <- paste0("x", frmted_dec)
  }

  if (is.na(single_glue_to_frmt)){
     frmted_string
  } else {
    str_replace(single_glue_to_frmt, "\\{.*\\}", frmted_string)
  }
}


#' Set custom parameter-level significant digits rounding
#'
#' @param ... Series of name-value pairs, optionally formatted using
#'   `glue::glue()` syntax (note `glue` syntax is required for combined
#'   parameters).The name represents the parameter and the value represents the number of places to round the parameter to.
#'   For combined parameters (e.g., "{min}, {max}"), value should
#'   be a vector of the same length (e.g., c(1,1)).
#'
#' @details Type `param_set()` in console to view package defaults. Use of the
#'   function will add to the defaults and/or override included defaults of the
#'   same name. For values that are integers, use `NA` so no decimal places will
#'   be added.
#'
#' @examples
#' # View included defaults
#' param_set()
#'
#' # Update the defaults
#' param_set("{mean} ({sd})" = c(2,3), "pct" = 1)
#'
#' # Separate mean and SD to different lines
#' param_set("mean" = 2, "sd" = 3)
#'
#' # Add formatting using the glue syntax
#' param_set("{pct} %" = 1)
#'
#' @return list of default parameter-level significant digits rounding
#' @export
#' @importFrom purrr map_lgl map2_lgl
param_set <- function(...){
  args <-  list(...)

  if (length(args)>0){
    all_numeric_args <- map_lgl(args, ~is.numeric(.) || is.na(.))  %>% all()
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

  # determine if any existing params need to be overwritten

  args_params <- c(names(args), str_extract_all(names(args), "(?<=\\{)[^\\}]+(?=\\})") %>% unlist)

  idx_drop <- seq_along(param_list) %>%
    map_dfr(~tibble(param_display = names(param_list)[.x],
                    params = str_extract_all(.data$param_display, "(?<=\\{)[^\\}]+(?=\\})"))) %>%
    mutate(idx = row_number()) %>%
    unnest("params", keep_empty = TRUE) %>%
    mutate(drop = map2_lgl(.data$param_display, .data$params, ~  (.x %in% args_params || .y %in% args_params))) %>%
    filter(drop == TRUE) %>%
    pull(.data$idx) %>%
    unique()

  if(length(idx_drop)>0){
    param_list <- param_list[-idx_drop]
  }
  c(param_list,
    args)

}



#' Create tfrmt object from significant digits spec
#'
#' This function creates a tfrmt based on significant digits specifications for
#' group/label values. The input data spec provided to `sigdig_df` will contain
#' group/label value specifications. `tfrmt_sigdig` assumes that these columns
#' are group columns unless otherwise specified. The user may optionally choose
#' to pass the names of the group and/or label columns as arguments to the
#' function.
#'
#'
#' @param sigdig_df data frame containing significant digits formatting spec.
#'   Has 1 record per group/label value, and columns for relevant group and/or
#'   label variables, as well as a numeric column `sigdig` containing the
#'   significant digits rounding to be applied in addition to the default. If
#'   unique group/label values are represented in multiple rows, this will
#'   result in only one of the `sigdig` values being carried through in
#'   implementation.
#' @param group what are the grouping vars of the input dataset
#' @param label what is the label column of the input dataset
#' @param param_defaults Option to override or add to default parameters.
#' @param missing missing option to be included in all `frmt`s
#' @param tfrmt_obj an optional tfrmt object to layer
#' @param ... These dots are for future extensions and must be empty.
#'
#' @return `tfrmt` object with a `body_plan` constructed based on the
#'   significant digits data spec and param-level significant digits defaults.
#'
#' @details
#'
#' ## Formats covered
#'
#' Currently covers specifications for `frmt` and
#' `frmt_combine`. `frmt_when` not supported and must be supplied in additional
#' `tfrmt` that is layered on.
#'
#' ## Group/label variables
#'
#' If the group/label variables are not provided to the arguments, the body_plan
#' will be constructed from the input data with the following behaviour:
#' - If no group or label are supplied, it will be assumed that all columns in the input
#' data are group columns.
#' - If a label variable is provided, but nothing is
#' specified for group, any leftover columns (i.e. not matching `sigdig` or the
#' supplied label variable name) in the input data will be assumed to be group
#' columns.
#' - If any group variable is provided, any leftover columns (i.e. not
#' matching `sigdig` or the supplied group/label variable) will be disregarded.
#'
#' @section Examples:
#'
#' ```r
#' sig_input <- tibble::tribble(
#'   ~group1,   ~group2, ~sigdig,
#'   "CHEMISTRY",   ".default", 3,
#'   "CHEMISTRY",   "ALBUMIN",  1,
#'   "CHEMISTRY",   "CALCIUM",   1,
#'   ".default",    ".default",  2
#' )
#'
#' # Subset data for the example
#' data <- dplyr::filter(data_labs, group2 == "BASOPHILS", col1 %in% c("Placebo", "Xanomeline Low Dose"))
#' tfrmt_sigdig(sigdig_df = sig_input,
#'              group = vars(group1, group2),
#'              label = rowlbl,
#'              param_defaults = param_set("[{n}]" = NA)) %>%
#'   tfrmt(column = vars(col1, col2),
#'         param = param,
#'         value = value,
#'         sorting_cols = vars(ord1, ord2, ord3),
#'         col_plan = col_plan(-starts_with("ord"))) %>%
#'   print_to_gt(.data = data)
#' ```
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_sigdig.png\" alt=\"Table of Hematology, which are rounded for visits baseline to week 26\" style=\"width:100\\%;\">"`
#' }}
#'
#'
#' @export
#'
#' @importFrom dplyr rowwise group_split desc vars all_of
#' @importFrom tidyr unite
#' @importFrom purrr map
#' @importFrom rlang quo_is_missing syms as_name as_label
tfrmt_sigdig <- function(sigdig_df,
                         group=vars(),
                         label=quo(),
                         param_defaults = param_set(),
                         missing = NULL,
                         tfrmt_obj = NULL,
                         ...){

  tfrmt_inputs <-  quo_get(c("group","label"), as_var_args = "group", as_quo_args = "label")

  # if a tfrmt_obj is supplied and no group or label parameters are passed, use the one from the tfrmt_obj
  if(!is.null(tfrmt_obj)){
    if(is_empty(tfrmt_inputs$group) && !is_empty(tfrmt_obj$group)){
        tfrmt_inputs$group <- tfrmt_obj$group
    }
    if(quo_is_missing(tfrmt_inputs$label) && !quo_is_missing(tfrmt_obj$label)){
      tfrmt_inputs$label <- tfrmt_obj$label
    }
  }

  # error if no sigdig column
  if (!"sigdig" %in% names(sigdig_df)){
    stop("`sigdig_df` input must contain `sigdig` column.")
  }

  # error if no group/label columns available
  data_names <- sigdig_df %>% select(-"sigdig") %>% names()
  if (length(data_names)==0){
    stop("`sigdig_df` input must contain group and/or label value columns.")
  }

  group_names <- map_chr(tfrmt_inputs$group, as_label)
  label_name <- if (quo_is_missing(tfrmt_inputs$label)) character(0) else as_label(tfrmt_inputs$label)

  # if group param is provided, figure out which group/label variables are present in data and only keep those
  if (length(group_names)>0){
    sigdig_df <- sigdig_df %>% select(any_of(c(group_names, label_name, "sigdig")))

    # error if mismatch between provided group (and label, if it exists) & data columns
    data_names <- sigdig_df %>% select(-"sigdig") %>% names()
    if (length(data_names)==0){
      group_msg <- if(length(group_names)>0) paste0("group: ", paste(group_names, collapse = ", "), "\n") else ""
      label_msg <- if(length(label_name)>0) paste0("label: ", paste(label_name, collapse = ", ")) else ""
      stop("`sigdig_df` input does not contain any of the specified group/label params:\n",
           group_msg,
           label_msg)
    }
  }

  # if group param is NOT provided, any columns not covered by label param will be set to group param
  if (length(group_names)==0){
    groups_to_add <- setdiff(data_names, label_name)
    tfrmt_inputs$group <- c(tfrmt_inputs$group, vars(!!!syms(groups_to_add)))
  }


  # warning if provided group params are not present in the data
  new_group_names <- map_chr(tfrmt_inputs$group, as_label)

  if (!all(new_group_names %in% names(sigdig_df))){
    grp <- setdiff(new_group_names, names(sigdig_df))
    warning("`sigdig_df` input does not contain the following group params: ", paste0(grp, collapse = ", "))
  }


  # if input data contains grouping variables, establish ordering based on
  # whether any of the grouping values are set to .default
  groups_in_data <- intersect(data_names, new_group_names)

  if (length(groups_in_data)>0){
    data_ord <- sigdig_df %>%
      unite("def_ord", all_of(groups_in_data), remove = FALSE) %>%
      mutate(def_ord = str_count(.data$def_ord, ".default"))
  } else {
    data_ord <- sigdig_df %>%
      mutate(def_ord = 0)
  }

  # Create body plan
  frmt_structure_list <- data_ord %>%
    group_by(def_ord = desc(.data$def_ord), .data$sigdig) %>%
    group_split() %>%
    map(select, -"def_ord") %>%
    map(body_plan_builder, tfrmt_inputs$group, tfrmt_inputs$label, param_defaults, missing = NULL)

  bp <- frmt_structure_list %>%
    do.call("c",.)   %>%
    do.call("body_plan", .)


  # output tfrmt (layer with previous, if applicable)
  tfrmt_out <- tfrmt(
    group = tfrmt_inputs$group,
    label = tfrmt_inputs$label,
    body_plan = bp)

  if(!is.null(tfrmt_obj)){
    layer_tfrmt(x = tfrmt_obj,
                y = tfrmt_out)
  } else {
     tfrmt_out
  }

}
