
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


#' Set custom parameter-level significant digits rounding
#'
#' @param ... Series of name-value pairs, optionally formatted using
#'   `glue::glue()` syntax (note `glue` syntax is required for combined
#'   parameters). For combined parameters (e.g., "{min}, {max}"), value should
#'   be a vector of the same length (e.g., c(1,1)).
#'
#' @details Type `param_set()` in console to view package defaults. Use of the
#'   function will add to the defaults and/or override included defaults of the
#'   same name. For values that are integers, use `NA` so no demical places will
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
    unnest(.data$params, keep_empty = TRUE) %>%
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
#'
#' @examples
#' \dontrun{
#'
#' data_input <- tibble::tribble(
#'   ~group1,   ~group2, ~sigdig,
#'   "CHEMISTRY",   ".default", 3,
#'   "CHEMISTRY",   "ALBUMIN",  1,
#'   "CHEMISTRY",   "CALCIUM",   1,
#'   ".default",    ".default",  2
#'   )
#'
#'
#' tfrmt_sigdig(data = data_input,
#'       group = vars(group1, group2),
#'       label = rowlbl,
#'       param_defaults = param_set("[{n}]" = NA)) %>%
#'     tfrmt(column = vars(col1, col2),
#'           param = param,
#'           values = value,
#'           sorting_cols = vars(ord1, ord2, ord3),
#'           col_plan = col_plan(-starts_with("ord"))) %>%
#'     print_to_gt(labs_data)
#'
#' }
#'
#' @export
#'
#' @importFrom dplyr rowwise group_split desc
#' @importFrom tidyr unite
#' @importFrom purrr map
tfrmt_sigdig <- function(data, group, label, param_defaults = param_set(), missing = NULL, ...){

  tfrmt_inputs <-  quo_get(c("group","label"), as_var_args = "group", as_quo_args = "label")

  frmt_structure_list <- data %>%
    unite("def_ord", !!!tfrmt_inputs$group, remove = FALSE) %>%
    mutate("def_ord" = str_count(.data$def_ord, ".default")) %>%
    group_by(def_ord = desc(.data$def_ord), .data$sigdig) %>%
    group_split() %>%
    map(select, -.data$def_ord) %>%
    map(body_plan_builder, tfrmt_inputs$group, tfrmt_inputs$label, param_defaults, missing = NULL)

  bp <- frmt_structure_list %>%
    do.call("c",.)   %>%
    do.call("body_plan", .)


  tfrmt(
    group = tfrmt_inputs$group,
    label = tfrmt_inputs$label,
    body_plan = bp)

}
