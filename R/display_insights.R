#' Matching the formatting to all values in the dataset
#'
#' @param .data data
#' @param table_frmt_plan styling element needed
#' @param group symbolic list of grouping
#' @param label symbolic label
#' @param param symbolic parameter
#'
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate slice pull tibble ungroup group_by left_join arrange
#' @importFrom purrr map
#' @noRd
match_frmt_to_rows <- function(.data, table_frmt_plan, group, label, param){
  .data <- .data %>%
    ungroup() %>%
    mutate(TEMP_row = row_number())

  TEMP_appl_row <- table_frmt_plan %>%
    map(fmt_test_data, .data, label, group, param)

  TEMP_fmt_to_apply <- table_frmt_plan %>% map(~.$frmt_to_apply[[1]])

  dat_plus_fmt <- tibble(
    TEMP_appl_row,
    TEMP_fmt_to_apply) %>%
    # TODO add a warning if a format isn't applied anywhere
    mutate(TEMP_fmt_rank = row_number()) %>%
    unnest(cols = c(TEMP_appl_row)) %>%
    group_by(TEMP_appl_row) %>%
    # TODO add warning if there are rows not covered
    arrange(TEMP_appl_row, desc(.data$TEMP_fmt_rank)) %>%
    slice(1) %>%
    left_join(.data, ., by = c("TEMP_row" = "TEMP_appl_row"))
}

#' Display formatting applied to each row
#'
#' Used when debugging formatting, it is an easy way to allow you to see which
#' formats are applied to each row in your dataset.
#'
#' @param tfrmt tfrmt object to apply to the data
#' @param .data Data to apply the tfrmt to
#' @param convert_to_txt Logical value converting formatting to text, by default
#'   `TRUE`
#'
#' @importFrom dplyr mutate rename select case_when
#' @importFrom purrr map_chr
#' @importFrom stringr str_remove str_remove_all str_sub str_trim
#' @return formatted tibble
#' @export
#' @examples
#'  library(dplyr)
#'  library(tidyr)
#'
#'  tfrmt_spec <- tfrmt(
#'  label = label,
#'  column = column,
#'  param = param,
#'  value=value,
#'  body_plan = body_plan(
#'    frmt_structure(group_val = ".default", label_val = ".default",
#'                   frmt_combine(
#'                     "{count} {percent}",
#'                     count = frmt("xxx"),
#'                     percent = frmt_when("==100"~ frmt(""),
#'                                         "==0"~ "",
#'                                         "TRUE" ~ frmt("(xx.x%)"))))
#'  ))
#'
#'  # Create data
#'  df <- crossing(label = c("label 1", "label 2"),
#'                 column = c("placebo", "trt1"),
#'                 param = c("count", "percent")) %>%
#'    mutate(value=c(24,19,2400/48,1900/38,5,1,500/48,100/38))
#'
#'  display_row_frmts(tfrmt_spec,df)
display_row_frmts <- function(tfrmt, .data, convert_to_txt = TRUE){
  if (convert_to_txt == FALSE){
    output <- match_frmt_to_rows(.data ,
                                 tfrmt$body_plan,
                                 tfrmt$group,
                                 tfrmt$label,
                                 tfrmt$param) %>%
      rename(frmt_applied = "TEMP_fmt_to_apply") %>%
      select(-starts_with("TEMP")) %>%
      mutate(frmt_type = map_chr(.data$frmt_applied, function(x) unlist(class(x)[1])))

  } else if (convert_to_txt == TRUE) {
    output <- match_frmt_to_rows(.data ,
                                 tfrmt$body_plan,
                                 tfrmt$group,
                                 tfrmt$label,
                                 tfrmt$param) %>%
      rename(frmt_applied = "TEMP_fmt_to_apply") %>%
      select(-starts_with("TEMP")) %>%
      mutate(frmt_type = map_chr(.data$frmt_applied, function(x) unlist(class(x)[1])),
             frmt_details = map_chr(.data$frmt_applied, format)) %>%
      select(-"frmt_applied")

    # extract < frmt > type from frmt_details
    output <- output %>%
      mutate(frmt_details = case_when(frmt_type == "frmt" ~ frmt_details %>%
                                        str_remove("< frmt \\| Expression: ") %>% str_remove(" >"),
                                      frmt_type == "frmt_combine" ~ frmt_details %>%
                                        str_remove("< frmt_combine \\| Expression: ") %>% str_remove(" >"),
                                      frmt_type == "frmt_when" ~ frmt_details %>%
                                        str_remove("< frmt_when \\| ") %>%
                                        str_sub(end = -2L) %>%
                                        str_remove_all("[\n]") %>%
                                        str_trim() %>%
                                        gsub(pattern = "\\s\\s", replacement = ", "))
      )

  } else {
    stop("Please pass a boolean value into the `convert_to_txt` parameter")
  }

  output
}

#' Display formatted values
#'
#' A helper for creating positional-alignment specifications for the col_style_plan.
#' Returns all unique formatted values to appear in the column(s) specified. Numeric values are represented by x's.
#'
#'
#' @param tfrmt tfrmt object to apply to the data
#' @param .data Data to apply the tfrmt to
#' @param mock Mock table? TRUE or FALSE (default)
#' @param col Column value to align on from `column` variable. May be a quoted
#'   or unquoted column name, a tidyselect semantic, or a span_structure.
#'
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr select mutate across arrange pull
#' @importFrom tidyselect everything any_of
#' @importFrom rlang is_empty as_name as_label
#' @importFrom stringr str_replace_all
#' @importFrom purrr map_chr map
#' @importFrom tidyr pivot_longer
#'
#' @return text representing character vector of formatted values to be copied and modified in the col_style_plan
#' @export
#' @examples
#'  tf_spec <- tfrmt(
#'  group = c(rowlbl1,grp),
#'  label = rowlbl2,
#'  column = column,
#'  param = param,
#'  value = value,
#'  sorting_cols = c(ord1, ord2),
#'  body_plan = body_plan(
#'  frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} ({pct} %)",
#'                                                                              n = frmt("xxx"),
#'                                                                              pct = frmt("xx.x"))),
#'  frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
#'  frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min","Max"),
#'                              frmt("xxx.x")),
#'  frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
#'  frmt_structure(group_val = ".default", label_val = ".default",
#'                             p = frmt_when(">0.99" ~ ">0.99",
#'                                           "<0.15" ~ "<0.15",
#'                                           TRUE ~ frmt("x.xxx", missing = "")))
#' ))
#'
#'  display_val_frmts(tf_spec, data_demog, col = vars(everything()))
#'  display_val_frmts(tf_spec, data_demog, col = "p-value")
display_val_frmts <- function(tfrmt, .data, mock = FALSE, col = NULL){


  tbl_dat <- .data %>%
    remove_big_ns(param = tfrmt$param,
                  big_n_structure = tfrmt$big_n) %>%
    apply_table_frmt_plan(
      .data = .,
      table_frmt_plan = tfrmt$body_plan,
      group = tfrmt$group,
      label = tfrmt$label,
      param = tfrmt$param,
      value = tfrmt$value,
      column = tfrmt$column,
      mock = mock
    )

  tbl_dat_wide <- tbl_dat %>%
    pivot_wider_tfrmt(tfrmt, mock) %>%
    select(-any_of(c(
      map_chr(tfrmt$group, as_name),
      as_name(tfrmt$label)))) %>%
    mutate(across(everything(), ~str_replace_all(., "[0-9]","x")))

  col_plan_vars <- as_vars.character(colnames(tbl_dat_wide))
  if(is_empty(tfrmt$column)){
    # create placeholder
    column_names <- "col"
  }else{
    column_names <- map_chr(tfrmt$column, as_label)
  }
  selection <- as.list(substitute(substitute(col)))[-1] %>%
    map(trim_vars_quo_c) %>%
    do.call('c',.) %>%
    check_col_plan_dots()

  col_selection <- col_style_selections(selection, column_names, col_plan_vars)

  vec_prep <- tbl_dat_wide%>%
    select(any_of(col_selection)) %>%
    pivot_longer(everything()) %>%
    arrange(nchar(.data$value)) %>%
    filter(!is.na(.data$value)) %>%
    pull(.data$value) %>%
    unique %>%
    paste0("\"", ., "\"") %>%
    glue_collapse(., ",\n  ")

  glue("c({vec_prep})")
}
