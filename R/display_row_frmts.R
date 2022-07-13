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

  TEMP_appl_row = table_frmt_plan %>%
    map(fmt_test_data, .data, label, group, param)
  TEMP_fmt_to_apply = table_frmt_plan %>% map(~.$frmt_to_apply[[1]])

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
#' @param tfrmt tfrmt object to apply to the data
#' @param .data Data to apply the tfrmt to
#' @param convert_to_txt Logical value converting formatting to text, by default `TRUE`
#'
#' @importFrom dplyr mutate rename select
#' @return formatted tibble
#' @export
display_row_frmts <- function(tfrmt, .data, convert_to_txt = TRUE){
  if (convert_to_txt == TRUE){
    output <- match_frmt_to_rows(.data ,
                                 tfrmt$body_plan,
                                 tfrmt$group,
                                 tfrmt$label,
                                 tfrmt$param) %>%
      mutate(frmt_applied = map_chr(TEMP_fmt_to_apply, format)) %>%
      select(-starts_with("TEMP"))

    output <- output %>%
      mutate(frmt_applied = str_remove_all(frmt_applied, "[<>*]")) %>%
      separate(col = frmt_applied, into = c("frmt_type", "frmt_expression"), sep = "\\|") %>%
      mutate(frmt_type = str_trim(frmt_type),
             frmt_expression = frmt_expression %>% str_remove("Expression: ") %>% str_trim())

  } else if (convert_to_txt == FALSE) {
    output <- match_frmt_to_rows(.data ,
                                 tfrmt$body_plan,
                                 tfrmt$group,
                                 tfrmt$label,
                                 tfrmt$param) %>%
      rename(frmt_applied = TEMP_fmt_to_apply) %>%
      select(-starts_with("TEMP"))

  } else {
    output <- warning("Please pass a boolean value into the `convert_to_txt` parameter")
  }

  output
}

