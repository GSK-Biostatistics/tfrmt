
#' Print mock table to GT
#'
#' @param tfrmt tfrmt the mock table will be based off of
#' @param .data Optional data. If this is missing, group values, labels values
#'   and parameter values will be estimated based on the tfrmt
#' @param .default sequence to replace the default values if a dataset isn't
#'   provided
#' @param n_cols the number of columns this will only be used if mock data isn't
#'   provided
#'
#' @return a stylized gt object
#' @export
#' @importFrom gt gt tab_header tab_style cell_text cells_body px
#' @importFrom tidyselect everything
#' @importFrom rlang quo_is_missing sym quo is_empty
#' @importFrom dplyr vars
print_mock_gt <- function(tfrmt, .data = NULL, .default = 1:3, n_cols = 3) {

  # fill param, column if not provided
  if (quo_is_missing(tfrmt$param)){
    message("`tfrmt` will need a `param` value to `print_to_gt` when data is avaliable")
    tfrmt$param <- quo(!!sym("__tfrmt__param"))
  }
  if (is_empty(tfrmt$column)){
    message("`tfrmt` will need `column` value(s) to `print_to_gt` when data is avaliable")
    tfrmt$column <- vars(!!sym("__tfrmt__column"))
  }

  if(quo_is_missing(tfrmt$values)){
    message("`tfrmt` will need `value` value to `print_to_gt` when data is avaliable")
    tfrmt$values <- quo(!!sym("__tfrmt__val"))
  }

  if(is.null(.data)){
    .data <- make_mock_data(tfrmt, .default, n_cols)
  }

  if(is.null(tfrmt$body_plan)){
    tfrmt$body_plan <- body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("X.X"))
    )
  }

  apply_tfrmt(.data, tfrmt, mock = TRUE) %>%
    cleaned_data_to_gt(tfrmt)

}

#' Print to gt
#'
#' @param tfrmt tfrmt object that will dictate the structure of the table
#' @param .data Data to style in order to make the table
#'
#' @return a stylized gt object
#' @export
#' @importFrom gt gt tab_header tab_style cell_text cells_body tab_options
#' @importFrom tidyselect everything
print_to_gt <- function(tfrmt, .data){
  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  if(!is.data.frame(.data)){
    stop("Requires data, if not avaliable please use `print_mock_gt()`")
  }
  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    cleaned_data_to_gt(tfrmt)

}


#' Do all the formatting for the GT
#'
#' @param .data cleaned dataset
#' @param tfrmt tfrmt
#'
#' @return GT object
#' @noRd
#' @importFrom gt cells_stub cells_row_groups default_fonts cell_borders
#'   opt_table_font tab_options tab_style cell_text px cells_column_spanners
#'   cells_body cells_column_labels
cleaned_data_to_gt <- function(.data, tfrmt){
  if(is.null(tfrmt$row_grp_plan) && length(tfrmt$group) > 0){
    existing_grp <- tfrmt$group %>%
      keep(function(x){
        as_label(x) %in% names(.data)
      })
    .data <- .data %>%
      group_by(!!!existing_grp)
  }


  if (!is.null(tfrmt$col_align)){
    align <- "left"
  } else {
    align <- NULL
  }

  gt_out <- .data %>%
    gt(
      rowname_col = as_label(tfrmt$label)) %>%
    tab_header(title = tfrmt$title,
               subtitle = tfrmt$subtitle) %>%
    apply_gt_footnote(tfrmt$footer) %>%
    apply_gt_spanning_labels(.data) %>%
    tab_style(
      style = cell_text(whitespace = "pre", align = align),
      locations = cells_body(columns = everything())
    )


  if(!is.null(tfrmt$row_grp_plan) && tfrmt$row_grp_plan$label_loc$location == "column"){
    gt_out <- gt_out %>%
      tab_options(row_group.as_column = TRUE)
  }

  gt_out %>%
    tab_style(
      style = list(
        cell_text(whitespace = "pre", align = "left")
      ),
      locations = list(cells_stub(), cells_row_groups())
    ) %>%
    tab_options(
      table.font.size = 14,
      data_row.padding = px(1),
      summary_row.padding = px(1),
      grand_summary_row.padding = px(1),
      footnotes.padding = px(1),
      source_notes.padding = px(1),
      row_group.padding = px(1),
      stub.border.color = "transparent",
      stub_row_group.border.color = "transparent",
      row_group.border.bottom.color = "transparent",
      row_group.border.top.color = "transparent",
      table.font.names = c("Courier", default_fonts())) %>%

    tab_style(
      style = cell_borders(
        sides = c("top","bottom"),
        color = "transparent"
      ),
      locations= list(
        cells_body(
          columns = everything(),
          rows = everything()
        ),
        cells_stub(), cells_row_groups())
    )  %>%

    tab_style(
      style = cell_borders(
        sides = c("top"),
        color = "transparent",
        weight = px(0),
      ),
      locations= list(
        cells_column_labels()
    )) %>%

    tab_style(
      style = cell_borders(
        sides = c("bottom"),
        weight = px(0),
        color = "transparent"
      ),
      locations= list(
        cells_column_spanners()
      ))
}


#' Apply gt Footnote
#'
#' @param gt gt object  to potentially add a footnote to
#' @param footer footnote text (should become a footnote element at somepoint )
#'
#' @return gt object
#' @noRd
#'
#' @importFrom gt tab_source_note md
apply_gt_footnote<- function(gt, footer){
  if(is.null(footer)){
    gt
  } else {
    gt %>%
      tab_source_note(
        source_note = md(footer)
      )

  }
}

#' Applies gt spanning labels
#'
#' Makes a gt objectt and applies the spanning labels if relevent
#'
#' @param .data dataset to convert to a gt
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_split
#' @importFrom gt cols_label tab_spanner
#' @importFrom dplyr as_tibble desc
#'
apply_gt_spanning_labels <- function(gt_table, .data){

  spanning <- names(.data) %>% keep(str_detect, .tlang_delim)
  if(length(spanning) > 0){

    work_df<- names(.data) %>%
      keep(str_detect, .tlang_delim) %>%
      str_split(.tlang_delim, simplify = TRUE) %>%
      as_tibble( .name_repair = ~paste0("V", 1:length(.))) %>%
      mutate(cols = spanning) %>%
      pivot_longer(-.data$cols)


    lowest_lvl <- work_df %>% filter(.data$name == max(.data$name))

    spans_to_apply <- work_df %>%
      filter(.data$name != max(.data$name)) %>%
      arrange(desc(.data$name)) %>%
      group_by(.data$value) %>%
      nest(set = "cols") %>%
      mutate(set = map(.data$set, ~pull(.,cols))) %>%
      filter(.data$value != "NA")

    for(i in 1:nrow(spans_to_apply)){
      gt_table <- gt_table %>%
        tab_spanner(spans_to_apply$value[i], columns = spans_to_apply$set[[i]])
    }

    renm_vals <- lowest_lvl %>%
      pull(.data$value)
    names(renm_vals) <-lowest_lvl %>%
      pull(.data$cols)

    gt_table <- gt_table %>%
      cols_label(.list = renm_vals)

  }
  gt_table

}

