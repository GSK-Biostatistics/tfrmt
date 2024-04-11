
#' Print mock table to GT
#'
#' @param tfrmt tfrmt the mock table will be based off of
#' @param .data Optional data. If this is missing, group values, labels values
#'   and parameter values will be estimated based on the tfrmt
#' @param .default sequence to replace the default values if a dataset isn't
#'   provided
#' @param n_cols the number of columns. This will only be used if mock data isn't
#'   provided. If not supplied, it will default to using the `col_plan` from the
#'   `tfrmt`. If neither are available it will use 3.
#' @param .unicode_ws Whether to convert white space to unicode in preparation for output
#'
#' @return a stylized gt object
#' @export
#'
#' @section Examples:
#'```r
#'
#'   # Create tfrmt specification
#'   tfrmt_spec <- tfrmt( label = label, column =
#'   column, param = param, body_plan = body_plan( frmt_structure(group_val =
#'   ".default", label_val = ".default", frmt_combine( "{count} {percent}",
#'   count = frmt("xxx"), percent = frmt_when("==100"~ frmt(""), "==0"~ "",
#'   "TRUE" ~ frmt("(xx.x%)")))) ))
#'
#'   # Print mock table using default
#'   print_mock_gt(tfrmt = tfrmt_spec)
#'
#'```
#'
#'   \if{html}{\out{ `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_mock_gt1.png\" alt = \"Simple 3 by 3 table without values\" style=\"width:50\\%;\">"` }}
#'
#'```r
#'   # Create mock data
#'   df <- crossing(label = c("label 1", "label 2",
#'   "label 3"), column = c("placebo", "trt1", "trt2"), param = c("count",
#'   "percent"))
#'
#'   # Print mock table using mock data
#'   print_mock_gt(tfrmt_spec, df)
#'
#'```
#'
#'   \if{html}{\out{ `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_mock_gt2.png\" alt = \"Simple 3 by 3 table without values, but with column names\" style=\"width:50\\%;\">"` }}
#'
#'
#' @importFrom gt gt tab_header tab_style cell_text cells_body px
#' @importFrom tidyselect everything eval_select
#' @importFrom rlang quo_is_missing sym quo is_empty
#' @importFrom dplyr vars
#' @importFrom purrr quietly safely
print_mock_gt <- function(tfrmt,
                          .data = NULL,
                          .default = 1:3,
                          n_cols = NULL,
                          .unicode_ws = TRUE) {

  # fill param, column if not provided
  if (quo_is_missing(tfrmt$param)){
    message("`tfrmt` will need a `param` value to `print_to_gt` when data is avaliable")
    tfrmt$param <- quo(!!sym("__tfrmt__param"))
  }
  if (is_empty(tfrmt$column)){
    message("`tfrmt` will need `column` value(s) to `print_to_gt` when data is avaliable")
    tfrmt$column <- vars(!!sym("__tfrmt__column"))
  }

  if(quo_is_missing(tfrmt$value)){
    message("Message: `tfrmt` will need `value` value to `print_to_gt` when data is avaliable")
    tfrmt$value <- quo(!!sym("__tfrmt__val"))
  }

  if(is.null(tfrmt$body_plan)){
    tfrmt$body_plan <- body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("X.X"))
    )
  }

  if(is.null(.data)){
    .data <- make_mock_data(tfrmt, .default, n_cols)
  }else{
    ## check that if value column exists in data, remove it for mocking
    select_try <- safely(quietly(eval_select))(tfrmt$value, data = .data)
    if(!is.null(select_try$result)){
      message(" Removing `",as_label(tfrmt$value),"` from input data for mocking.")
      .data <- .data[,-select_try$result$result]
    }
  }

  apply_tfrmt(.data, tfrmt, mock = TRUE) %>%
    cleaned_data_to_gt(tfrmt, .unicode_ws)

}

#' Print to gt
#'
#' @param tfrmt tfrmt object that will dictate the structure of the table
#' @param .data Data to style in order to make the table
#' @param .unicode_ws Whether to convert white space to unicode in preparation for output
#'
#' @return a stylized gt object
#' @export
#'
#' @section Examples:
#'
#' ```r
#' library(dplyr)
#' # Create tfrmt specification
#' tfrmt_spec <- tfrmt(
#'   label = label,
#'   column = column,
#'   param = param,
#'   value=value,
#'   body_plan = body_plan(
#'     frmt_structure(group_val = ".default", label_val = ".default",
#'                    frmt_combine(
#'                      "{count} {percent}",
#'                      count = frmt("xxx"),
#'                      percent = frmt_when("==100"~ frmt(""),
#'                                          "==0"~ "",
#'                                          "TRUE" ~ frmt("(xx.x%)"))))
#'   ))
#'
#' # Create data
#' df <- crossing(label = c("label 1", "label 2"),
#'                column = c("placebo", "trt1"),
#'                param = c("count", "percent")) %>%
#'       mutate(value=c(24,19,2400/48,1900/38,5,1,500/48,100/38))
#'
#' print_to_gt(tfrmt_spec,df)
#'
#' ```
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_to_gt.png\" alt = \"2 by 2 table with labels down the side and placebo and trt1 across the top\" style=\"width:50\\%;\">"`
#' }}
#'
#' @importFrom gt gt tab_header tab_style cell_text cells_body tab_options
#' @importFrom tidyselect everything
print_to_gt <- function(tfrmt, .data, .unicode_ws = TRUE){
  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  if(!is.data.frame(.data)){
    stop("Requires data, if not avaliable please use `print_mock_gt()`")
  }
  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    cleaned_data_to_gt(tfrmt, .unicode_ws)

}


#' Do all the formatting for the GT
#' @rdname cleaned_data_to_gt
#' @export
cleaned_data_to_gt <- function(.data, tfrmt, .unicode_ws){
  UseMethod("cleaned_data_to_gt", .data)
}

#' Apply formatting to a list of tables
#'
#' @param .data list of cleaned datasets
#' @param tfrmt tfrmt
#' @param .unicode_ws Whether to convert white space to unicode in preparation for output
#'
#' @return gt_group object
#' @rdname cleaned_data_to_gt
#' @export
#'
#' @keywords internal
#' @importFrom gt gt_group
#' @importFrom purrr map2
cleaned_data_to_gt.list <- function(.data, tfrmt, .unicode_ws){

  map(.data, ~cleaned_data_to_gt.default(.x, tfrmt, .unicode_ws)) %>%
    gt_group(.list = .)
}
#' Apply formatting to a single table
#'
#' @param .data cleaned dataset
#' @param tfrmt tfrmt
#' @param .unicode_ws Whether to convert white space to unicode in preparation for output
#'
#' @return GT object
#' @rdname cleaned_data_to_gt
#' @export
#'
#' @keywords internal
#' @importFrom gt cells_stub cells_row_groups default_fonts cell_borders
#'   opt_table_font tab_options tab_style cell_text px cells_column_spanners
#'   cells_body cells_column_labels md cols_hide sub_missing tab_stubhead
cleaned_data_to_gt.default <- function(.data, tfrmt, .unicode_ws){


  if((is.null(tfrmt$row_grp_plan) ||(!inherits(.data, "grouped_df"))) && length(tfrmt$group) > 0){
    existing_grp <- tfrmt$group %>%
      keep(function(x){
        as_label(x) %in% names(.data)
      })
    .data <- .data %>%
      group_by(!!!existing_grp)
  }

  if (!is.null(tfrmt$col_style_plan)){
    align <- "left"
  } else {
    align <- NULL
  }
  if (!"..tfrmt_row_grp_lbl" %in% names(.data)) {
    # keep attribute for footnotes
    attr_footnote <- attr(.data,".footnote_locs")
    attr_stub_header <- attr(.data,".stub_header")
    .data <- mutate(.data, ..tfrmt_row_grp_lbl = FALSE)
    attr(.data,".footnote_locs") <- attr_footnote
    attr(.data,".stub_header") <- attr_stub_header
  }
  gt_out <- .data %>%
    gt(
      rowname_col = as_label(tfrmt$label)) %>%
    sub_missing(
      rows = .data$..tfrmt_row_grp_lbl==TRUE,
      missing_text = ""
    ) %>%
    cols_hide(columns = "..tfrmt_row_grp_lbl") %>%
    format_gt_column_labels(.data) %>%
    tab_style(
      style = cell_text(whitespace = "pre-wrap", align = align),
      locations = cells_body(columns = everything())
    )

  # group label in its own column
  if(!is.null(tfrmt$row_grp_plan) && tfrmt$row_grp_plan$label_loc$location == "column"){
    gt_out <- gt_out %>%
      tab_options(row_group.as_column = TRUE)
  }

  # stub header
  if (!is.null(attr(.data, ".stub_header"))){
    gt_out <- gt_out %>%
      tab_stubhead(label = attr(.data, ".stub_header"))
  }

  gt_out_final  <- gt_out %>%
    tab_style(
      style = list(
        cell_text(whitespace = "pre-wrap", align = "left")
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
      stub.border.width = px(0),
      stub.border.color = "transparent",
      stub_row_group.border.width = px(0),
      stub_row_group.border.color = "transparent",
      row_group.border.bottom.width = px(0),
      row_group.border.bottom.color = "transparent",
      row_group.border.top.color = "transparent",
      table.font.names = c("Courier", default_fonts()),
      page.numbering = TRUE,
      page.header.use_tbl_headings = FALSE,
      page.orientation = "landscape") %>%

    tab_style(
      style = cell_text(whitespace = "pre-wrap", align = "center"),
      locations = list(cells_column_spanners(),cells_column_labels())
    ) %>%

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
      )) %>%
    tab_style(
      style = cell_text(font = c("Courier", default_fonts())),
      locations = list(cells_body(), cells_row_groups(), cells_stub(),
                       cells_column_labels(), cells_column_spanners())
    )

  # add page note if applicable
  if (!is.null(attr(.data, ".page_note")) &&
      !is.null(tfrmt$page_plan) &&
      !tfrmt$page_plan$note_loc=="noprint"){

    if (tfrmt$page_plan$note_loc=="preheader"){

      gt_out_final <- gt_out_final  %>%
        tab_header(title = tfrmt$title,
                   subtitle = tfrmt$subtitle,
                   preheader = attr(.data, ".page_note"))

    } else if (tfrmt$page_plan$note_loc=="subtitle"){

      title <- tfrmt$title %||% ""
      subtitle <- paste0(tfrmt$subtitle, attr(.data, ".page_note"), collapse = "\n")

      gt_out_final <- gt_out_final  %>%
        tab_header(title = title,
                   subtitle = subtitle)

    } else {
      gt_out_final <- gt_out_final  %>%
        tab_header(title = tfrmt$title,
                   subtitle = tfrmt$subtitle)

      if (tfrmt$page_plan$note_loc=="source_note"){
        gt_out_final <- gt_out_final %>%
          tab_source_note(attr(.data, ".page_note"))
      }

    }
  } else {
    gt_out_final <- gt_out_final  %>%
      tab_header(title = tfrmt$title,
                 subtitle = tfrmt$subtitle)
  }

  # convert white space to unicode
  if (.unicode_ws){
    gt_out_final <- gt_out_final %>%
      convert_ws_unicode()
  }

  # add footnotes and output
  gt_out_final %>%
    apply_footnote_plan(tfrmt,attr(.data,".footnote_locs"))

}



#' Format gt column labels
#'
#' Makes a gt object and applies the spanning labels if relevant, as well as
#' markdown formatting of all column labels
#'
#' @param gt_table gt object
#' @param tfrmt tfrmt object
#'
#' @return gt object
#' @noRd
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_split
#' @importFrom gt cols_label tab_spanner md
#' @importFrom dplyr as_tibble desc coalesce left_join mutate
#' @importFrom purrr keep
#'
format_gt_column_labels <- function(gt_table, .data){

  spanning <- names(.data) %>% keep(str_detect, .tlang_delim)
  if(length(spanning) > 0){

    work_df<- names(.data) %>%
      keep(str_detect, .tlang_delim) %>%
      str_split(.tlang_delim, simplify = TRUE) %>%
      as_tibble( .name_repair = ~paste0("V", 1:length(.))) %>%
      mutate(cols = spanning) %>%
      pivot_longer(-"cols")

    lowest_lvl <- work_df %>% filter(.data$name == max(.data$name))

    spans_to_apply <- work_df %>%
      filter(.data$name != max(.data$name)) %>%
      arrange(desc(.data$name)) %>%
      group_by(.data$value) %>%
      nest(set = "cols") %>%
      mutate(set = map(.data$set, ~pull(.,.data$cols))) %>%
      filter(.data$value != "NA")

    for(i in 1:nrow(spans_to_apply)){

      # convert column spanning labels to markdown format
      gt_table <- gt_table %>%
        tab_spanner(md(spans_to_apply$value[i]), columns = all_of(spans_to_apply$set[[i]]))
    }

    # ensure all columns are represented
    lowest_lvl <- names(.data) %>%
      tibble(cols = .) %>%
      left_join(lowest_lvl, by = "cols") %>%
      mutate(value = coalesce(.data$value, .data$cols))

    renm_vals <- lowest_lvl %>%
      pull(.data$value)
    names(renm_vals) <-lowest_lvl %>%
      pull(.data$cols)


  } else {

    renm_vals <- names(.data)
    names(renm_vals) <- renm_vals

  }

  # convert lowest level column labels to markdown format
  gt_table %>%
    cols_label(.list=
                 lapply(renm_vals,md))


}

#' Convert gt whitespace to unicode text
#'
#' @param gt_table gt object
#'
#' @return gt object
#' @noRd
#' @importFrom gt text_transform cells_body cells_stub cells_column_labels cells_column_spanners
#' @importFrom stringr str_match str_c str_dup str_trim
#'
convert_ws_unicode <- function(gt_table){

  locations <- list(cells_body())

  if (sum(c(!is.na(gt_table[["_stub_df"]]$row_id),!is.na(gt_table[["_stub_df"]]$group_id)) > 0)){

    locations <- c(locations,
                   list(cells_stub()))
  }

  gt_table %>%
    text_transform(
      locations = locations,
      fn = function(x) {

        x_trimmed <- str_trim(x)
        space_left <- str_match(x, "^\\s*") %>% nchar()
        space_right <- str_match(x, "\\s*$") %>% nchar()
        space_right[x_trimmed == ""] <- 0

        str_c(
          str_dup("\U00A0", space_left),
          x_trimmed,
          str_dup("\U00A0", space_right)
        )
      }
    )
}
