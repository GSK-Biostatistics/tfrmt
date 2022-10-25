#' N Percent Template
#'
#' This function creates an tfrmt for an n % table, so count based table. The
#' parameter values for n and percent can be provided (by default it will assume
#' `n` and `pct`). Additionally the `frmt_when` for formatting the percent can
#' be specified. By default 100% and 0% will not appear and everything between
#' 99% and 100% and 0% and 1% will be rounded using greater than (>) and less
#' than (<) signs respectively.
#' @param n name of count (n) value in the parameter column
#' @param pct  name of percent (pct) value in the parameter column
#' @param pct_frmt_when formatting to be used on the the percent values
#' @param tfrmt_obj an optional tfrmt object to layer
#'
#' @returns tfrmt object
#' @export
#' @section Examples:
#'
#' ```r
#' print_mock_gt(tfrmt_n_pct())
#' ```
#'
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_n_percent.png\" alt=\"3 by 3 table\" style=\"width:50\\%;\">"`
#' }}
#'
#' @importFrom rlang parse_expr
tfrmt_n_pct <- function(n = "n",
                       pct = "pct",
                       pct_frmt_when = frmt_when("==100"~ frmt(""),
                                             ">99"~ frmt("(>99%)"),
                                             "==0"~ "",
                                             "<1" ~ frmt("(<1%)"),
                                             "TRUE" ~ frmt("(xx.x%)")),
                       tfrmt_obj = NULL){
  if(is.null(n)|is.na(n)|n==""){
    stop("`n` value must be provided")
  }
  if(is.null(pct)|is.na(pct)|pct==""){
    stop("`pct` value must be provided")
  }

  combo <- paste0(
    "frmt_combine('{",
    n,
    "} {", pct, "}',",
    n, "=frmt('x'),",
    pct, "=pct_frmt_when)"
  ) %>%
    parse_expr() %>% eval()

  if(!is.null(tfrmt_obj)){
    ae_tbl <- tfrmt(
      body_plan = body_plan(
        frmt_structure(
          group_val = ".default", label_val = ".default",
          combo
        )
      )
    )
    ae_tbl <- layer_tfrmt(x = tfrmt_obj, y = ae_tbl)
  } else {
    ae_tbl <- tfrmt(
      param = "param",
      label = "row_label1",
      column = "col1",
      value = "value",
      body_plan = body_plan(
        frmt_structure(
          group_val = ".default", label_val = ".default",
          combo
        )
      )
    )
  }
  ae_tbl
}
