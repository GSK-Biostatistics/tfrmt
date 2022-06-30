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
#' @param frmt_when formatting to be used on the the percent values
#' @param tfrmt_obj an optional tfrmt object to layer
#'
#' @export

frmt_n_pct <- function(n = c("n", "count", "distinct_n"),
                       pct = c("pct", "percent", "distinct_pct"),
                       frmt_when = frmt_when("==100"~ frmt(""),
                                             ">99"~ frmt("(>99%)"),
                                             "==0"~ "",
                                             "<1" ~ frmt("(<1%)"),
                                             "TRUE" ~ frmt("(xx.x%)")),
                       tfrmt_obj = NULL){

  n = match.arg(n)
  pct = match.arg(pct)

  if (n == "n" & pct == "pct"){
    combo <- frmt_combine(
      "{n} {pct}",
      n = frmt("xxx"),
      pct = frmt_when)
  }
  else if (n == "n" & pct == "percent") {
    combo <- frmt_combine(
      "{n} {percent}",
      n = frmt("xxx"),
      percent = frmt_when)
  }
  else if (n == "count" & pct == "pct") {
    combo <- frmt_combine(
      "{count} {pct}",
      count = frmt("xxx"),
      pct = frmt_when)
  }
  else if (n == "count" & pct == "percent"){
    combo <- frmt_combine(
      "{count} {percent}",
      count = frmt("xxx"),
      percent = frmt_when)
  }
  else {
    combo <- frmt_combine(
      "{n} {pct}",
      n = frmt("xxx"),
      pct = frmt_when)
  }


  if(!is.null(tfrmt_obj)){
    ae_tbl <- tfrmt(
      body_plan = body_plan(
        frmt_structure(
          combo
        )
      )
    )
    ae_tbl <- layer_tfrmt(x = tfrmt_obj, y = ae_tbl)
  } else {
    ae_tbl <- tfrmt(
      param = param,
      label = row_label1,
      column = col1,
      group = row_label2,
      values = value,
      body_plan = body_plan(
        frmt_structure(
          combo
        )
      )
    )
  }
  ae_tbl
}
