#' Standard count-percent format (n_pct_frmt)
#'
#' @param n name of count (n) column
#' @param pct  name of percent (pct) column
#'
#' @export

n_pct_frmt <- function(n = c("n", "count"),
                       pct = c("pct", "percent")){

  n = match.arg(n)
  pct = match.arg(pct)

  if (n == "n" & pct == "pct"){
    frmt_combine(
      "{n} {pct}",
      n = frmt("xxx"),
      pct = frmt_when("==100"~ frmt(""),
                      "==0"~ "",
                      "TRUE" ~ frmt("(xx.x%)")))
  }
  else if (n == "n" & pct == "percent") {
    frmt_combine(
      "{n} {percent}",
      n = frmt("xxx"),
      percent = frmt_when("==100"~ frmt(""),
                          "==0"~ "",
                          "TRUE" ~ frmt("(xx.x%)")))
  }
  else if (n == "count" & pct == "pct") {
    frmt_combine(
      "{count} {pct}",
      count = frmt("xxx"),
      pct = frmt_when("==100"~ frmt(""),
                      "==0"~ "",
                      "TRUE" ~ frmt("(xx.x%)")))
  }
  else if (n == "count" & pct == "percent"){
    frmt_combine(
      "{count} {percent}",
      count = frmt("xxx"),
      percent = frmt_when("==100"~ frmt(""),
                          "==0"~ "",
                          "TRUE" ~ frmt("(xx.x%)")))
  }
  else {
    frmt_combine(
      "{n} {pct}",
      n = frmt("xxx"),
      pct = frmt_when("==100"~ frmt(""),
                      "==0"~ "",
                      "TRUE" ~ frmt("(xx.x%)")))
  }
}
