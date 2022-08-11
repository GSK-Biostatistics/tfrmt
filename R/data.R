#' Demography Analysis Results Data
#'
#' A dataset containing the results needed for a demography table. Using the
#' CDISC pilot data.
#'
#' @format A data frame with 386 rows and 7 variables:
#'  \describe{
#'   \item{rowlbl1}{highest level row labels}
#'   \item{rowlbl2}{more specific row labels}
#'   \item{param}{parameter to explain each value}
#'   \item{grp}{grouping column used to distinguish continuous and categorical}
#'   \item{ord1}{controls ordering}
#'   \item{ord2}{more ordering controls}
#'   \item{column}{column names}
#'   \item{value}{values to put in a table}
#'   }
"data_demog"


#' Adverse Events Analysis Results Data
#'
#' A dataset containing the results needed for an AE table. Using the
#' CDISC pilot data.
#'
#' @format A data frame with 2,794 rows and 8 variables:
#'  \describe{
#'   \item{AEBODSYS}{highest level row labels: System Organ Class}
#'   \item{AETERM}{more specific row labels: Preferred Term}
#'   \item{col2}{higher level column names (spanners)}
#'   \item{col1}{lower level column names}
#'   \item{param}{parameter to explain each value}
#'   \item{value}{values to put in a table}
#'   \item{ord1}{controls ordering}
#'   \item{ord2}{more ordering controls}
#'   }
"data_ae"

#' Efficacy Analysis Results Data
#'
#' A dataset containing the results needed for an Efficacy table. Using the
#' CDISC pilot data for ADAS-Cog(11).
#'
#' @format A data frame with 70 rows and 7 variables:
#'  \describe{
#'   \item{group}{highest level row labels}
#'   \item{label}{more specific row labels}
#'   \item{column}{column names}
#'   \item{param}{parameter to explain each value}
#'   \item{value}{values to put in a table}
#'   \item{ord1}{controls ordering}
#'   \item{ord2}{more ordering controls}
#'   }
"data_efficacy"


#' Labs Analysis Results Data
#'
#' A dataset containing the results needed for an labs results table. Using the
#' CDISC pilot data.
#'
#' @format A data frame with 4,950 rows and 7 variables:
#'  \describe{
#'   \item{group1}{highest level row labels: Lab value class}
#'   \item{group2}{more specific row labels: Lab parameter}
#'   \item{rowlbl}{most specific row labels: Study visit}
#'   \item{col1}{higher level column names (spanners)}
#'   \item{col2}{lower level column names}
#'   \item{param}{parameter to explain each value}
#'   \item{value}{values to put in a table}
#'   \item{ord1}{controls ordering}
#'   \item{ord2}{more ordering controls}
#'   \item{ord3}{more ordering controls}
#'   }
"data_labs"
