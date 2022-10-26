#' Print to ggplot
#'
#' @param tfrmt tfrmt object that will dictate the structure of the ggplot object
#' @param .data Data to style in order to make the ggplot object
#' @param ... Inputs to geom_text to modify the style of the table body
#'
#' @return a stylized ggplot object
#' @export
#'
#' @section Examples:
#'
#' ```r
#'
#' # Create data
#' risk<-tibble(time=c(rep(c(0,1000,2000,3000),3)),
#'             label=c(rep("Obs",4),rep("Lev",4),rep("Lev+5FU",4)),
#'             value=c(630,372,256,11,620,360,266,8,608,425,328,14),
#'             param=rep("n",12))
#'
#'table<-tfrmt(
#'  label = label ,
#'  column = time,
#'  param = param,
#'  value = value) %>%
#'   print_to_ggplot(risk)
#'
#'table
#'
#' ```
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_to_ggplot.png\" alt=\"Simple table to stack with a KM-plot\" style=\"width:50\\%;\">"`
#' }}
#'
#' @importFrom rlang quo_is_missing as_label
#' @importFrom dplyr select pull
#' @importFrom magrittr %>%

print_to_ggplot <- function(tfrmt, .data, ...){
  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  if(!is.data.frame(.data)){
    stop("Requires data")
  }

  # stop if label location is not indented
  if(is.null(tfrmt$row_grp_plan)==FALSE && tfrmt$row_grp_plan$label_loc[1] != "indented"){
    stop("print_to_ggplot must have label location 'indented' if row_group_plan is present")
  }

  # stop if span structures are present
  if(is.null(tfrmt$col_plan$span_structures)==FALSE){
    stop("print_to_ggplot does not support spanning headers")
  }

  # stop if more than one column variable
  if(length(tfrmt$column)>1){
    stop("print_to_ggplot does not support multiple column variables")
  }

  # stop if column style plan added
  if(is.null(tfrmt$col_style_plan)==FALSE){
    stop("print_to_ggplot does not support col_style_plan elements")
  }

  # stop if param, column values not provided
  if (quo_is_missing(tfrmt$param)){
    stop("param variable required for print_to_ggplot")
  }
  if (is_empty(tfrmt$column)){
    stop("column variable required for print_to_ggplot")
  }
  if (quo_is_missing(tfrmt$label)){
    stop("label variable required for print_to_ggplot")
  }

  if(quo_is_missing(tfrmt$value)){
    stop("value variable required for print_to_ggplot")
  }


  # Keeping the original data of column to preserve data type later on
  column_name <- as_label(tfrmt$column[[1]])
  column_data <-.data %>%
    pull(!!column_name)

  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    cleaned_data_to_ggplot(tfrmt,column_data, ...)

}

#' Do all the formatting for the ggplot
#'
#' @param .data data to clean
#' @param tfrmt tfrmt object for formatting
#' @param column_data original column data to preserve type
#' @param ... Inputs to geom_text to modify the style of the table body
#'
#' @return ggplot object
#' @noRd
#'
#' @importFrom ggplot2 ggplot .data ylab xlab theme_void theme scale_y_discrete scale_x_continuous scale_x_discrete element_text aes geom_text margin unit element_blank
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
cleaned_data_to_ggplot <- function(.data,tfrmt,column_data, ...){

  # apply grouping if any
  # create y variable to preserve ordering and levels
  .data<-apply_grp_ggplot(.data,tfrmt) %>%
    mutate(y=n():1)

  # handle cases for "..tfrmt_row_grp_lbl pivoting
  if("..tfrmt_row_grp_lbl" %in% names(.data)){
    # reshape data for ggplot
    long_data <- .data %>%
      pivot_longer(-c(as_label(tfrmt$label),"y","..tfrmt_row_grp_lbl"),
                   names_to = "column",values_to="value") %>%
      mutate(value=if_else(.data$`..tfrmt_row_grp_lbl`==TRUE,"",.data$value))
  }else{
    long_data <- .data %>%
      pivot_longer(-c(as_label(tfrmt$label),"y"),
                   names_to = "column",values_to="value")

  }

  # preserve data types from original data - changed by pivoting
  column_type <- class(column_data)
  if(column_type=="factor"){
    column_levels<-levels(column_data)
    long_data$column<-factor(long_data$column,levels=column_levels)
  }else if(column_type == "character"){
    # preserves character column order
    column_levels <- unique(long_data$column)
    long_data$column <- factor(long_data$column, levels = column_levels)
  }

  if(column_type=="numeric"){
    long_data$column <- as.numeric(long_data$column)
    plot<- ggplot(long_data, aes(x=as.numeric(.data$column),
                                 y=as.factor(.data$y), label = .data$value)) +
      scale_x_continuous(position = "top",
                         breaks = unique(long_data$column),
                         labels = as.character(unique(long_data$column))
                         )
  }else{
    plot <- ggplot(long_data, aes(x=.data$column, y=as.factor(.data$y),
                                  label = .data$value)) +
      scale_x_discrete(position = "top")
  }

  plot +
    geom_text(...) +
    xlab("") +
    ylab("") +
    theme_void() +
    theme(
      axis.text.y = element_text(
        size = 10,
        margin = margin(r = 0),
        hjust = 0
      ),
      panel.spacing = unit(0, "mm"),
      strip.text = element_blank(),
      axis.text.x = element_text(size = 10)
    ) + # replace y values with labels
    scale_y_discrete(labels = pull(long_data, !!tfrmt$label),
                     breaks = long_data$y)

}

#' Apply grouping for ggplot
#'
#' @param .data data with group variable
#' @param tfrmt tfrmt for formatting
#'
#' @return dataset wth grouping columns combined
#' @noRd
#'
#' @importFrom rlang quo_name is_empty
#' @importFrom magrittr %>%
#' @importFrom dplyr all_of select
apply_grp_ggplot<-function(.data,tfrmt){

  if(!is.null(tfrmt$row_grp_plan) && is_empty(tfrmt$group)==FALSE &&
     tfrmt$row_grp_plan$label_loc$location == "gtdefault"){
    group_name <- quo_name(tfrmt$group[[1]])

    element<-element_row_grp_loc(location="indented", indent = "    ")

    combine_group_cols(.data,tfrmt$group,tfrmt$label,element) %>%
      select(-all_of(group_name))


  }else{
    .data
  }
}

