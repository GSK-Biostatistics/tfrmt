#' Print to ggplot
#'
#' @param tfrmt tfrmt object that will dictate the structure of the ggplot object
#' @param .data Data to style in order to make the ggplot object
#'
#' @return a stylized ggplot object
#' @export
#'
#' @importFrom rlang quo_is_missing quo_name
#' @importFrom dplyr select
#' @importFrom magrittr %>%

print_to_ggplot <- function(tfrmt, .data){
  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  if(!is.data.frame(.data)){
    stop("Requires data")
  }

  if(is.null(tfrmt$body_plan)==FALSE | is.null(tfrmt$col_plan)==FALSE | is.null(tfrmt$row_grp_plan)==FALSE | is.null(tfrmt$col_align_plan)==FALSE){
    stop("print_to_ggplot is not currently compatible with Body Plan, Row Group Plan, Column Plan or Column Alignment Plan. Please remove before continuing.")
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

  if(quo_is_missing(tfrmt$values)){
    stop("values variable required for print_to_ggplot")
  }


  # Keeping the original data of column to preserve data type later on
  column_name <- quo_name(tfrmt$column[[1]])
  column_data<-(.data %>%
    select(!!column_name))[[1]]

  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    cleaned_data_to_ggplot(tfrmt,column_data)

}

#' Do all the formatting for the ggplot
#'
#' @param .data data to clean
#' @param tfrmt tfrmt object for formatting
#' @param column_data original column data to preserve type
#'
#' @return ggplot object
#' @noRd
#'
#' @importFrom ggplot2 ggplot xlab theme_void theme scale_y_discrete element_text
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
cleaned_data_to_ggplot <- function(.data,tfrmt,column_data){

  # apply grouping if any
  # create y variable to preserve ordering and levels
  .data<-apply_grp_ggplot(.data,tfrmt) %>%
    mutate(y=n():1)

  # reshape data for ggplot
  data <- .data %>%
    pivot_longer(-c(as_label(tfrmt$label),"y"),names_to = "column")


  # preserve data types from original data - changed by pivoting
  column_type <- class(column_data)
  if(column_type=="factor"){
    column_levels<-levels(column_data)
    data$column<-factor(data$column,levels=column_levels)
  }
  if(column_type=="numeric"){
  plot<- ggplot(data, aes(x=as.numeric(column), y=as.factor(y), label = value))
  }else{
  plot <- ggplot(data, aes(x=column, y=as.factor(y), label = value))
  }

  plot +
    geom_text(size = 3) +
    xlab("") +
    theme_void() +
    theme(
      axis.text.y = element_text(
        size = 10,
        margin = margin(r = 0),
        hjust = 0
      ),
      panel.spacing = unit(0, "mm"),
      strip.text = element_blank()
    ) + # replace y values with labels
    scale_y_discrete(labels = data$label, breaks = data$y)


}

#' Apply grouping for ggplot
#'
#' @param .data data with group variable
#' @param tfrmt tfrmt for formatting
#'
#' @return dataset wth grouping columns combined
#' @noRd
#'
#' @importFrom rlang quo_name
#' @importFrom magrittr %>%
apply_grp_ggplot<-function(.data,tfrmt){


  if(is_empty(tfrmt$group)){
    data<-.data
  }else{
    group_name <- quo_name(tfrmt$group[[1]])

    element<-element_row_grp_loc(location="indented")

    combine_group_cols(.data,tfrmt$group,tfrmt$label,element) %>%
      select(-group_name)


  }

}

