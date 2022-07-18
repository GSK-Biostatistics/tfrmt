#' Print to ggplot
#'
#' @param tfrmt tfrmt object that will dictate the structure of the ggplot object
#' @param .data Data to style in order to make the ggplot object
#'
#' @return a stylized ggplot object
#' @export

print_to_ggplot <- function(tfrmt, .data){
  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  if(!is.data.frame(.data)){
    stop("Requires data`")
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
cleaned_data_to_ggplot <- function(.data,tfrmt,column_data){

  # fill param, column if not provided
  if (quo_is_missing(tfrmt$param)){
    message("`tfrmt` will need a `param` value to `print_to_ggplot` when data is avaliable")
    tfrmt$param <- quo(!!sym("__tfrmt__param"))
  }
  if (is_empty(tfrmt$column)){
    message("`tfrmt` will need `column` value(s) to `print_to_ggplot` when data is avaliable")
    tfrmt$column <- vars(!!sym("__tfrmt__column"))
  }

  if(quo_is_missing(tfrmt$values)){
    message("Message: `tfrmt` will need `values` value to `print_to_ggplot` when data is avaliable")
    tfrmt$values <- quo(!!sym("__tfrmt__val"))
  }

  # apply grouping if any
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
    ) +
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

