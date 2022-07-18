print_to_ggplot <- function(tfrmt, .data){
  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  if(!is.data.frame(.data)){
    stop("Requires data`")
  }

  # Find the original data type of column
  column_name <- quo_name(tfrmt$column[[1]])
  column_data<-(.data %>%
    select(!!column_name))[[1]]
  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    cleaned_data_to_ggplot(tfrmt,column_data)

}

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

  data <- .data %>%
    pivot_longer(-as_label(tfrmt$label),names_to = "column")
  column_type <- class(column_data)
  if(column_type=="factor"){
    column_levels<-levels(column_data)
    data$column<-factor(data$column,levels=column_levels)

  }
  if(column_type=="numeric"){
  ggplot(data, aes(x=as.numeric(column), y=label, label = value)) +
    geom_text(size = 3) +
   # geom_tile(fill = "white", alpha = .4, color = "black") +
  #  scale_x_continuous(expand = expansion(mult = c(0, 0)))+
    xlab("") +
    theme_void() +
    theme(axis.text.y = element_text(size = 10, margin = margin(r = 0)),
          panel.spacing = unit(0, "mm"),
          strip.text = element_blank())
  }else{
    ggplot(data, aes(x=column, y=label, label = value)) +
    #  geom_tile(fill = "white", alpha = .4, color = "black") +
      geom_text(size = 3) +
      xlab("") +
      theme_void() +
      theme(axis.text.y = element_text(size = 10, margin = margin(r = 0)),
            panel.spacing = unit(0, "mm"),
            strip.text = element_blank())
  }
}
