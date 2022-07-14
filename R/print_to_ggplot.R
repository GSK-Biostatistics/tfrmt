print_to_ggplot <- function(tfrmt, .data){
  if(!is_tfrmt(tfrmt)){
    stop("Requires a tfrmt object")
  }

  if(!is.data.frame(.data)){
    stop("Requires data`")
  }
  apply_tfrmt(.data, tfrmt, mock = FALSE) %>%
    cleaned_data_to_ggplot(tfrmt)

}

cleaned_data_to_ggplot <- function(.data,tfrmt){

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
    pivot_longer(-as_label(tfrmt$label),names_to = "time")


  ggplot(data, aes(x=time, y=label, label = value)) +
    geom_text(size = 3) +
    xlab("") +
   # theme_void() +
    theme(axis.text.y = element_text(size = 10, margin = margin(r = 0)),
          panel.spacing = unit(0, "mm"),
          strip.text = element_blank())
}
