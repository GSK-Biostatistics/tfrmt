
#' @import dplyr
#'
#'
NULL




pull_current_fmt <- function(tfmt, element = NULL){

}



#' Apply formatting
#'
#'
#' SCIENFIC NOTATION????
#' @param vals vector of numeric values
#' @param fmt formatting to be applied
#'
#' @return
#' @noRd
#' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' @importFrom dplyr if_else case_when tibble
#' @importFrom purrr map_lgl
apply_fmt <- function(vals, fmt){
  #Round
  dig <- fmt$rounding %>%
    str_count("(?<=\\.)[X|x]")
  rounded_vals <- format(round(vals, dig)) %>%
    str_trim()

  #Bound
  if(!is_null(fmt$bounds$upper_exp)){
    up_bound_lb <- str_c(vals, fmt$bounds$upper_exp) %>%
      map_lgl(~eval(parse(text =.))) %>%
      if_else(., fmt$bounds$upper_lab, NA_character_)
  } else {
    up_bound_lb <- rep(NA_character_, length(vals))
  }
  if(!is_null(fmt$bounds$lower_exp)){
    low_bound_lb <- str_c(vals, fmt$bounds$lower_exp) %>%
      map_lgl(~eval(parse(text =.))) %>%
      if_else(., fmt$bounds$lower_lab, NA_character_)
  } else {
    low_bound_lb <- rep(NA_character_, length(vals))
  }
  bound <- case_when(!is.na(up_bound_lb) & !is.na(low_bound_lb) ~ "THIS IS AN ISSUE",
                     !is.na(up_bound_lb) ~ up_bound_lb,
                     !is.na(low_bound_lb) ~ low_bound_lb,
                     TRUE ~ NA_character_
  )
  if("THIS IS AN ISSUE" %in% bound){
    stop("Overlapping bounds")
  }
  pre_dec <- fmt$rounding %>%
    str_remove("\\..*$") %>%
    str_count("[X|x]")

  fmt_options <- tibble(
    rounded = rounded_vals,
    bound = bound,
    act_pre_dec = rounded_vals %>%
      str_remove("\\..*$") %>%
      str_count("."),
    space_to_add = if_else(!is.na(bound), 0L, pre_dec - act_pre_dec)
  )

  if(any(fmt_options$space_to_add < 0)){
    stop("Check format, there largest value is larger than expected")
  }

  fmt_vals <- case_when(!is.na(fmt_options$bound) ~ fmt_options$bound,
            fmt_options$rounded == "NA" ~ NA_character_,
            TRUE ~ str_c(str_dup(" ", fmt_options$space_to_add),
                         fmt_options$rounded))


  start <- fmt$rounding %>%
    str_extract("^[^X|^x]*(?=[X|x])")
  end <- fmt$rounding %>%
    str_extract("(?<=[X|x])[^X|^x]*$")

  # Combining the additional formatting
  case_when(fmt_options$bound == "" ~ "",
            TRUE ~ str_c(fmt$padding, start, fmt_vals, end))
}

# apply_fmt_combine(.data, param, values, fmt){
#
#   names(fmt$fmt_ls)
#
# }

