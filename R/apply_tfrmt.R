
#' @import dplyr
#'
#'
NULL


#' Apply formatting
#'
#' @param .data data, but only what is getting changed
#' @param frmt_def formatting to be applied
#' @param values values symbol should only be one
#' @param ... additional arguments for methods
#'
#' @noRd
#' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' @importFrom dplyr if_else case_when tibble
apply_frmt <- function(frmt_def, .data, values, ...){
  UseMethod("apply_frmt", frmt_def)
}

#' Apply formatting
#'
#'
#' SCIENFIC NOTATION????
#' @param vals vector of numeric values
#' @param frmt_def formatting to be applied
#'
#' @return
#' @noRd
#' @importFrom stringr str_count str_trim str_dup str_c str_remove str_extract
#' @importFrom dplyr case_when tibble
#' @importFrom purrr pluck
apply_frmt.frmt <- function( frmt_def, .data, values, ...){

  vals <- .data %>%
    select(!!values) %>%
    pluck(1)

  if(length(vals) == 0){
    return(.data)
  }

  #Apply Expression
  dig <- frmt_def$expression %>%
    str_count("(?<=\\.)[X|x]")

  rounded_vals <- format(round(vals, dig)) %>%
    str_trim()

  pre_dec <- frmt_def$expression %>%
    str_remove("\\..*$") %>%
    str_count("[X|x]")

  fmt_options <- tibble(
    rounded = rounded_vals,
    act_pre_dec = rounded_vals %>%
      str_remove("\\..*$") %>%
      str_count("."),
    space_to_add = pmax(pre_dec - act_pre_dec,0) ## keep from being negative
  )

  if(!is.null(frmt_def$missing)){
    miss_val <- frmt_def$missing
  } else {
    miss_val <- NA_character_
  }

  fmt_vals <- str_c(str_dup(" ", fmt_options$space_to_add), fmt_options$rounded)

  expr_start <- frmt_def$expression %>%
    str_extract("^[^X|^x]*(?=[X|x])")

  expr_end <- frmt_def$expression %>%
    str_extract("(?<=[X|x])[^X|^x]*$")

  # Combining the additional formatting
  fmt_val_output <- case_when(
    fmt_options$rounded == "NA" ~ miss_val,
    TRUE ~ str_c(frmt_def$padding, expr_start, fmt_vals, expr_end)
    )

  .data %>%
    mutate(
      !!values := fmt_val_output
    )

}


#' Apply frmt_combine information to data
#'
#' @param .data data, but only what is getting changed
#' @param fmt_combine
#' @param param
#' @param values
#'
#' @return rounded and formatted df
#' @importFrom stringr str_extract_all str_count str_trim str_dup str_c str_remove str_glue
#' @importFrom dplyr case_when tibble filter pull left_join
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr map_chr
#' @noRd
apply_frmt.frmt_combine <- function(frmt_def, .data, values, param, column, label, group, ...){

  fmt_param_vals <- frmt_def$expression %>%
    str_extract_all("(?<=\\{)[^\\}]+(?=\\})") %>%
    unlist()
  # Check if unspecified param values are in the dataset

  if(!setequal(names(frmt_def$fmt_ls), fmt_param_vals)){
    stop("The values in the expression don't match the names of the given formats ")
  }

  ## format params as needed
  .tmp_data <- map_dfr(fmt_param_vals, function(var){
    fmt_to_apply <- frmt_def$fmt_ls[[var]]
    .data %>%
      filter(!!param == var) %>%
      apply_frmt(
        frmt_def = fmt_to_apply,
        .data = .,
        values = values,
        column = column,
        param = param,
        label = label,
        group = group,
        ...
        )
  })

  #Test if common information exists
  miss_param_from_data <- .tmp_data %>%
    pull(!!param) %>%
    unique() %>%
    setdiff(fmt_param_vals, .)

  if(length(miss_param_from_data) > 0 ){
    stop(paste0("Unable to create formatting combination because the following parameters are missing from the data:\n ",
                paste0(miss_from_data, collapse = " \n")))
  }

  .tmp_data_wide <- .tmp_data %>%
    select(!!values, !!param, !!column, !!label, !!!group) %>%
    pivot_wider(
      values_from = !!values,
      names_from = !!param
      ) %>%
    mutate(
      .is_all_missing =  all_missing(fmt_param_vals,.)
    )

  if(is.null(frmt_def$missing)){
    frmt_def$missing <- ""
  }


  ## if both params are missing, then drop in frmt definition missing value
  ## otherwise concat the params
  .tmp_data_fmted <- .tmp_data_wide %>%
    mutate(
      !!values := case_when(
        .is_all_missing ~ frmt_def$missing,
        TRUE ~ str_glue(frmt_def$expression) %>% as.character()
        )
      ) %>%
    select(-all_of(fmt_param_vals), -.is_all_missing)

  ## keep only the first case of param, and add the joined values
  .data %>%
    filter(!!param == fmt_param_vals[[1]]) %>%
    select(-!!values) %>%
    left_join(
      .tmp_data_fmted,
      by = map_chr(c(column, label, group), as_label)
    )

}

#' @importFrom rlang parse_expr eval_bare
all_missing <- function(cols, .data){
  paste0("is.na(.data$",cols,")", collapse = " & ") %>%
    parse_expr() %>%
    eval_bare(env = environment())
}
