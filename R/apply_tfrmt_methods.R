
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
#' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' @importFrom dplyr if_else case_when tibble
#' @export
apply_frmt <- function(frmt_def, .data, values, ...){
  UseMethod("apply_frmt", frmt_def)
}

#' Apply formatting
#'
#' Applying the most basic frmt element. All other frmt type eventually call this
#' @param frmt_def formatting to be applied
#' @param .data data, but only the rows getting changed
#' @param values value column as a quosure
#' @param ... additional arguments
#'
#' @return formatted dataset
#'
#' @importFrom stringr str_count str_trim str_dup str_c str_remove str_extract str_detect
#' @importFrom dplyr case_when tibble pull mutate
#' @importFrom rlang :=
#' @export
apply_frmt.frmt <- function( frmt_def, .data, values, ...){
  vals <- .data %>%
    pull(!!values)

  if(length(vals) == 0){
    return(.data)
  }

  if(str_detect(frmt_def$expression, "[x|X]")){

    # digits following period in expression
    dig <- frmt_def$expression %>%
      str_extract("(?<=\\.)[X|x]+") %>%
      str_count(pattern = "[X|x]")

    # vals rounded and trimmed
    rounded_vals <- format(round(vals, dig), decimal.mark = ".") %>%
      str_trim()

    # digits preceding period in expression
    pre_dec_expr <- frmt_def$expression %>%
      str_remove("\\..*$") %>%
      str_count("[X|x]")

    # variables used When scientific argument supplied:
    num_sci <- format(vals, scientific = TRUE) # vals converted to standard notation
    num_pre_e <- as.numeric(str_extract(as.character(num_sci), "[^e]+")) # digits preceding 'e'
    num_rounded <- format(round(num_pre_e, dig), decimal.mark = ".") %>% str_trim() # num_pre_e rounded
    index <- str_extract(format(num_sci, scientific = TRUE), "[^e]+$") %>% as.numeric()
    multiply <- str_extract(frmt_def$scientific, "(.*)(?!$)") # character scientific argument
    sci_vals <- paste0(num_rounded, multiply, index)

    fmt_options <- tibble(
      rounded = rounded_vals,
      scientific = sci_vals,
      # digits preceding period in vals
      pre_dec_vals_max = rounded_vals %>%
        str_remove("\\..*$") %>%
        str_count(".") %>% max(),
      pre_dec_vals_sci = num_rounded %>%
        str_remove("\\..*$") %>%
        str_count("."),
      pre_dec_vals_not_sci = rounded_vals %>%
        str_remove("\\..*$") %>%
        str_count(".")) %>%
      mutate(
        # keep from being negative
        space_to_add_sci = pmax(pre_dec_expr - .data$pre_dec_vals_sci, 0),
        space_to_add_not_sci = pmax(pre_dec_vals_max - .data$pre_dec_vals_not_sci, 0)
      )

    if(!is.null(frmt_def$missing)){
      miss_val <- frmt_def$missing
    } else {
      miss_val <- NA_character_
    }

    # when scientific is null paste rounded value, if not then append scientific expression
    fmt_vals <- case_when(is.null(frmt_def$scientific) ~ str_c(str_dup(" ", fmt_options$space_to_add_not_sci), fmt_options$rounded),
                          !is.null(frmt_def$scientific) ~ str_c(str_dup(" ", fmt_options$space_to_add_sci), fmt_options$scientific))

    expr_start <- frmt_def$expression %>%
      str_extract("^[^X|^x]*(?=[X|x])")

    expr_end <- frmt_def$expression %>%
      str_extract("(?<=[X|x])[^X|^x]*$")

    # Combining the additional formatting
    fmt_val_output <- case_when(
      fmt_options$rounded == "NA" ~ miss_val,
      TRUE ~ str_c(expr_start, fmt_vals, expr_end)
    )

  } else {
    fmt_val_output <- frmt_def$expression
  }

  .data %>%
    mutate(
      !!values := fmt_val_output
    )

}


#' Apply frmt_combine information to data
#'
#' @param frmt_def frmt_combine object
#' @param .data data, but only the rows getting changed
#' @param param param column as a quosure
#' @param values value column as a quosure
#' @param column column column as a quosure
#' @param label label column as a quosure
#' @param group group column as a quosure
#' @param ... additional arguments for applying a basic frmt
#'
#' @return rounded and formatted df
#' @importFrom stringr str_extract_all str_count str_trim str_dup str_c str_remove str_glue
#' @importFrom dplyr case_when tibble filter pull left_join
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr map_chr
#' @importFrom rlang :=
#' @export
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
                paste0(miss_param_from_data, collapse = " \n")))
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
        .data$.is_all_missing ~ frmt_def$missing,
        TRUE ~ str_glue(frmt_def$expression) %>% as.character()
      )
    ) %>%
    select(-all_of(fmt_param_vals), -.data$.is_all_missing)

  ## keep only the first case of param, and add the joined values
  .data %>%
    filter(!!param == fmt_param_vals[[1]]) %>%
    select(-!!values) %>%
    left_join(
      .tmp_data_fmted,
      by = map_chr(c(column, label, group), as_label)
    )

}

#' Apply form for frmt_when formats
#'
#' @param frmt_def formatting to be applied
#' @param .data data, but only what is getting changed
#' @param values values symbol should only be one
#' @param ... additional arguments for methods
#'
#' @return rounded and formatted df
#' @export
#' @importFrom rlang as_label f_rhs f_lhs parse_exprs eval_tidy
#' @importFrom dplyr pull if_else mutate
#' @importFrom purrr map map_chr
#' @importFrom rlang :=
apply_frmt.frmt_when <- function(frmt_def, .data, values, ...){
  values_str <- as_label(values)
  n <- length(frmt_def)

  val_len <- length(pull(.data, !!values))
  right <- frmt_def %>%
    map(f_rhs) %>%
    map(function(x) {
      if(is_frmt(x)){
        out <- apply_frmt(x, .data, values, ...) %>% pull(!!values)
      } else {
        out <- rep(x, val_len)
      }
      out
    })

  left <- frmt_def %>%
    map_chr(f_lhs) %>%
    if_else(. == "TRUE", ., paste0(values_str, .)) %>%
    parse_exprs() %>%
    map(eval_tidy, .data)


  out <- rep(NA_character_, val_len)
  replaced <- rep(FALSE, val_len)

  for(i in seq_len(n)){
    out <- replace_val(out, left[[i]] & !replaced, right[[i]])
    replaced <- replaced | (left[[i]] & !is.na(left[[i]]))
  }

  .data %>%
    mutate(
      !!values := out
    )
}


