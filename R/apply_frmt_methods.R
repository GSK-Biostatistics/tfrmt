
#' Apply formatting
#'
#' @param .data data, but only what is getting changed
#' @param frmt_def formatting to be applied
#' @param value value symbol should only be one
#' @param mock Logical value is this is for a mock or not. By default `FALSE`
#' @param ... additional arguments for methods
#' @param param param column as a quosure
#' @param column column columns as a list of quosures
#' @param label label column as a quosure
#' @param group group column as a list of quosures
#'
#' @return formatted dataset
#'
#' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' @importFrom dplyr if_else case_when tibble
#' @export
#' @examples
#'
#' library(tibble)
#' library(dplyr)
#' # Set up data
#' df <- tibble(x = c(20.12,34.54,12.34))
#'
#' apply_frmt(
#'  frmt_def = frmt("XX.X"),
#'  .data=df,
#'  value=quo(x))
#'
#' @rdname apply_frmt
apply_frmt <- function(frmt_def, .data, value, mock = FALSE, ...){
  UseMethod("apply_frmt", frmt_def)
}


#' @importFrom stringr str_count str_trim str_dup str_c str_remove str_extract str_detect
#' @importFrom dplyr case_when tibble pull mutate
#' @importFrom rlang := as_function
#' @export
#'
#' @rdname apply_frmt
apply_frmt.frmt <- function( frmt_def, .data, value, mock = FALSE, ...){
  if(mock){
    out <- .data %>%
      mutate(!!value := frmt_def$expression)
  } else {
    vals <- .data %>%
      pull(!!value)

    if(length(vals) == 0){
      return(.data)
    } else if(!is.null(frmt_def$transform)){
      vals <- as_function(frmt_def$transform)(vals)
    }

    if(str_detect(frmt_def$expression, "[x|X]")){

      # digits following period in expression
      dig <- frmt_def$expression %>%
        str_extract("(?<=\\.)[X|x]+") %>%
        str_count("[X|x]")

      ## There were no x's after a `.` to extract, so assume none
      if(is.na(dig)){
        dig <- 0
      }

      ## convert to scientific if scientific
      if(!is.null(frmt_def$scientific)){

        vals_sci <- format(vals, scientific = TRUE)

        vals <- vals_sci %>%
          str_extract("[^e]+") %>%
          as.numeric()

        ## remove x's from end of scientific
        multiply <- str_remove(frmt_def$scientific, "[xX]+(?<=$)")
        sci_width <- str_extract(frmt_def$scientific, "[xX]+(?<=$)") %>%
          str_count("[X|x]")

        vals_sci_post <- vals_sci %>%
          str_extract("[^e]+$") %>%
          as.numeric() %>%
          format(trim = TRUE,width = sci_width) %>%
          paste0(multiply,.)

      }else{
        vals_sci_post <- ""
      }

      # digits preceding period in expression
      pre_dec_expr <- frmt_def$expression %>%
        str_remove("\\..*$") %>%
        str_count("[X|x]")

      # vals rounded and trimmed
      rounded_vals <- format(
        round(vals, dig),
        decimal.mark = ".",
        nsmall = dig
        ) %>%
        str_trim()

      fmt_options <- tibble(
        rounded = rounded_vals,
        # digits preceding period in vals
        act_pre_dec  = rounded_vals %>%
          str_remove("\\..*$") %>%
          str_count(".")
        ) %>%
        mutate(
          # keep from being negative
          space_to_add = pmax(pre_dec_expr - .data$act_pre_dec, 0),
        )


      # when scientific is null paste rounded value, if not then append scientific expression
      fmt_vals <- str_c(str_dup(" ", fmt_options$space_to_add), fmt_options$rounded, vals_sci_post)

      expr_start <- frmt_def$expression %>%
          str_extract("^[^X|^x]*(?=[X|x])")

      expr_end <- frmt_def$expression %>%
          str_extract("(?<=[X|x])[^X|^x]*$")

      if(!is.null(frmt_def$missing)){
        miss_val <- frmt_def$missing
      } else {
        miss_val <- NA_character_
      }

      # Combining the additional formatting
      fmt_val_output <- case_when(
        fmt_options$rounded == "NA" ~ miss_val,
        TRUE ~ str_c(expr_start, fmt_vals, expr_end)
      )

    } else {
      fmt_val_output <- frmt_def$expression
    }

    out <- .data %>%
      mutate(
        !!value := fmt_val_output
      )
  }

  out

}


#' @importFrom stringr str_extract_all str_count str_trim str_dup str_c str_remove str_glue
#' @importFrom dplyr case_when tibble filter pull left_join
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom purrr map_dfr map_chr discard
#' @importFrom rlang :=
#' @export
#'
#' @rdname apply_frmt
apply_frmt.frmt_combine <- function(frmt_def, .data, value, mock = FALSE, param, column, label, group, ...){

  fmt_param_vals <- frmt_def$expression %>%
    str_extract_all("(?<=\\{)[^\\}]+(?=\\})") %>%
    unlist()

  # Adding the unquoted version to match while long
  fmt_param_vals_uq <- str_remove_all(fmt_param_vals, "`")

  # Check if unspecified param values are in the dataset

  if(!setequal(names(frmt_def$frmt_ls), fmt_param_vals)){
    stop("The values in the expression don't match the names of the given formats ")
  }

  ## format params as needed
  .tmp_data <- map_dfr(fmt_param_vals, function(`__var`){
    fmt_to_apply <- frmt_def$frmt_ls[[`__var`]]
    .data %>%
      filter(!!param == str_remove_all(`__var`, "`")) %>%
      apply_frmt(
        frmt_def = fmt_to_apply,
        .data = .,
        value = value,
        column = column,
        param = param,
        label = label,
        group = group,
        mock=mock,
        ...
      )
  })

  #Test if common information exists
  miss_param_from_data <- .tmp_data %>%
    pull(!!param) %>%
    unique() %>%
    setdiff(fmt_param_vals_uq, .)

  if(length(miss_param_from_data) > 0 ){
    stop(paste0("Unable to create formatting combination because the following parameters are missing from the data:\n ",
                paste0(miss_param_from_data, collapse = " \n")))
  }

  .tmp_data_wide <- .tmp_data %>%
    select(!!value, !!param, !!!column, !!label, !!!group) %>%
    pivot_wider(
      values_from = !!value,
      names_from = !!param
    ) %>%
    mutate(
      .is_all_missing =  all_missing(fmt_param_vals,.)
    )

  missing_param_replacements <-
    map(fmt_param_vals, ~ frmt_def$frmt_ls[[.x]]$missing) %>%
    setNames(fmt_param_vals) %>%
    discard(is.null)

  if(length(missing_param_replacements)>0){
    ## after .is_all_missing so that can be tabulated first
    .tmp_data_wide <- .tmp_data_wide %>%
      replace_na(missing_param_replacements)
  }

  # check that pivot_wider resulted in a reduction of rows, which indicates that at least
  #  1 row will successfully have a frmt_combine in it
  if (nrow(.tmp_data_wide)==nrow(.tmp_data)){
    id_cols <- .tmp_data %>% select(!!!column, !!label, !!!group, !!param)
    warning(paste0("Unable to apply `frmt_combine` due to uniqueness of column/row identifiers. Params that are to be combined need to have matching values across: ",
                   paste(names(id_cols %>% select(-!!param)), collapse = ", "),
                   ". Current values:\n",
                   paste(capture.output(id_cols %>% as.data.frame), collapse = "\n")))
  }


  if(is.null(frmt_def$missing)){
    frmt_def$missing <- ""
  }

  ## if both params are missing, then drop in frmt definition missing value
  ## otherwise concat the params
  .tmp_data_fmted <- .tmp_data_wide %>%
    mutate(
      !!value := case_when(
        .data$.is_all_missing ~ frmt_def$missing,
        TRUE ~ str_glue(!!frmt_def$expression) %>% as.character()
      )
    ) %>%
    select(-all_of(fmt_param_vals_uq), -".is_all_missing")

  ## if not mock remove
  if(!mock){
    .data <- .data %>%
      select(-!!value)
  }

  merge_group <- map(
    c(column, label, group),
    function(x){
      if(!quo_is_missing(x)){x}
    }) %>%
    discard(is.null) %>%
    do.call("vars", .)

  # merge on new values, and remove cases other than first occurance of group/label/column pairing
  .data %>%
    left_join(
      .tmp_data_fmted,
      by = map_chr(merge_group, as_label)
    ) %>%
    group_by(!!!merge_group) %>%
    slice(1) %>%
    ungroup()

}

#' @export
#' @importFrom rlang as_label f_rhs f_lhs parse_exprs eval_tidy
#' @importFrom dplyr pull if_else mutate
#' @importFrom purrr map map_chr keep
#' @importFrom rlang :=
#' @importFrom tidyr replace_na
#'
#' @rdname apply_frmt
apply_frmt.frmt_when <- function(frmt_def, .data, value, mock = FALSE, ...){

  if(mock){
    frmt_to_prt <- frmt_def$frmt_ls %>%
      keep(~f_lhs(.) == "TRUE")
    if(length(frmt_to_prt) < 1){
      frmt_to_prt <- frmt_def$frmt_ls
    }
    str_to_prnt <- f_rhs(frmt_to_prt[[1]])$expression
    out <- .data %>%
      mutate(!!value := str_to_prnt)

  } else {
    values_str <- as_label(value)
    n <- length(frmt_def$frmt_ls)

    val_len <- length(pull(.data, !!value))
    right <- frmt_def$frmt_ls %>%
      map(f_rhs) %>%
      map(function(x) {
        if(is_frmt(x)){
          out <- apply_frmt(x, .data, value, ...) %>% pull(!!value)
        } else {
          out <- rep(x, val_len)
        }
        out
      })

    left <- frmt_def$frmt_ls %>%
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

    if (is.null(frmt_def$missing)){
      out <- out
    } else if (!is.null(frmt_def$missing)){
      out <- out %>% replace_na(replace = frmt_def$missing)
    }

    out <- .data %>%
      mutate(
        !!value := out
      )
  }
  out

}
