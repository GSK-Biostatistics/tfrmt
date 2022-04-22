#' @import dplyr
#'
#'
NULL


#' Apply formatting
#'
#'
#' #' SCIENFIC NOTATION????
#' #' @param vals vector of numeric values
#' #' @param fmt formatting to be applied
#' #'
#' #' @return
#' #' @noRd
#' #' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' #' @importFrom dplyr if_else case_when tibble
#' #' @importFrom purrr map_lgl
#' apply_fmt <- function(vals, fmt){
#'
#'   #Round
#'   dig <- fmt$expression %>%
#'     str_count("(?<=\\.)[X|x]")
#'   rounded_vals <- format(round(vals, dig)) %>%
#'     str_trim()

#'
#'   #Bound
#'   if(!is_null(fmt$bounds$upper_exp)){
#'     up_bound_lb <- str_c(vals, fmt$bounds$upper_exp) %>%
#'       map_lgl(~eval(parse(text =.))) %>%
#'       if_else(., fmt$bounds$upper_lab, NA_character_)
#'   } else {
#'     up_bound_lb <- rep(NA_character_, length(vals))
#'   }
#'   if(!is_null(fmt$bounds$lower_exp)){
#'     low_bound_lb <- str_c(vals, fmt$bounds$lower_exp) %>%
#'       map_lgl(~eval(parse(text =.))) %>%
#'       if_else(., fmt$bounds$lower_lab, NA_character_)
#'   } else {
#'     low_bound_lb <- rep(NA_character_, length(vals))
#'   }
#'   bound <- case_when(!is.na(up_bound_lb) & !is.na(low_bound_lb) ~ "THIS IS AN ISSUE",
#'                      !is.na(up_bound_lb) ~ up_bound_lb,
#'                      !is.na(low_bound_lb) ~ low_bound_lb,
#'                      TRUE ~ NA_character_
#'   )
#'   if("THIS IS AN ISSUE" %in% bound){
#'     stop("Overlapping bounds")
#'   }
#'

#'   pre_dec <- fmt$expression %>%
#'     str_remove("\\..*$") %>%
#'     str_count("[X|x]")
#'
#'   fmt_options <- tibble(
#'     rounded = rounded_vals,
#'     bound = bound,
#'     act_pre_dec = rounded_vals %>%
#'       str_remove("\\..*$") %>%
#'       str_count("."),
#'     space_to_add = if_else(!is.na(bound), 0L, pre_dec - act_pre_dec)
#'   )
#'
#'   if(any(fmt_options$space_to_add < 0)){
#'     stop("Check format, there largest value is larger than expected")
#'   }
#'
#'   if(!is.null(fmt$missing)){
#'     miss_val <- fmt$missing
#'   } else {
#'     miss_val <- NA_character_
#'   }
#'
#'   fmt_vals <- case_when(!is.na(fmt_options$bound) ~ fmt_options$bound,
#'                         TRUE ~ str_c(str_dup(" ", fmt_options$space_to_add),
#'                                      fmt_options$rounded))
#'
#'
#'   start <- fmt$expression %>%
#'     str_extract("^[^X|^x]*(?=[X|x])")
#'   end <- fmt$expression %>%
#'     str_extract("(?<=[X|x])[^X|^x]*$")
#'
#'   # Combining the additional formatting
#'   case_when(fmt_options$bound == "" ~ "",
#'             fmt_options$rounded == "NA" ~ miss_val,
#'             TRUE ~ str_c(fmt$padding, start, fmt_vals, end))
#' }
#'
#'
#' #' Apply fmt_combine information to data
#' #'
#' #' @param .data data, but only what is getting changed
#' #' @param fmt_combine
#' #' @param param
#' #' @param values
#' #'
#' #' @return rounded and formatted df
#' #' #' @noRd
#' apply_combo_fmt <- function(.data, fmt_combine, param, values){
#'   param_vals <- fmt_combine$expression %>%
#'     str_extract_all("(?<=\\{)[^\\}]+(?=\\})") %>%
#'     unlist()
#'   # Check if unspecified param values are in the dataset
#'
#'   if(!setequal(names(fmt_combine$fmt_ls), param_vals)){
#'     stop("The values in the expression don't match the names of the given formats ")
#'   }
#'   out <- map_dfr(param_vals, function(var){
#'     fmt <- fmt_combine$fmt_ls[[var]]
#'     .data %>%
#'       filter(!!param == var) %>%
#'       mutate(!!values := apply_fmt(!!values, fmt))
#'   })
#'   #Test if common information exists
#'   miss_from_data <- out %>%
#'     pull(!!param) %>%
#'     unique()%>%
#'     setdiff(param_vals, .)
#'   if(length(miss_from_data) > 0 ){
#'     stop(paste0("Unable to create formatting combination because the following parameters are missing from the data:\n ",
#'                 paste0(miss_from_data, collapse = " \n")))
#'   }
#'   test <- out %>%
#'     select(-!!param, -!!values) %>%
#'     distinct() %>%
#'     nrow()
#'   if(test == nrow(out)){
#'     stop("Unique information exsists in rows that should be combined. Unable to combine")
#'   }
#'   out %>%
#'     pivot_wider(values_from = !!values,
#'                 names_from = !!param) %>%
#'     mutate(!!values := str_glue(fmt_combine$expression) %>% as.character()) %>%
#'     select(-all_of(param_vals))
#'   #TODO MANAGE MISSING only when both are missing
#' }






#' Test of the frmt of the data
#'
#' @param cur_fmt current formatting
#' @param data data to test against NOTE: `TEMP_row` must be in the dataset
#' @param label label symbol should only be one
#' @param group list of the group parameters
#' @param param param symbol should only be one
#'
#' @return vector of the rows which this format could be applied to
#' @noRd
fmt_test_data <- function(cur_fmt, .data, label, group, param){
  #get filters for each column type
  grp_expr <- expr_to_filter(group, cur_fmt$group_val)

  lbl_expr <- expr_to_filter(label, cur_fmt$label_val)
  parm_expr <- expr_to_filter(param, cur_fmt$param_val)

  filter_expr <- paste(lbl_expr,
                       "&",
                       grp_expr,
                       "&",
                       parm_expr) %>%
    parse_expr()

  .data %>%
    filter(!!filter_expr) %>%
    pull(TEMP_row)
}


expr_to_filter <- function(cols, val){
  UseMethod("expr_to_filter", cols)
}

expr_to_filter.quosure <- function(cols, val){
  if(all(val == ".default")){ # This is all so it works when there is a list
    out <- "TRUE"
  } else {
    out <- as_label(cols) %>%
      paste0(" %in% c('",
             paste0(val, collapse = "', '"),
             "')")
  }
  out
}

expr_to_filter.quosures <- function(cols, val){
  if(!is.list(val) & length(cols) == 1){
    cols <- cols[[1]]
    out <- expr_to_filter(cols,val)
  } else if(!is.list(val) && val == ".default"){
    out <- "TRUE"
  }else if(!is.list(val)){
    stop("If multiple groups are provided group_val must be a named list")
  }else{
    stopifnot(all(names(val) %in% map_chr(cols, as_label)))
    out <- map2_chr(names(val), val, function(col, x){
      paste0(col, " %in% c('",
             paste0(x, collapse = "', '"),
             "')")
    }) %>%
      paste0(collapse = "&")
  }
  out
}


#' Apply the formatting to all values in the dataset
#'
#' @param .data data
#' @param element_style styling element needed
#' @param group symbolic list of grouping
#' @param label symbolic label
#' @param param symbolic parameter
#' @param values symbolic value
#'
#' @noRd
#' @importFrom dplyr tibble mutate group_by arrange slice bind_cols group_split pull select starts_with
#' @importFrom purrr map map_dfr
#' @importFrom tidyr unnest
#' @importFrom rlang !!
apply_table_frmt_plan <- function(.data, table_frmt_plan, group, label, param, values, column, ...){

  ## identify which formatting needs to be applied where
  .data <- .data %>%
    ungroup() %>%
    mutate(TEMP_row = row_number())

  TEMP_appl_row = table_frmt_plan %>%
    map(fmt_test_data, .data, label, group, param)
  TEMP_fmt_to_apply = table_frmt_plan %>% map(~.$frmt_to_apply[[1]])

  dat_plus_fmt <- tibble(
    TEMP_appl_row,
    TEMP_fmt_to_apply) %>%
    # TODO? add a warning if a format isn't applied anywhere?
    mutate(TEMP_fmt_rank = row_number()) %>%
    unnest(cols = c(TEMP_appl_row)) %>%
    group_by(TEMP_appl_row) %>%
    #TODO add warning if there are rows not covered
    arrange(TEMP_appl_row, desc(TEMP_fmt_rank)) %>%
    slice(1) %>%
    left_join(.data, ., by= c("TEMP_row" = "TEMP_appl_row")) %>%
    group_by(TEMP_fmt_rank) %>%
    group_split()

  ## apply formatting
  dat_plus_fmt %>%
    map_dfr(function(x){

      cur_fmt <- x %>%
        pull(TEMP_fmt_to_apply) %>%
        .[1] %>%
        .[[1]]

      # Remove all the temporary variables
      data_only <- x %>%
        select(-starts_with("TEMP_"))

      if(is.null(cur_fmt)){
        out <- data_only %>%
          mutate(!!values := as.character(!!values)) %>%
          select(-!!param)

        # Add message
        x %>%
          pull(TEMP_row) %>%
          paste0(collapse = ", ") %>%
          paste("The following rows of the given dataset have no format applied to them", .) %>%
          message()
      }else{

        ## apply the formatting based on method of cur_fmt
        out <- apply_frmt(
          frmt_def = cur_fmt,
          .data = data_only,
          values = values,
          param = param,
          column = column,
          label = label,
          group = group
          ) %>%
          select(-!!param)
      }

      out
    })
}
