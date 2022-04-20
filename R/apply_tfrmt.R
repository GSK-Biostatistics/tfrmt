
#' #' @import dplyr
#' #'
#' #'
#' NULL
#'
#' #' Apply the formatting to all values in the dataset
#' #'
#' #' @param .data data
#' #' @param element_style styling element needed
#' #' @param group symbolic list of grouping
#' #' @param label symbolic label
#' #' @param param symbolic parameter
#' #' @param values symbolic value
#' #'
#' #' @noRd
#' #' @importFrom dplyr tibble mutate group_by arrange slice bind_cols group_split pull select starts_with
#' #' @importFrom purrr map map_dfr
#' #' @importFrom tidyr unnest
#' #' @importFrom rlang !!
#' apply_table_frmt_plan <- function(.data, table_frmt_plan, group, label, param, values, column){
#'
#'   .data$`..frmt_values` <- ""
#'   .data$`..applied_frmt_values` <- FALSE
#'   browser()
#'
#'   for(frmt_struct in rev(table_frmt_plan)){
#'
#'     .data <- apply_frmt_structure(
#'        struct = frmt_struct,
#'        .data = .data,
#'        group = group,
#'        label = label,
#'        param = param,
#'        values = values,
#'        column = column,
#'        frmt_values = "..frmt_values",
#'        applied_frmt_values = "..applied_frmt_values"
#'     )
#'   }
#'
#'
#'
#'   .data %>%
#'     mutate(
#'       !!values := case_when(
#'         `..applied_frmt_values` == TRUE ~ `..frmt_values`,
#'         TRUE ~ as.character(!!values)
#'       )
#'     ) %>%
#'     select(-`..applied_frmt_values`, - `..frmt_values`, -!!param)
#'
#' }
#'
#'
#'
#' #' Apply format structure to data
#' #'
#' #'
#' #' @param struct format structure
#' #' @param .data data to apply format structure to
#' #' @param group list of the group parameters
#' #' @param label label symbol should only be one
#' #' @param column column symbol should only be one
#' #' @param param param symbol should only be one
#' #' @param values values symbol should only be one
#' #' @param frmt_values column name to write applied format to
#' #' @param applied_frmt_values column name to document that format has been applied
#' #'
#' #' @return
#' #' @noRd
#' #' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' #' @importFrom dplyr if_else case_when tibble
#' #' @importFrom purrr map_lgl
#'
#' apply_frmt_structure <- function(struct, .data, group, label, column, param, values, frmt_values, applied_frmt_values){
#'   struct_locs <- idx_frmt_group_lab_rows(
#'     struct_to_apply = struct,
#'     .data = .data,
#'     group = group,
#'     label = label,
#'     param = param
#'     )
#'
#'   .data_sub <- .data[struct_locs,]
#'
#'   struct_frmts <- unique(tibble(param = struct$param, frmt_to_apply = struct$frmt_to_apply))
#'   ## set default to always be last
#'
#'   if(".default" %in% struct_frmts$param & nrow(struct_frmts) > 1){
#'     struct_frmts <- bind_rows(
#'       struct_frmts[struct_frmts$param != ".default",],
#'       struct_frmts[struct_frmts$param == ".default",]
#'     )
#'   }
#'
#'   for(frmt_idx in seq_len(nrow(struct_frmts))){
#'
#'     frmt_to_apply <- struct_frmts$frmt_to_apply[[frmt_idx]]
#'     param_out_loc <- struct_frmts$param[[frmt_idx]]
#'
#'     .data_sub <- apply_frmt(
#'         frmt_def = frmt_to_apply,
#'         .data = .data_sub,
#'         values = values,
#'         group = group,
#'         label = label,
#'         column = column,
#'         param = param,
#'         param_val = param_out_loc,
#'         frmt_values = frmt_values,
#'         applied_frmt_values = applied_frmt_values
#'       )
#'   }
#'
#'   .data <- bind_rows(
#'     .data[!struct_locs,],
#'     .data_sub
#'   )
#'
#'   .data
#' }
#'
#'
#' #' Apply formatting
#' #'
#' #'
#' #' SCIENFIC NOTATION????
#' #' @param .data data, but only what is getting changed
#' #' @param frmt_def formatting to be applied
#' #' @param param param symbol should only be one
#' #' @param column column symbol should only be one
#' #' @param values values symbol should only be one
#' #'
#' #' @return
#' #' @noRd
#' #' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' #' @importFrom dplyr if_else case_when tibble
#' #' @importFrom purrr map_lgl
#' apply_frmt <- function(frmt_def, .data, values, param, param_def, ..., frmt_values, applied_frmt_values){
#'
#'   UseMethod("apply_frmt", frmt_def)
#' }
#'
#' #' Apply formatting
#' #'
#' #'
#' #' SCIENFIC NOTATION????
#' #' @param vals vector of numeric values
#' #' @param frmt_def formatting to be applied
#' #'
#' #' @return
#' #' @noRd
#' #' @importFrom stringr str_count str_trim str_dup str_c str_remove
#' #' @importFrom dplyr if_else case_when tibble
#' #' @importFrom purrr map_lgl
#' apply_frmt.frmt <- function( frmt_def, .data, values, param, param_val, ..., frmt_values, applied_frmt_values){
#'   if(param_val == ".default"){
#'      vals <- .data %>%
#'        filter(!(!!sym(applied_frmt_values))) %>%
#'        select(!!values) %>%
#'        pluck(1)
#'   }else{
#'     vals <- .data %>%
#'       filter(!(!!sym(applied_frmt_values))) %>%
#'       filter(!!param == param_val) %>%
#'       select(!!values) %>%
#'       pluck(1)
#'   }
#'
#'   if(length(vals) == 0){
#'     return(.data)
#'   }
#'
#'   #Apply Expression
#'   dig <- frmt_def$expression %>%
#'     str_count("(?<=\\.)[X|x]")
#'
#'   rounded_vals <- format(round(vals, dig)) %>%
#'     str_trim()
#'
#'   pre_dec <- frmt_def$expression %>%
#'     str_remove("\\..*$") %>%
#'     str_count("[X|x]")
#'
#'   fmt_options <- tibble(
#'     rounded = rounded_vals,
#'     act_pre_dec = rounded_vals %>%
#'       str_remove("\\..*$") %>%
#'       str_count("."),
#'     space_to_add = pre_dec - act_pre_dec
#'   )
#'
#'   if(any(fmt_options$space_to_add < 0)){
#'     stop("Check format, there largest value is larger than expected")
#'   }
#'
#'   if(!is.null(frmt_def$missing)){
#'     miss_val <- frmt_def$missing
#'   } else {
#'     miss_val <- NA_character_
#'   }
#'
#'   fmt_vals <- str_c(str_dup(" ", fmt_options$space_to_add), fmt_options$rounded)
#'
#'   expr_start <- frmt_def$expression %>%
#'     str_extract("^[^X|^x]*(?=[X|x])")
#'
#'   expr_end <- frmt_def$expression %>%
#'     str_extract("(?<=[X|x])[^X|^x]*$")
#'
#'   # Combining the additional formatting
#'   fmt_val_output <- case_when(
#'     fmt_options$rounded == "NA" ~ miss_val,
#'     TRUE ~ str_c(frmt_def$padding, expr_start, fmt_vals, expr_end)
#'     )
#'
#'   if(param_val == ".default"){
#'     .data[!.data[[applied_frmt_values]], frmt_values] <- fmt_val_output
#'     .data[!.data[[applied_frmt_values]], applied_frmt_values] <- TRUE
#'   }else{
#'     .data[.data[[as_label(param)]] %in% param_val & !.data[[applied_frmt_values]], frmt_values] <- fmt_val_output
#'     .data[.data[[as_label(param)]] %in% param_val & !.data[[applied_frmt_values]], applied_frmt_values] <- TRUE
#'   }
#'
#'   .data
#'
#' }
#'
#'
#' #' Apply frmt_combine information to data
#' #'
#' #' @param .data data, but only what is getting changed
#' #' @param fmt_combine
#' #' @param param
#' #' @param values
#' #'
#' #' @return rounded and formatted df
#' #' @noRd
#' apply_frmt.frmt_combine <- function(frmt_def, .data, values, column, param, param_val, label, group, ..., frmt_values, applied_frmt_values){
#'
#'   fmt_param_vals <- frmt_def$expression %>%
#'     str_extract_all("(?<=\\{)[^\\}]+(?=\\})") %>%
#'     unlist()
#'   # Check if unspecified param values are in the dataset
#'
#'   if(!setequal(names(frmt_def$fmt_ls), fmt_param_vals)){
#'     stop("The values in the expression don't match the names of the given formats ")
#'   }
#'
#'
#'   .tmp_data <- .data
#'   .tmp_data$..frmt_values <- ""
#'   .tmp_data$..is_frmt_values <- FALSE
#'
#'   .tmp_data <- map_dfr(fmt_param_vals, function(var){
#'
#'     frmt_param <- frmt_def$fmt_ls[[var]]
#'     apply_frmt(
#'       frmt_def = frmt_param,
#'       .data = .tmp_data,
#'       values = values,
#'       param = param,
#'       param_val = var,
#'       frmt_values = "..frmt_values",
#'       applied_frmt_values = "..is_frmt_values"
#'       ) %>%
#'       filter(!!param == var)
#'     }) %>%
#'     select(-!!values) %>%
#'     rename(
#'       !!values := `..frmt_values`
#'     ) %>%
#'     select(-`..is_frmt_values`, -(!!applied_frmt_values))
#'
#'   #Test if common information exists
#'   miss_from_data <- .tmp_data %>%
#'     pull(!!param) %>%
#'     unique() %>%
#'     setdiff(fmt_param_vals, .)
#'
#'   if(length(miss_from_data) > 0 ){
#'     stop(paste0("Unable to create formatting combination because the following parameters are missing from the data:\n ",
#'                 paste0(miss_from_data, collapse = " \n")))
#'   }
#'
#'   test <- .tmp_data %>%
#'     select(-!!param, -!!values) %>%
#'     distinct() %>%
#'     nrow()
#'
#'   if(test == nrow(.tmp_data)){
#'     stop("Unique information exsists in rows that should be combined. Unable to combine")
#'   }
#'
#'   if(param_val == ".default"){
#'     param_val <- fmt_param_vals[[1]]
#'   }
#'
#'   .tmp_data_fmted <- .tmp_data %>%
#'     select(!!values, !!param, !!column, !!label, !!!group) %>%
#'     pivot_wider(
#'       values_from = !!values,
#'       names_from = !!param
#'       ) %>%
#'     mutate(
#'       !!frmt_values := str_glue(frmt_def$expression) %>% as.character(),
#'       !!applied_frmt_values := TRUE,
#'       !!param := param_val
#'       ) %>%
#'     select(-all_of(fmt_param_vals))
#'
#'   .merged_data <- .data %>%
#'       filter(
#'         !!param == param_val,
#'         !(!!sym(applied_frmt_values))
#'       )
#'
#'   .reserved_data <- .data %>%
#'       filter(
#'         !!param %in% setdiff(unique(.data[[as_label(param)]]),fmt_param_vals) |
#'           !(!!param %in% fmt_param_vals & !(!!sym(applied_frmt_values)))
#'       )
#'
#'
#'   .merged_data <- .merged_data %>%
#'     select(
#'       -all_of(c(frmt_values,applied_frmt_values))
#'     ) %>%
#'     left_join(
#'       .tmp_data_fmted,
#'       by = map_chr(c(param, column, label, group), as_label)
#'     )
#'
#'   .data <- bind_rows(
#'     .reserved_data,
#'     .merged_data
#'   )
#'   #TODO MANAGE MISSING only when both are missing
#'
#'   .data
#'
#' }
#'
#' #' Test of the frmt of the data
#' #'
#' #' @param frmt_to_apply frmt to apply
#' #' @param data data to test against
#' #' @param group list of the group parameters
#' #' @param label label symbol should only be one
#' #' @param param param symbol should only be one
#' #'
#' #' @return vector of the rows which this format could be applied to
#' #' @noRd
#' idx_frmt_group_lab_rows <- function(struct_to_apply, .data, group, label, param){
#'   group_locs <- eval_locs(group, .data, struct_to_apply$group[[1]])
#'   label_locs <- eval_locs(label, .data, struct_to_apply$label)
#'   group_locs & label_locs
#' }
#'
#' eval_locs <- function(cols, .data, values){
#'   UseMethod("eval_locs", cols)
#' }
#'
#' eval_locs.quosure <- function(cols, .data, values){
#'
#'   if(".default" %in% values){
#'     rep(TRUE, nrow(.data))
#'   }else{
#'     .data %>%
#'       mutate(
#'         .in_values = (!!cols) %in% values
#'       ) %>%
#'       pluck(".in_values")
#'   }
#'
#' }
#'
#' eval_locs.quosures <- function(cols, .data, values){
#'
#'   if(!is.list(values)){
#'     cols <- cols[[1]]
#'     eval_locs(cols, .data, values)
#'
#'   }else{
#'     stopifnot(all(names(values) %in% sapply(cols, as_label)))
#'     .in_values_locs <- lapply(names(values),function(new_col){
#'       new_quo <- quo(!!sym(new_col))
#'       eval_locs(new_quo, .data, values[[new_col]])
#'     })
#'     Reduce(`&`, are_locs)
#'   }
#' }
#'
