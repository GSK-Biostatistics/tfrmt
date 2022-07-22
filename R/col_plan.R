#' Define the Column Plan & Span Structures
#'
#' Using <[`tidy-select`][dplyr_tidy_select]> expressions and a series
#' span_structures, define the spanned column names, and the label to apply.
#' span_structures can be nested to allow for layered spanning headers.
#'
#' @details
#'
#' ## Column Selection
#'
#' When col_plan gets applied and is used to create the output table, the
#' underlying logic becomes the input to \code{\link[dplyr]{select}}. Therefore,
#' behavior falls to the \code{\link[dplyr]{select}} for sub-setting columns, renaming,
#' and reordering the columns.
#'
#' Avoid beginning the \code{col_plan()} column selection with a deselection (ie
#' \code{col_plan(-col1)}, \code{col_plan(-starts_with("value")))}. This will
#' result in the table preserving all columns not "de-selected" in the
#' statement, and the order of the columns not changed. It is preferred when
#' creating the \code{col_plan()} to identify all the columns planned on
#' preserving in the order they are wished to appear, or if
#' <[`tidy-select`][dplyr_tidy_select]> arguments - such as
#' \code{\link[dplyr]{everything}}- are used, identify the de-selection after
#' the positive-selection. Experiment with the \code{\link[dplyr]{select}}
#' function to understand this sort of behavior better.
#'
#' Alternatively, once the gt table is produced, use the \code{\link[gt]{cols_hide}}
#' function to remove un-wanted columns.
#'
#'
#' @rdname col_plan
#'
#' @param ... For a col_plan and span_structure,
#'   <[`tidy-select`][dplyr_tidy_select]> arguments, unquoted expressions
#'   separated by commas, and span_structures. Span_structures can nest
#'   additional span_structures. To use a span_structure, there can only be one
#'   defined "column" in the tfrmt.
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' ## select col_1 as the first column, remove col_last, then create spanning
#' ## structures that have multiple levels
#' spanning_col_plan_ex <- col_plan(
#'  col_1,
#'  -col_last,
#'  span_structure(
#'    label = "Top Label Level 1",
#'    span_structure(
#'      label = "Second Label Level 1.1",
#'      col_3, col_4
#'    ),
#'    span_structure(
#'      label = "Second Label Level 1.2",
#'      starts_with("B")
#'    ),
#'    col_5
#'  ),
#'  span_structure(
#'    label = "Top Label Level 2",
#'    col_6, col_7
#'  )
#' )
#'
#' ## select my_col_1 as the first column, then
#' ## rename col_2 to new_col_1 and put as the
#' ## second column, then select the rest of the columns
#' renaming_col_plan_ex <- col_plan(
#'    my_col_1,
#'    new_col_1 = col_2,
#'    everything()
#'    )
#'
#' @section Images:
#' Here are some example outputs:
#'
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-span_structure.jpg\" style=\"width:100\\%;\">"`
#' }}
#'
col_plan <- function(...){
  ## selectively evaluate dots (only if is a span_structure)
  ## confirm contents otherwise
  dots <- as.list(substitute(substitute(...)))[-1]
  dots <- check_col_plan_dots(dots)
  #Add the new spanning columns to the dots.

  ## get columns of the span structures
  span_struct_entries_locs <- sapply(dots, is_span_structure)
  if(any(span_struct_entries_locs)){
    span_struct_entries <- dots[span_struct_entries_locs]
    span_struct_dots <- lapply(span_struct_entries, get_span_structure_dots)

    ## flatten dots
    dots[span_struct_entries_locs] <- span_struct_dots
    dots <- unlist(dots)
  }else{
    span_struct_entries <- NULL
  }

  ## convert dots into a vars list (list of quosures)
  dots_as_vars <- do.call(vars, dots)

  ##TODO: check for duplicate variable calls?

  structure(
    list(
      dots = dots_as_vars,
      span_structures = span_struct_entries
    ),
    class = c("col_plan","plan")
  )
}

#' @rdname col_plan
#'
#' @param label text label to span across the defined columns
#'
#' @export
span_structure <- function(label, ...){

  if(!(is.character(label))){
    stop("`label` must be a character vector")
  }

  span_cols <- as.list(substitute(substitute(...)))[-1]
  span_cols <- check_span_structure_dots(span_cols)

  any_dots_span_structure <- any(sapply(span_cols, is_span_structure))

  structure(
    list(
      label = label,
      span_cols = span_cols
    ),
    class = c("span_structures"[any_dots_span_structure],"span_structure")
  )
}

is_span_structure <- function(x){
  inherits(x, "span_structure")
}

is_span_structures <- function(x){
  inherits(x, "span_structures")
}

#' @importFrom rlang eval_tidy
check_span_structure_dots <- function(x, envir = parent.frame()){
  x_dots <- lapply(x,function(x){
    if(is.name(x)){
      if(identical(as_label(x), "<empty>")){
        return(NULL)
      }else{
        return(quo(!!x))
      }
    }else if(is.call(x)){
      if(is_valid_tidyselect_call(x)){
        quo(!!x)
      }else if(is_valid_span_structure_call(x) | is_valid_quo_call(x)){
        return(eval_tidy(x))
      }else{
        stop(
          "Invalid entry: `",format(x),"`\n",
          "Only span_structures (`span_structure()`), ",
          "selection helpers (See <https://tidyselect.r-lib.org/reference>), ",
          " or unquoted expressions representing variable names ",
          " can be entered as contents.",
          " Changing the names of individual variables using new_name = old_name syntax is allowable",
          call. = FALSE
        )
      }
    }else if(is_span_structure(x)){
      return(x)
    }else if(is.character(x)){
      return(as_length_one_quo.character(x))
    }else{
      stop("Unexpected entry type")
    }
  })

  x_dots[!sapply(x_dots, is.null)]
}

is_valid_span_structure_call <- function(x){
  as.character(as.list(x)[[1]]) %in% c("span_structure")
}

is_valid_tidyselect_call <- function(x){
  ## drop - from determining if
  if(as.character(as.list(x)[[1]]) == "-"){
    x <- x[[-1]]
    if(is.name(x)){
      return(TRUE)
    }
  }
  as.character(as.list(x)[[1]]) %in% c("starts_with","ends_with","contains","matches","num_range","all_of","any_of","everything","last_col", "where")
}

is_valid_quo_call <- function(x){
  ## drop - from determining if
  if(as.character(as.list(x)[[1]]) == "-"){
    x <- x[[-1]]
    if(is.name(x)){
      return(TRUE)
    }
  }
  as.character(as.list(x)[[1]]) %in% c("vars","quo")
}

check_col_plan_dots <- check_span_structure_dots


## ---------------------------------------
## determine which columns to span across
## ---------------------------------------
eval_tidyselect_on_colvec <- function(x, column_vec){
  span_col_select_function <- get(paste0("eval_tidyselect_on_colvec.",class(x)[1]),envir = asNamespace("tfrmt"))
  span_col_select_function(x, column_vec = column_vec)
}

#' @importFrom tidyselect eval_select
#' @importFrom rlang !!!
#' @importFrom dplyr expr
eval_tidyselect_on_colvec.quosures <- function(x, column_vec){

  names(column_vec) <- column_vec

  names(eval_select(expr(c(!!!x)), data = column_vec))
}

#' @importFrom tidyselect eval_select
#' @importFrom rlang !!
#' @importFrom dplyr expr
eval_tidyselect_on_colvec.quosure <- function(x, column_vec){

  names(column_vec) <- column_vec


  names(eval_select(expr(c(!!x)), data = column_vec))
}

eval_tidyselect_on_colvec.span_structure <- function(x, column_vec){
  do.call('c',lapply(x$span_cols, eval_tidyselect_on_colvec, column_vec = column_vec))
}

eval_tidyselect_on_colvec.span_structures <- function(x, column_vec){
  do.call('c',lapply(x$span_cols, eval_tidyselect_on_colvec, column_vec = column_vec))
}

## -----------------------------------------------
## get the span cols entries from a span structure
## We need to keep this
## -----------------------------------------------
get_span_structure_dots <- function(x){
  get_span_structure_dots_function <- get(paste0("get_span_structure_dots.",class(x)[1]),envir = asNamespace("tfrmt"))
  get_span_structure_dots_function(x)
}

get_span_structure_dots.quosure <- function(x){
  x
}

get_span_structure_dots.quosures <- get_span_structure_dots.quosure

get_span_structure_dots.span_structure <- function(x){
  x$span_cols %>% unlist()
}

get_span_structure_dots.span_structures <- function(x){
  do.call('c',lapply(x$span_cols, get_span_structure_dots))
}




#' @importFrom tidyr unite
#' @importFrom dplyr as_tibble relocate last_col right_join
#' @importFrom stringr str_remove str_detect
#' @importFrom purrr pmap_chr map2
#' @importFrom utils capture.output
#' @importFrom rlang quo
select_col_plan <- function(data, tfrmt){

  if (is.null(tfrmt$col_plan)){
    if(!is.null(tfrmt$row_grp_plan$label_loc$location)&&
       tfrmt$row_grp_plan$label_loc$location=="noprint"){

      out <- data %>% select(-c(!!!tfrmt$group))

    } else {
      out <- data
    }
  } else {

    ## triple dots passed to select
    new_dots <- tfrmt$col_plan$dots

    ### if there are span_structures or multiple columns, modify new_dots
    if(length(tfrmt$col_plan$span_structures) > 0 | length(tfrmt$column) > 1){

      #make a dummy dataset based on the last section of the column
      if(length(tfrmt$col_plan$span_structures) > 0){

        total_split_cols <- names(data) %>% str_split(.tlang_delim) %>% lengths() %>% max() %>% `-`(1)

        new_cols <- c(paste0(.tlang_struct_col_prefix, seq_len(total_split_cols)), ".original_col")

        tpm_data <- tibble(.original_col = names(data)) %>%
          separate(.data$.original_col, into = new_cols, sep = .tlang_delim, fill = "left", remove = TRUE)

        span_struct_cols <- tfrmt$col_plan$span_structures %>%
          map_dfr(span_struct_to_df, tpm_data$.original_col) %>%
          mutate(
            .removal_identifier_col = .data$.original_col %>% str_detect("^-")
          )

        rename_tpm <- tpm_data %>%
          filter(!(.data$.original_col %in% span_struct_cols$.original_col)) %>%
          left_join(
            tibble(
              .original_col = tfrmt$col_plan$dots %>% map_chr(as_label) %>% str_remove("^-"),
              .rename_col = names(tfrmt$col_plan$dots),
              .removal_identifier_col = tfrmt$col_plan$dots %>% map_chr(as_label) %>% str_detect("^-")
            ),
            by = ".original_col"
          )


        # Get the new names
        col_name_df <-  span_struct_cols %>%
          bind_rows(
            rename_tpm
          )


      } else if(length(tfrmt$column) > 1){

        df_col_names <- tibble(df_names = names(data)) %>%
          separate(df_names, map_chr(tfrmt$column, as_label), sep = .tlang_delim, fill = "left")

        col_name_df <- tibble(
          .original_col = tfrmt$col_plan$dots %>% map_chr(as_label) %>% str_remove("^-"),
          .rename_col = names(tfrmt$col_plan$dots),
          .removal_identifier_col = tfrmt$col_plan$dots %>% map_chr(as_label) %>% str_detect("^-")
        )  %>%
          left_join(
            df_col_names,
            by = c(".original_col" = names(df_col_names)[ncol(df_col_names)])
          )

      }

      n_layers <- length(setdiff(names(col_name_df),c(".original_col",".rename_col",".removal_identifier_col")))

      new_name_df <- col_name_df %>%
        mutate(
          .rename_col = case_when(
            .rename_col != "" ~ .rename_col,
            TRUE ~ .original_col
          )
        ) %>%
        relocate(.data$.original_col, .after = last_col()) %>%
        relocate(.data$.rename_col, .after = last_col()) %>%
        unite("new_name_in_df", c(-.data$.rename_col, -.data$.removal_identifier_col) , sep = .tlang_delim, remove = FALSE) %>%
        unite("new_name_in_df_output", c(-.data$.original_col, -.data$new_name_in_df, -.data$.removal_identifier_col), sep = .tlang_delim, remove = FALSE) %>%
        mutate(
          new_name_in_df = remove_empty_layers(.data$new_name_in_df, n_layers),
          new_name_in_df_output = remove_empty_layers(.data$new_name_in_df_output, n_layers)
        )

      new_dots_tmp <- tibble(
        dots = tfrmt$col_plan$dots,
        dot_chr = map_chr(tfrmt$col_plan$dots, as_label) %>% str_remove("^-"),
        dot_names = names(tfrmt$col_plan$dots),
        dot_removal = tfrmt$col_plan$dots %>% map_chr(as_label) %>% str_detect("^-")
      ) %>%
        left_join(new_name_df, by =c("dot_chr"=".original_col")) %>%
        mutate(
          new_name_quo = map2(.data$new_name_in_df, .data$dot_removal, dot_char_as_quo),
          new_name_in_df_output = case_when(
            is.na(.data$new_name_in_df_output) ~ "",
            TRUE ~ .data$new_name_in_df_output
          )
        ) %>%
        mutate(
          dot2 = ifelse(!is.na(.data$new_name_in_df), .data$new_name_quo, .data$dots),
          dot2_names = pmap_chr(list(x = .data$new_name_in_df_output, y = .data$dot_chr, z = .data$dot_removal), function(x, y, z){
            if(!identical(x, y) & !z){
              x
            }else{
              ""
            }
          })
        )

      new_dots <- as.list(new_dots_tmp$dot2)
      names(new_dots) <- new_dots_tmp$dot2_names

    }

    # Adding in labels and grouping if people forgot it
    # because the order of these are set by the GT I don't it will matter
    #  new_dots <- c(tfrmt$label, tfrmt$group, new_dots)

    if((!is.null(tfrmt$row_grp_plan) &&
        !is.null(tfrmt$row_grp_plan$label_loc)&&
        tfrmt$row_grp_plan$label_loc$location=="noprint")){
      new_dots <- setdiff(new_dots, tfrmt$group)
    }

    dot_var <- do.call(vars,new_dots)

    out <- select(data, !!!dot_var)
  }

  out
}


## given a string - x - see how to convert to a quosure.
##  if negative is TRUE, it will mark it as a `-`.
dot_char_as_quo <- function(x, negative = FALSE) {


  ## if x is a valid tidyselect call, leave it as is,
  ## otherwise wrap it in "`". This is so we can pass
  ## colnames with spaces (which are common in spanned columns)
  ## to quo. IE `spanned col header__tfrmt_delim__col1`
  x_text <- tryCatch({
    x_lang <- parse(text = x)[[1]]
    if (is_valid_tidyselect_call(x_lang)) {
      x
    } else{
      paste0("`", x, "`")
    }},
    error = function(e) {
      paste0("`", x, "`")
    }
  )

  if (negative) {
    expr_to_eval <- paste0("quo(-", x_text, ")")
  } else{
    expr_to_eval <- paste0("quo(", x_text, ")")
  }

  eval(parse(text = expr_to_eval)[[1]])
}

## -----------------------------------------------
## When we have span structures in the col_plan,
## edit data to add in columns with the spanners to allow
## us to have consistent behavior across multi-column
## dfs and span_structs
##-----------------------------------------------
#' @importFrom purrr quietly
apply_span_structures_to_data <- function(tfrmt_obj, x){

  ## create temp df with columns based on
  tmp_df_name_vec <- x %>%
    pull(!!(tfrmt_obj$column[[1]])) %>%
    unique()

  ## create df of span structures
  span_struct_df <- tfrmt_obj$col_plan$span_structures %>%
    map_dfr(span_struct_to_df, tmp_df_name_vec) %>%
    relocate(.data$.original_col, .after = last_col()) %>%
    rename(!!as_label(tfrmt_obj$column[[1]]) := ".original_col") %>%
    select(-.rename_col)

  ## merge together data and span df on column arrange
  left_join(x,
            span_struct_df,
            by = as_label(tfrmt_obj$column[[1]])) %>%
    select(starts_with(.tlang_struct_col_prefix),
           !!(tfrmt_obj$column[[1]]),
           everything())
}

##----------------------------------------------------
## convert span_structure to a data.frame representation
##----------------------------------------------------

span_struct_to_df <- function(span_struct, data_col, depth = 1){
  span_struct_to_df_function <- get(paste0("span_struct_to_df.",class(span_struct)[1]),envir = asNamespace("tfrmt"))
  span_struct_to_df_function(span_struct=span_struct, data_col = data_col, depth = depth)
}

#' @importFrom rlang %||%
span_struct_to_df.span_structure <- function(span_struct, data_col, depth = 1){

  lab <- span_struct$label

  contents <- eval_tidyselect_on_colvec(span_struct, data_col)

  new_names <- names(contents) %||% contents

  tibble(
    lab = lab,
    .original_col = contents,
    .rename_col = new_names
  ) %>%
    rename(
      !!paste0(.tlang_struct_col_prefix,depth) := lab
    )
}

span_struct_to_df.span_structures <- function(span_struct, data_col, depth = 1){


  lab <- span_struct$label
  span_across <- span_struct$span_cols

  span_across %>%
    map_dfr(function(x) {
      if (is_span_structure(x)) {
        span_struct_to_df(span_struct = x, data_col, depth = depth + 1)
      } else{
        contents <- eval_tidyselect_on_colvec(x, data_col)
        tibble(.original_col = contents, .rename_col = names(x))
      }
    }) %>%
    mutate(lab = lab) %>%
    rename(!!paste0(.tlang_struct_col_prefix, depth) := lab) %>%
    relocate(!!paste0(.tlang_struct_col_prefix, depth), .before = 1)

}











