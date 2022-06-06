
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

check_span_structure_dots <- function(x, envir = parent.frame()){
  lapply(x,function(x){
    if(is.name(x)){
      return(quo(!!x))
    }else if(is.call(x)){
      if(is_valid_tidyselect_call(x)){
        quo(!!x)
      }else if(is_valid_span_structure_call(x) | is_valid_quo_call(x)){
        return(rlang::eval_tidy(x))
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
  span_col_select_function <- get(paste0("eval_tidyselect_on_colvec.",class(x)[1]),envir = asNamespace("tlang"))
  span_col_select_function(x, column_vec = column_vec)
}

#' @importFrom tidyselect eval_select
#' @importFrom rlang !!!
#' @importFrom dplyr expr
eval_tidyselect_on_colvec.quosures <- function(x, column_vec){

  data <- data.frame(
    matrix(data = rep(0, length(column_vec)), nrow = 1,
           dimnames = list(
             NULL, column_vec
           )),
    check.names = FALSE
  )[-1,]

  names(eval_select(expr(c(!!!x)), data = data))
}

#' @importFrom tidyselect eval_select
#' @importFrom rlang !!
#' @importFrom dplyr expr
eval_tidyselect_on_colvec.quosure <- function(x, column_vec){

  data <- data.frame(
    matrix(data = rep(0, length(column_vec)), nrow = 1,
           dimnames = list(
             NULL, column_vec
           )),
    check.names = FALSE
  )[-1,]

  names(eval_select(expr(c(!!x)), data = data))
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
  get_span_structure_dots_function <- get(paste0("get_span_structure_dots.",class(x)[1]),envir = asNamespace("tlang"))
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


## -----------------------------------------------
## check in tfrmt that the column and col_plan
## are compatable
##-----------------------------------------------
check_column_and_col_plan <- function(x){
  multi_column_defined <- length(x$column) > 1
  span_structures_defined <- if(!is.null(x$col_plan)){
    !is.null(x$col_plan$span_structures)
  }else{
    FALSE
  }

  if(multi_column_defined & span_structures_defined){
    stop(
      "Multiple columns defined in `column` argument of tfrmt ",
      "as well as span_structures in `col_plan`.\n",
      "The use of only one approach is permitted. ",
      "Select a single column or remove span_structures from `col_plan()`"
    )
  }

}

#' @importFrom tidyr unite
#' @importFrom dplyr as_tibble relocate last_col right_join
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
          map_dfr(span_struct_to_df, tpm_data$.original_col)

        rename_tpm <- tpm_data %>%
          filter(!(.data$.original_col %in% span_struct_cols$.original_col)) %>%
          left_join(
            tibble(
              .original_col = tfrmt$col_plan$dots %>% map_chr(as_label),
              .rename_col = names(tfrmt$col_plan$dots)
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
          .original_col = tfrmt$col_plan$dots %>% map_chr(as_label),
          .rename_col = names(tfrmt$col_plan$dots)
          )  %>%
          left_join(
            df_col_names,
            by = c(".original_col" = names(df_col_names)[ncol(df_col_names)])
          )
      }

      new_name_df <- col_name_df %>%
        mutate(
          .rename_col = case_when(
            .rename_col != "" ~ .rename_col,
            TRUE ~ .original_col
          )
        ) %>%
        relocate(.data$.original_col, .after = last_col()) %>%
        relocate(.data$.rename_col, .after = last_col()) %>%
        unite("new_name_in_df", -.rename_col , sep = .tlang_delim, remove = FALSE, na.rm = TRUE) %>%
        unite("new_name_in_df_output", c(-.original_col, -new_name_in_df), sep = .tlang_delim, remove = FALSE, na.rm = TRUE) %>%
        mutate(new_name_quo = map(.data$new_name_in_df, sym)) %>%
        mutate(
          new_name_in_df = case_when(
            new_name_in_df == .original_col ~ NA_character_,
            TRUE ~ new_name_in_df
          )
        )


      new_dots_tmp <- tibble(
          dots = tfrmt$col_plan$dots,
          dot_chr = map_chr(tfrmt$col_plan$dots, as_label),
          dot_names = names(tfrmt$col_plan$dots)
        ) %>%
        left_join(new_name_df, by =c("dot_chr"=".original_col")) %>%
        mutate(
          dot2 = ifelse(!is.na(.data$new_name_in_df), .data$new_name_quo, .data$dots),
          dot2_names = purrr::map2_chr(.data$new_name_in_df_output, .data$dot_chr , function(x, y){
            if(!identical(x, y)){
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

    out <- select( data, !!!new_dots)
  }

  out
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
  span_struct_to_df_function <- get(paste0("span_struct_to_df.",class(span_struct)[1]),envir = asNamespace("tlang"))
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












