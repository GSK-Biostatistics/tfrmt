
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
## create list of tab_spanners based on span_structures
## ---------------------------------------
create_span_group <- function(x, data){
  create_span_group_function <- get(paste0("create_span_group.",class(x)[1]),envir = asNamespace("tlang"))
  create_span_group_function(x, data)
}

#' @importFrom gt tab_spanner cols_move
create_span_group.span_structure <- function(x, data){

  cols <- span_col_select(x, data = data)
  label <- format(x$label)

  list(function(gt_tab){
    tab_spanner(gt_tab,label = label, columns = cols, gather = FALSE)
  });
}

create_span_group.span_structures <- function(x, data){

  ## for child span_structures, create tab_spanner funcs
  child_span_structures <- x$span_cols[sapply(x$span_cols, is_span_structure)]

  ## do.call('c') quickly concatenates list into vector.
  span_structure_span_func <- do.call('c',lapply(child_span_structures, create_span_group, data = data))

  ## for parent span_structure, create tab_spanner funcs
  span_structures_span_func <- create_span_group.span_structure(x, data)

  ## combine together
  c(
    span_structure_span_func,
    span_structures_span_func
  )
}


## ---------------------------------------
## determine which columns to span across
## ---------------------------------------
span_col_select <- function(x, data){
  span_col_select_function <- get(paste0("span_col_select.",class(x)[1]),envir = asNamespace("tlang"))
  span_col_select_function(x, data = data)
}

#' @importFrom tidyselect eval_select
#' @importFrom dplyr expr
span_col_select.quosures <- function(x, data){
  names(eval_select(expr(c(!!!x)), data = data))
}

span_col_select.quosure <- function(x, data){
  names(eval_select(expr(c(!!x)), data = data))
}

span_col_select.span_structure <- function(x, data){
  do.call('c',lapply(x$span_cols, span_col_select, data = data))
}

span_col_select.span_structures <- function(x, data){
  do.call('c',lapply(x$span_cols, span_col_select, data = data))
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
#' @importFrom dplyr as_tibble relocate last_col
select_col_plan <- function(data, tfrmt){

  if (is.null(tfrmt$col_plan)){
    if(!is.null(tfrmt$row_grp_plan$label_loc$location)&&
       tfrmt$row_grp_plan$label_loc$location=="noprint"){

      out <- data %>% select(-c(!!!tfrmt$group))

    } else {
      out <- data
    }
  } else {
    #make a dummy dataset based on the last section of the column
    if(length(tfrmt$col_plan$span_structures) > 0){
      tpm_data <- names(data) %>%
        str_split(.tlang_delim) %>%
        map_chr(last) %>%
        as_tibble() %>%
        mutate(val = 0) %>%
        pivot_wider(names_from = .data$value,
                    values_from = .data$val) %>%
        slice(0)

      # Get the new names
      new_name_df <- tfrmt$col_plan$span_structures %>%
        map_dfr(span_struct_to_df, tpm_data) %>%
        relocate(.data$.original_col, .after = last_col()) %>%
        unite("new_name", everything(), sep = .tlang_delim, remove = FALSE) %>%
        mutate(new_name_quo = map(.data$new_name, sym))

      new_dots <- tibble(dots = tfrmt$col_plan$dots,
                         chr_dots = map_chr(tfrmt$col_plan$dots, as_label)) %>%
        left_join(new_name_df, by =c("chr_dots"=".original_col")) %>%
        mutate(dot2 = ifelse(!is.na(.data$new_name), .data$new_name_quo, .data$dots))%>%
        pull(.data$dot2)
    } else if(length(tfrmt$column) > 1){
      kernal_name <- names(data) %>%
        str_split(.tlang_delim) %>%
        map_chr(last)
      # if(length(unique(kernal_name)) != length(kernal_name)){
      #   stop("Unable to select columns due to duplicate naming.
      #        If you would like to set the order consider using span structures rather than multiple columns.")
      # }

      dots_df <-tibble(dots = tfrmt$col_plan$dots,
                       dots_chr = tfrmt$col_plan$dots %>% map_chr(as_label))

      new_dots <- tibble(dat_nm = names(data),
                         kernal_name = kernal_name,
                         new_name = map(.data$dat_nm, sym)
      ) %>%
        left_join(dots_df, ., by = c("dots_chr" = "kernal_name")) %>%
        mutate(
          dot2 = ifelse(!is.na(.data$dat_nm), .data$new_name, .data$dots))%>%
        pull(.data$dot2)

    } else {
      new_dots <- tfrmt$col_plan$dots
    }

    #Adding in labels and grouping if people forgot it
    # because the order of these are set by the GT I don't it will matter
  #  new_dots <- c(tfrmt$label, tfrmt$group, new_dots)

    if((!is.null(tfrmt$row_grp_plan) &&
        !is.null(tfrmt$row_grp_plan$label_loc)&&
        tfrmt$row_grp_plan$label_loc$location=="noprint")){

      new_dots <- setdiff(new_dots, tfrmt$group)

    }

    out<- select(
      data,
      !!!new_dots
    )
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
  tmp_df <- x %>%
    select(!!(tfrmt_obj$column[[1]])) %>%
    mutate(val = 0) %>%
    quietly(pivot_wider)(names_from = !!(tfrmt_obj$column[[1]]),
                         values_from = .data$val
    ) %>%
    .$result %>%
    slice(0)

  ## create df of span structures
  span_struct_df <- tfrmt_obj$col_plan$span_structures %>%
    map_dfr(span_struct_to_df, tmp_df) %>%
    relocate(.data$.original_col, .after = last_col()) %>%
    rename(!!as_label(tfrmt_obj$column[[1]]) := ".original_col")

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

span_struct_to_df.span_structure <- function(span_struct, data_col, depth = 1){
  lab <- span_struct$label
  contents <- span_col_select(
    span_struct, data_col
  )

  tibble(
    lab = lab,
    .original_col = contents
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
        contents <- span_col_select(x, data_col)
        tibble(.original_col = contents)
      }
    }) %>%
    mutate(lab = lab) %>%
    rename(!!paste0(.tlang_struct_col_prefix, depth) := lab) %>%
    relocate(!!paste0(.tlang_struct_col_prefix, depth), .before = 1)

}












