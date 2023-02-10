#' Print to JSON
#'
#' @param tfrmt tfrmt to print
#' @param path file path to save JSON to. If not provided the JSON will just print to the console
#'
#' @return JSON
#' @export
#'
#' @importFrom jsonlite toJSON validate
tfrmt_to_json <- function(tfrmt, path = NULL){
  if(!is_tfrmt(tfrmt)){
    stop("Needs tfrmt")
  }
  output <- as_json(tfrmt)

  if(!is.null(path)){
    message(paste0("Writing json file out to:\n", path))
    write(output, path)
  } else {
    return(output)
  }

}


#' Convert tfrmt elements
#'
#' This collection of functions is used to create a json or json ready objects.
#' While primarily inteded for internal use these functions can be used externally
#'
#' @param x tfrmt or tfrmt element
#'
#' @return as_json.tfrmt() will return a json object all other methods will
#'   return a list that is cleaned so it can be directly converted to a json
#'   object
#'
#' @keywords internal
#' @export
as_json <- function(x){
  UseMethod("as_json", x)
}

#' @export
#' @importFrom jsonlite toJSON validate
#' @importFrom stringr str_replace_all
as_json.tfrmt <- function(x){
  # Prepare each element to get converted to JSON
  tfrmt_nm <- names(x)

  out <- vector("list", length = length(x))
  for(i in 1:length(x)){
    out[[i]] <- x[[i]] %>%
      as_json()
  }
  names(out) <- tfrmt_nm

  # Converts list to a json object
  output_json <- out %>%
    toJSON(pretty = TRUE, force = TRUE)

  # Removing names added in by jsonlite
  json_clean <- output_json %>% str_replace_all('\"\\s(\\.\\d+)?\"',  '\"\"')
  if(validate(json_clean)){
    class(json_clean) <- "json"
  } else {
    stop("Error when creating the json object")
  }
  json_clean
}

#' @export
as_json.default <- function(x){
  x
}

#' @export
as_json.quosures <- function(x){
  x %>%
    map_chr(.,as_label)
}


#' @export
as_json.quosure <- function(x){
  out <- x %>%
    as_label()
  if(out != "<empty>") {
    return(out)
  }
}

#' @export
as_json.body_plan <- function(x){
  x %>%
    map(as_json)
}

#' @export
as_json.frmt_structure <- function(x){
  x$frmt_to_apply <- x$frmt_to_apply %>%
    map(as_json)

  list(group_val = x$group_val,
       label_val = x$label_val,
       param_val = x$param_val) %>%
    c(x$frmt_to_apply[[1]][1])
}

#' @export
as_json.frmt <- function(x){
  list(frmt = x)
}

#' @export
as_json.frmt_when <- function(x){
  lhs <- map_chr(x$frmt_ls, f_lhs)
  rhs <- map(x$frmt_ls, f_rhs) %>% map(as_json)
  names(rhs) <- lhs
  list(frmt_when = list(frmt_ls = rhs, missing = x$missing))
}

#' @export
as_json.frmt_combine <- function(x){
  frmts <- x$frmt_ls %>%
    map(as_json)
  list(frmt_combine = list(expression = x$expression,
                           frmt_ls = frmts, missing = x$missing))
}

#' @export
as_json.col_plan <- function(x){
  if(is.null(x)){
    c()
  } else {
    dot_ls <- x$dots %>%
      map(as_json)

    if(!is.null(names(dot_ls))){
      names(dot_ls) <- names(x$dots) %>% ifelse(.=="", " ", .)
    }
    list(col_plan = list(dots = dot_ls, .drop = x$.drop))
  }
}

#' @export
as_json.col_styel_plan <- function(x){
  x %>%
    map(as_json)
}

#' @export
as_json.col_style_plan <- function(x){
  x %>%
    map(as_json)
}

#' @export
as_json.col_style_structure <- function(x){
  x$cols <- x$cols %>%
    map(as_json)
  x
}





#' json to tfrmt
#'
#' @param path
#'
#' @param json by default this is null, if
#'
#' @importFrom jsonlite read_json parse_json
json_to_tfrmt <- function(path = NULL, json = NULL){
  if(!is.null(json)){
    dirty_list <- parse_json(json)
  } else if(!is.null(path)){
    dirty_list <- read_json(path)
  } else {
    stop("Path or json object needed")
  }

  json_nm <- names(dirty_list)
  simple_vars <- c("group", "label", "param", "value", "column",
                   "title", "subtitle", "sorting_cols")

  simple_params <- dirty_list[intersect(simple_vars, json_nm)] %>%
    map(unlist)

  rgp <- ls_to_row_grp_plan(dirty_list$row_grp_plan)
  bp <- ls_to_body_plan(dirty_list$body_plan)
  # ls_to_col_style_plan(dirty_list$col_style_plan)
  # ls_to_col_plan(dirty_list$col_plan)
  # ls_to_big_n(dirty_list$big_n)
  # ls_to_footnote_plan(dirty_list$footnote_plan)

  all_params <- c(simple_params,
                  list(row_grp_plan = rgp),
                  list(body_plan = bp)) %>%
    discard(is.null)
  do.call(tfrmt, all_params)
}

ls_to_row_grp_plan <- function(ls){
  if(!is.null(ls)){
    struct_ls <- ls$struct_ls %>%
      map(function(struct){
        el_block <- do.call(element_block, struct$block_to_apply %>% map(unlist))
        if(!is.null(names(struct$group_val))){
          group_val = map(struct$group_val, unlist)
        } else {
          group_val = unlist(struct$group_val)
        }
        do.call(row_grp_structure, list(group_val = group_val, element_block = el_block))
      })

    label_loc <- do.call(element_row_grp_loc, ls$label_loc %>% map(unlist))
    ls <- do.call(row_grp_plan, c(struct_ls, list(label_loc = label_loc)))
  }
  ls
}

#' @importFrom stringr str_which
ls_to_body_plan <- function(ls){
  if(!is.null(ls)){
    frmts_ls <- ls %>%
      map(function(struct){
        frmt_loc <- str_which(names(struct), "frmt*")
        type <- names(struct)[frmt_loc]
        frmt_val <- do.call(paste0("ls_to_", type), list(x = struct[[type]])) %>%
          list()
        group_val = simplify_group_val(struct$group_val)
        arg_list <- c(list(group_val = group_val),
                      list(label_val= unlist(struct$label_val)),
                      frmt_val)
        out_struct <- do.call(frmt_structure, arg_list)
        out_struct$param_val <- unlist(struct$param_val)
        out_struct
      })

    do.call(body_plan, frmts_ls)
  }
}

ls_to_frmt <- function(x){
  x <- x %>%
    map(unlist)
  do.call(frmt, list(expression = x$expression, missing = x$missing, scientific = x$scientific))
}

ls_to_frmt_combine <- function(x){
  fmts <- x$frmt_ls %>%
    map(function(a_frmt){
      do.call(paste0("ls_to_", names(a_frmt)), list(x = a_frmt[[1]]))
    })
  x <- x %>%
    map(unlist)
  do.call(frmt_combine, c(list(expression = x$expression),
                          fmts, list(missing = x$missing)))
}

ls_to_frmt_when <- function(x){
  fmts <- x$frmt_ls %>%
    map(function(a_frmt){
      do.call(paste0("ls_to_", names(a_frmt)), list(x = a_frmt[[1]])) %>%
        as.character()
    })

  formula_ls <- str_c("'", names(fmts), "' ~ ", fmts) %>%
    map(as.formula)

  do.call(frmt_when, c(formula_ls, list(missing = x$missing)))
}

ls_to_col_plan <- function(ls){
  browser()
  dots <- ls$col_plan$dots %>%
    unlist() %>%
    map(~rlang::parse_quo(.x, env = rlang::current_env())) %>%
    unlist()
  do.call(col_plan, as.list(dots))
  # .drop = unlist(ls$col_plan$.drop))

}

simplify_group_val <- function(group_ls){
  if(!is.null(names(group_ls))){
    group_val = map(group_ls, unlist)
  } else {
    group_val = unlist(group_ls)
  }
  group_val
}
