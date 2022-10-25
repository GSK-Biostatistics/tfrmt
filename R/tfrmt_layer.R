#' Layer tfrmt objects together
#'
#' Provide utility for layering tfrmt objects together. If both tfrmt's have
#' values, it will preferentially choose the second tfrmt by default. This is an
#' alternative to piping together tfrmt's
#'
#' @param x,y tfrmt objects that need to be combined
#' @param ... arguments passed to layer_tfrmt_arg functions for combining different tfrmt elements
#' @param join_body_plans should the `body_plans` be combined, or just keep styling in y. See details: join_body_plans for more details.
#'
#' @details
#'
#' ## join_body_plan
#'
#' When combining two body_plans, the body plans will stack together, first the
#' body plan from x tfrmt then y tfrmt. This means that frmt_structures in y
#' will take priority over those in x.
#'
#' Combining two tfrmt with large body_plans can lead to slow table evaluation.
#' Consider setting `join_body_plan` to `FALSE`. Only the y `body_plan` will be
#' preserved.
#'
#' @returns tfrmt object
#' @export
#' @examples
#'
#' tfrmt_1 <- tfrmt(title = "title1")
#'
#' tfrmt_2 <- tfrmt(title = "title2",subtitle = "subtitle2")
#'
#' layered_table_format <- layer_tfrmt(tfrmt_1, tfrmt_2)
#'
layer_tfrmt <- function(x, y, ..., join_body_plans = TRUE){

  if(missing(x)){
    stopifnot(is_tfrmt(y))
    return(y)
  }else if(missing(y)){
    stopifnot(is_tfrmt(x))
    return(x)
  }

  stopifnot(is_tfrmt(y))
  stopifnot(is_tfrmt(x))

  args <- union(names(x), names(y))

  arg_list <- lapply(args, function(argname, x, y, ..., join_body_plans){
    func <- get_layer_tfrmt_arg_method(argname)
    func(x, y, argname, ..., join_body_plans = join_body_plans)
  },x=x, y = y, ..., join_body_plans = join_body_plans)

  names(arg_list) <- args

  ## remove null values that may have made it through
  arg_list <- arg_list[!sapply(arg_list, is.null)]

  tfrmt_call <- as.call(c(as.name("tfrmt"), arg_list))

  tryCatch(
    eval(tfrmt_call),
    error = function(e){
      if(inherits(e, "_tfrmt_invalid_body_plan")){
        e <- append_update_group_message(e, x, y)
      }
      abort(
        e$message,
        call = e$call,
        trace = e$trace
      )
    }
  )

}

get_layer_tfrmt_arg_method <- function(argname){
  tryCatch(
    get(paste0("layer_tfrmt_arg.",argname),envir = asNamespace("tfrmt"), inherits = FALSE),
    error = function(e){ layer_tfrmt_arg.default}
  )
}

layer_tfrmt_arg.default<- function(x, y, arg_name, ...){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]

  if(is.null(y_arg_val)){
    x_arg_val
  }else{
    y_arg_val
  }
}

## if group is an empty vars, keep the original value
layer_tfrmt_arg_vars<- function(x, y, arg_name, ...){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]

  if(is.null(y_arg_val) | identical(y_arg_val, vars())){
    x_arg_val
  }else{
    y_arg_val
  }
}

## if label/param/value/column is an empty quo, keep the original value
layer_tfrmt_arg_quo<- function(x, y, arg_name, ...){
  x_arg_val <- x[[arg_name]]
  y_arg_val <- y[[arg_name]]

  if(is.null(y_arg_val) | identical(y_arg_val, quo())){
    x_arg_val
  }else{
    y_arg_val
  }
}

layer_tfrmt_arg.group <- layer_tfrmt_arg_vars
layer_tfrmt_arg.label <- layer_tfrmt_arg_quo
layer_tfrmt_arg.param <- layer_tfrmt_arg_quo
layer_tfrmt_arg.value <- layer_tfrmt_arg_quo
layer_tfrmt_arg.column <- layer_tfrmt_arg_vars
layer_tfrmt_arg.sorting_cols <- layer_tfrmt_arg_vars


layer_tfrmt_arg.body_plan <- function(x, y, ...,  join_body_plans = TRUE){
  x_body_plan <- x[["body_plan"]]
  y_body_plan <- y[["body_plan"]]

  if(join_body_plans){
    body_plan_el <- unique(c(x_body_plan, y_body_plan))
  }else{
    body_plan_el <- y_body_plan
  }

  do.call(body_plan,body_plan_el)
}



#' Remap group values in a tfrmt
#'
#' @param tfrmt a `tfrmt`
#' @param ... Use new_name = old_name to rename selected variables
#'
#' @return
#' A `tfrmt` with the `group` variables updated in all places
#'
#' @importFrom rlang as_label is_empty
#'
#' @returns tfrmt object with updated groups#'
#' @export
#' @examples
#'
#' tfrmt_spec <- tfrmt(
#'     group = c(group1, group2),
#'     body_plan  = body_plan(
#'       frmt_structure(
#'          group_val = list(group2 = "value"),
#'          label_val = ".default",
#'          frmt("XXX")
#'          ),
#'      frmt_structure(
#'          group_val = list(group1 = "value", group2 = "value"),
#'          label_val = ".default",
#'          frmt("XXX")
#'        )
#'     ))
#'
#' tfrmt_spec %>%
#'   update_group(New_Group = group1)
#'
update_group <- function(tfrmt, ...){

  dots <- as.list(substitute(substitute(...)))[-1]

  old_groups <- do.call(vars, unname(dots))
  new_group_map <- setNames(names(dots), map_chr(old_groups, as_label))

  if(!is_empty(tfrmt$group)){

    var_list <- sapply(tfrmt$group, function(x){
      x_lab <- as_label(x)
      if(x_lab %in% names(new_group_map)){
        new_group_map[[x_lab]]
      }else{
        x_lab
      }
    })

    tfrmt$group <- as_vars(var_list)

  }else{
    stop("No group values defined in input tfrmt.")
  }

  if(!is.null(tfrmt$body_plan)){
    bp_list <- lapply(tfrmt$body_plan, function(frmt_struct){
      if(is.list(frmt_struct$group_val)){
        struct_groups <- names(frmt_struct$group_val)
        for(struct_group_idx in seq_along(struct_groups)){
          if(struct_groups[struct_group_idx] %in% names(new_group_map)){
            names(frmt_struct$group_val)[struct_group_idx] <- new_group_map[struct_groups[struct_group_idx]]
          }
        }
      }
      frmt_struct
    })

    tfrmt$body_plan <- do.call("body_plan", bp_list)

  }

  check_group_var_consistency(tfrmt)

  tfrmt

}


append_update_group_message <- function(e, x, y){

  x_grp <- map_chr(x$group, as_label)
  y_grp <- map_chr(y$group, as_label)

  update_grp_message <- c(i = paste0(
    "You might need to update group names using ",
    "\"update_group(",
    paste0("`",y_grp,"` = `", x_grp,"`", collapse = ","),
    ")\""))

  e$message <- c(e$message, "", update_grp_message)
  e
}
