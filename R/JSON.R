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






json_to_tfrmt <- function(){
  # foo <- base_ts$col_plan$dots[1] %>%
  #   rlang::parse_quo(env = rlang::current_env())
  # tibble(col1 = letters[1:10], col2 = letters [11:20]) %>%
  #   select(!!foo)
}

