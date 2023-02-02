#' Print to JSON
#'
#' @param tfrmt tfrmt to print
#' @param path file path to save JSON to. If not provided the JSON will just print to the console
#'
#' @return JSON
#' @export
#'
#' @importFrom jsonlite toJSON validate
#' @importFrom stringr str_replace_all
tfrmt_to_json <- function(tfrmt, path = NULL){
  if(!is_tfrmt(tfrmt)){
    stop("Needs tfrmt")
  }
  tfrmt_nm <- names(tfrmt)

  #Listed quosures
  quosure_ls <- tfrmt[intersect(tfrmt_nm, c("group", "column", "sorting_cols"))] %>%
    map(~map_chr(.,as_label))
  #Single quosures
  quosure <- tfrmt[intersect(tfrmt_nm,c("label","param", "value"))] %>%
    map(as_label) %>%
    map(function(x){
      if(x != "<empty>") x
    })
  #Get body_plan
  tfrmt$body_plan %>%
    unpack_body_plan()

  # Get col_plan
  col_plan_ls <- tfrmt$col_plan %>%
    unpack_col_plan()

  other_nm <- setdiff(tfrmt_nm, c("group", "column", "sorting_cols","label","param", "value","col_plan"))
  others <- tfrmt[other_nm]

  # Convert to JSON
  output_json <- c(quosure_ls, quosure, others, col_plan_ls) %>%
    toJSON(pretty = TRUE, force = TRUE)
  # Removing names added in my jsonlite
  json_clean <- output_json %>% str_replace_all('\"\\s(\\.\\d+)?\"',  '\"\"')
  if(validate(json_clean)){
    class(json_clean) <- "json"
  } else {
    stop("Error when creating the json object")
  }
  json_clean
}

unpack_col_plan <- function(col_plan) {
  if(is.null(col_plan)){
    c()
  } else {
    dot_ls <- col_plan$dots %>%
      map(function(x){
        if(is_quosure(x)){
          out <- as_label(x)
        } else {
          out <- x %>%
            map(~map_chr(.,as_label))
        }
        out
      })
    if(!is.null(names(dot_ls))){
      names(dot_ls) <- names(col_plan$dots) %>% ifelse(.=="", " ", .)
    }
    list(col_plan = list(dots = dot_ls, .drop = col_plan$.drop))
  }
}

unpack_col_style_plan <- function(){

}


unpack_body_plan <- function(body_plan){
  browser()
body_plan %>%
  map(function(x){
    x["frmt_to_apply"]["frmt_ls"]
    browser()
    x
  })
  purrr::flatten()

}


json_to_tfrmt <- function(){
  # foo <- base_ts$col_plan$dots[1] %>%
  #   rlang::parse_quo(env = rlang::current_env())
  # tibble(col1 = letters[1:10], col2 = letters [11:20]) %>%
  #   select(!!foo)
}

