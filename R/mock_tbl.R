

#' @importFrom tidyr crossing
#' @importFrom dplyr rowwise mutate pull rename ungroup
#' @importFrom purrr reduce
make_mock_data <- function(tfrmt, .default = 1:3, n_cols = 3){
  body_style <- tfrmt$body_style
  cols <- tibble(
    grp = body_style %>% process_for_mock("group_val", .default),
    lab = body_style %>% process_for_mock("label_val", .default),
    param = body_style %>% process_for_mock("param_val", .default=1)
  )

  col_names <- paste0("col", seq(1:n_cols))

  cols %>%
    rowwise() %>%
    mutate(tbl = list(crossing(grp,lab,param))) %>%
    pull(tbl) %>%
    reduce(bind_rows) %>%
    rename(!!tfrmt$group[[1]] := grp,
           !!tfrmt$label := lab,
           !!tfrmt$param := param) %>%
    crossing(!!tfrmt$column := col_names) %>%
    ungroup()

}



process_for_mock <-function(body_style, column, .default = 1:3){
  body_style %>%
    map(function(x){
      out <- x[column]
      if(out == ".default"){
        out <- str_remove(column, "_val") %>%
          str_c(.default)
      } else {
        out <- out[[1]]
      }
      out
    })
}

