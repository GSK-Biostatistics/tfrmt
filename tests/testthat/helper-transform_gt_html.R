strip_id <- function(x) {
  stringr::str_replace_all(x, 'id="\\w{10}"', 'id="stripped_id"')
}

