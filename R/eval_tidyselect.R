## ---------------------------------------
## determine which columns to span across
## ---------------------------------------
eval_tidyselect_on_colvec <- function(x, column_vec) {
    UseMethod("eval_tidyselect_on_colvec", x)
}

eval_tidyselect_on_colvec.quosures <- function(x, column_vec) {
    names(column_vec) <- column_vec
    avli_x <- x %>%
        purrr::keep(~ as_label(.) %in% column_vec)

    names(
        tidyselect::eval_select(
            expr(c(!!!avli_x)),
            data = column_vec
        )
    )
}

eval_tidyselect_on_colvec.quosure <- function(x, column_vec) {
    names(column_vec) <- column_vec

    names(
        tidyselect::eval_select(
            expr(c(!!x)),
            data = column_vec
        )
    )
}
