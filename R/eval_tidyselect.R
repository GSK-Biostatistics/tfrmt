## ---------------------------------------
## determine which columns to span across
## ---------------------------------------
eval_tidyselect_on_colvec <- function(x, column_vec) {
    UseMethod("eval_tidyselect_on_colvec", x)
}

#' @export
eval_tidyselect_on_colvec.quosures <- function(x, column_vec) {
    names(column_vec) <- column_vec
    avli_x <- x %>%
        purrr::keep(~ rlang::as_label(.) %in% column_vec)

    names(
        tidyselect::eval_select(
            rlang::expr(c(!!!avli_x)),
            data = column_vec
        )
    )
}
#' @export
eval_tidyselect_on_colvec.quosure <- function(x, column_vec) {
    names(column_vec) <- column_vec

    names(
        tidyselect::eval_select(
            rlang::expr(c(!!x)),
            data = column_vec
        )
    )
}
