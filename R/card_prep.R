#' Prepare card for tfrmt
#'
#' What does the preparation function need to do?
#   *
#'  * Is `bigN` only used for categorical variables?
#'
#' @param x (card) card object
#' @param tfrmt (`tfrmt`) `tfrmt` object
#' @param var_order variable order.
#' @param stat_order stat order
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x, tfrmt, var_order, stat_order = "n") {

  # extract metadata (from the shuffle output)
  card_args <- attributes(x)[["args"]]
  card_by <- card_args$by
  card_variables <- card_args$variables

  # TODO handle multiple grouping variables
  first_grouping_var <- card_by[[1]]

  output <- x |>
    process_labels() |>
    process_big_n(by = first_grouping_var) |>
    process_order(
      var_order = var_order,
      stat_order = stat_order
    ) |>
    dplyr::mutate(
      variable = dplyr::coalesce(
        variable_label,
        variable
      )
    ) |>
    dplyr::select(
      !!sym(first_grouping_var),
      variable,
      label,
      stat_name,
      stat,
      ord1,
      ord2
    ) |>
    unique() |>
    tidyr::unnest(stat)

  output
}



has_attributes <- function(x) {
  shuffled_card_attributes_df <- x |>
    dplyr::filter(
      context == "attributes"
    )

  output <- FALSE

  if (nrow(shuffled_card_attributes_df) > 0) {
    output <- TRUE
  }

  output
}

process_labels <- function(x) {

  if (!has_attributes(x)) {
    output <- x |>
      dplyr::mutate(
        variable_label = NA_character_
      )

    return(output)
  }

  variable_labels <- x |>
    dplyr::filter(context == "attributes") |>
    dplyr::filter(stat_label == "Variable Label") |>
    dplyr::select(
      variable,
      variable_label = stat
    ) |>
    tidyr::unnest(variable_label)

  output <- x |>
    dplyr::left_join(
      variable_labels,
      by = dplyr::join_by(variable)
    ) |>
    dplyr::relocate(
      variable_label,
      .after = variable
    ) |>
    # remove attributes
    dplyr::filter(
      context != "attributes"
    )

  output
}

process_big_n <- function(x, by) {

  by <- force(by)
  sym_by <- rlang::ensym(by)

  output <- x |>
    # derive `label`
    dplyr::mutate(
      label = dplyr::coalesce(.data$variable_level, .data$stat_label)
    ) |>
    dplyr::mutate(
      !!sym_by := dplyr::if_else(
        # variable == "ARM",
        # TODO handle multiple grouping variables
        # TODO check understanding: is bigN only for the outer (first) grouping
        # variable?
        variable == by,
        label,
        # ARM
        !!sym_by
      ),
      !!sym_by := dplyr::if_else(
        is.na(!!sym_by) | variable == "..ard_total_n..",
        "Total",
        !!sym_by
      )
    ) |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        variable == by & stat_name == "n" ~ "bigN",
        variable == "..ard_total_n.." ~ "bigN",
        TRUE ~ stat_name
      ),
      label = dplyr::if_else(
        stat_name == "N",
        "n",
        label
      )
    ) |>
    # remove unneeded stats
    # if variable is "ARM" keep only the big N rows
    # drop the rows where the variable is "ARM" and the stat_name is not `"bigN"`
    dplyr::filter(!(variable == by & stat_name !="bigN"))

  output
}

process_order <- function(x, var_order, stat_order = "n") {

  # TODO check assumption holds
  # assumption: ordering early works and we do not need to do that via the
  # sorting_cols = c(ord1, ord2) argument
  output <- x |>
    dplyr::mutate(
      ord1 = forcats::fct_inorder(variable) |>
        forcats::fct_relevel(var_order, after = 0) |>
        as.numeric(),
      ord2 = dplyr::if_else(
        label == stat_order,
        1,
        2
      )
    )

  output
}
