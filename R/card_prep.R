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

  # TODO support tfrmt (priority 1), direct passing of args (2) and extracting from
  # attributes (3)

  # extract metadata (from the shuffle output)
  card_args <- attributes(x)[["args"]]
  card_by <- card_args$by
  card_variables <- card_args$variables

  # TODO handle multiple grouping variables
  #
  first_grouping_var <- card_by[[1]]

  # figure out which column(s) - currently supporting only one - will be spread
  # and will make the column headers
  column <- tfrmt$column |>
    purrr::map(rlang::quo_get_expr) |>
    purrr::map_chr(rlang::as_string)

  output <- x |>
    process_labels() |>
    # process big N by columns, not grouping variables
    process_big_n(column = column) |>
    dplyr::mutate(
      variable = dplyr::coalesce(
        .data$variable_label,
        .data$variable
      )
    ) |>
    dplyr::select(
      # might be the spread column and not the grouping variable
      !!sym(first_grouping_var),
      .data$variable,
      .data$label,
      .data$stat_name,
      .data$stat,
      .data$ord1,
      .data$ord2
    ) |>
    unique() |>
    tidyr::unnest(
      .data$stat
    )

  output
}



has_attributes <- function(x) {
  shuffled_card_attributes_df <- x |>
    dplyr::filter(
      .data$context == "attributes"
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
    dplyr::filter(
      .data$context == "attributes" &
      .data$stat_label == "Variable Label"
    ) |>
    dplyr::select(
      .data$variable,
      variable_label = .data$stat
    ) |>
    tidyr::unnest(
      .data$variable_label
    )

  output <- x |>
    dplyr::left_join(
      variable_labels,
      by = dplyr::join_by(variable)
    ) |>
    dplyr::relocate(
      .data$variable_label,
      .after = .data$variable
    ) |>
    # remove attributes
    dplyr::filter(
      .data$context != "attributes"
    )

  output
}

process_big_n <- function(x, column) {
# browser()
  # I originally thought we process bigN for the grouping variable (hence `by`),
  # but we should process it based on the tfrmt$column (as this indicates which
  # column will be spread -> mapped to the column headers)

  # TODO likely will need to be aware of the various big N's. the only thing we
  # can do is prepare bigN everywhere where it exists and display it. if the user
  # does not want it it needs to be removed from the ARD. the tfrmt bigN does not
  # do any selection

  # we are interested in the relationship between column headers (given by the
  # tfrmt$column )

  by <- column
  # TODO support multiple columns (with `rlang::ensyms()`)
  sym_by <- rlang::ensym(column)

  output <- x |>
    # derive `label`
    dplyr::mutate(
      label = dplyr::coalesce(
        .data$variable_level,
        .data$stat_label
      )
    ) |>
    dplyr::mutate(
      !!sym_by := dplyr::if_else(
        # variable == "ARM",
        # TODO handle multiple grouping variables
        # bigN can be for multiple grouping variables. it can only be used in
        # the column headers though
        .data$variable == by,
        .data$label,
        # ARM
        !!sym_by
      ),
      !!sym_by := dplyr::if_else(
        is.na(!!sym_by) | .data$variable == "..ard_total_n..",
        glue::glue("Overall {rlang::as_string(sym_by)}"),
        !!sym_by
      )
    ) |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        .data$variable == by & .data$stat_name == "n" ~ "bigN",
        .data$variable == "..ard_total_n.." ~ "bigN",
        TRUE ~ .data$stat_name
      ),
      label = dplyr::if_else(
        .data$stat_name == "N",
        "n",
        .data$label
      )
    ) |>
    # remove unneeded stats
    # if variable is "ARM" keep only the big N rows
    # drop the rows where the variable is "ARM" and the stat_name is not `"bigN"`
    dplyr::filter(
      !(.data$variable == by & .data$stat_name !="bigN")
    )

  output
}
