#' Prepare card for tfrmt
#'
#' What does the preparation function need to do?
#'   * process labels: labels are persistent in cards and passed down via
#'  attributes (e.g. by `ard_stack(..., .attributes = TRUE)`)
#'  * `prepare_big_n()`: Is `bigN` only used for categorical variables?
#'
#'
#' @param x (`card`) a `card` object
#' @param column (character) column to use as header. symbol?
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x, column = NULL, variables = NULL) {

  # browser()
  # TODO priority for extracting context - e.g. by variables, etc:
  #   1. direct passing of args
  #   2. from attributes
  #   3. tfrmt object

  # extract metadata (from the shuffle output)
  card_args <- attr(x, "args")
  card_by <- card_args$by
  card_variables <- card_args$variables

  if (is.null(column)) {
    # TODO handle multiple grouping variables?
    # this assumption (the column making up the headers is the first of the
    # ard `by` columns)
    column <- card_by[1]
  }

  if (is.null(variables)) {
    variables <- card_variables
  }

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
      .data[[column]],
      .data$variable,
      .data$label,
      .data$stat_name,
      .data$stat
    ) |>
    unique() |>
    tidyr::unnest(
      .data$stat
    ) |>
    order_rows_n_first()

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
      by = dplyr::join_by(
        variable
      )
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

  # TODO support multiple columns (with `rlang::syms()`). `sym()` if we pass
  # column as a string
  col_sym <- rlang::sym(column)

  output <- x |>
    # derive `label`
    dplyr::mutate(
      label = dplyr::coalesce(
        .data$variable_level,
        .data$stat_label
      )
    ) |>
    dplyr::mutate(
      !!col_sym := dplyr::if_else(
        # variable == "ARM",
        # TODO handle multiple grouping variables
        # bigN can be for multiple grouping variables. it can only be used in
        # the column headers though
        .data$variable == column,
        .data$label,
        # ARM
        !!col_sym
      )
    ) |>
    # TODO we might not need this since it should now be handled by `shuffle_ard()`
    # mutate(
    #   !!col_sym := dplyr::if_else(
    #     is.na(!!col_sym) | .data$variable == "..ard_total_n..",
    #     glue::glue("Overall {column}"),
    #     !!col_sym
    #   )
    # ) |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        .data$variable == column & .data$stat_name == "n" ~ "bigN",
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
      !(.data$variable == column & .data$stat_name != "bigN")
    )

  output
}

order_rows_n_first <- function(x) {
  output <- x |>
    dplyr::mutate(
      stat_name_order = dplyr::if_else(
        stat_name == "N",
        1,
        2
      )
    ) |>
    dplyr::arrange(
      variable,
      stat_name_order
    ) |>
    dplyr::select(
      -stat_name_order
    )

  output
}
