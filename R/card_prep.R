#' Prepare card for tfrmt
#'
#' What does the preparation function need to do?
#'   * process labels: labels are persistent in cards and passed down via
#'  attributes (e.g. by `cards::ard_stack(..., .attributes = TRUE)`)
#'  * `prepare_big_n()`: Is `bigN` only used for categorical variables?
#'
#'
#' @param x (`card`) a `card` object
#' @param column (character) column to use as header. symbol?
#' @param tbl_header (character) variable to use in table header.
#' @param variables (character) variables.
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x, column = NULL, tbl_header = NULL, variables = NULL) {

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
    # process big N by (header) column, not grouping variables
    process_big_n_col_header(column = column) |>
    augment_with_big_n(column = tbl_header) |>
    dplyr::mutate(
      variable = dplyr::coalesce(
        .data$variable_label,
        .data$variable
      )
    ) |>
    dplyr::select(
      all_of(column),
      # use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      # we need to use `"variable"` instead of `.data$variable`
      "variable",
      "label",
      "stat_name",
      "stat"
    ) |>
    unique() |>
    tidyr::unnest(
      "stat"
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
      # Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      # we need to use `"stat"` instead of `.data$stat`
      "variable",
      "variable_label" = "stat"
    ) |>
    tidyr::unnest(
      "variable_label"
    )

  output <- x |>
    dplyr::left_join(
      variable_labels,
      by = dplyr::join_by(
        "variable"
      )
    ) |>
    dplyr::relocate(
      "variable_label",
      .after = "variable"
    ) |>
    # remove attributes
    dplyr::filter(
      .data$context != "attributes"
    )

  output
}

# `column` here is the same value as the `column` argument
# from `tfrmt(..., column = , ...)`
process_big_n_col_header <- function(x, column = NULL) {

  # this corresponds to bigN structure

  if(is.null(column)) {
    return(x)
  }

  if (length(column) != 1) {
    stop(
      "`process_big_n_col_header` supports a single column.",
      call. = FALSE
    )
  }

  # TODO maybe loon into supporting multiple columns (with `rlang::syms()`). `
  # sym()` if we pass column as a string
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
        # TODO handle multiple grouping variables
        # bigN can be for multiple grouping variables. it can only be used in
        # the column headers though
        .data$variable == column,
        .data$label,
        !!col_sym
      )
    ) |>
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
    # keep only the big N rows for the target variable
    dplyr::filter(
      !(.data$variable == column & .data$stat_name != "bigN")
    )

  output
}

augment_with_big_n <- function(x, column = NULL) {
  # this attempts to cater to the case when we do not have a big_n_structure,
  # but we still have big N's displayed somewhere (for example in table headers)
  # adds the bigN values to the column values

  if(is.null(column)) {
    return(x)
  }

  if (length(column) != 1) {
    stop(
      "`process_big_n_col_header` supports a single column.",
      call. = FALSE
    )
  }

  sym_column <- rlang::sym(column)

  pattern <- glue::glue("Overall {column}")

  augmented_big_n <- x |>
    dplyr::filter(
      !is.na(.data[[column]]),
      .data[[column]] != pattern,
      .data$variable == column,
      .data$stat_name == "n"
    ) |>
    dplyr::mutate(
      col_augmented = glue::glue(
        "{.data[[column]]} (N={stat})"
      )
    )

  output <- x |>
    dplyr::left_join(
      augmented_big_n,
      by = names(x)
    ) |>
    dplyr::mutate(
      !!sym_column := dplyr::coalesce(
        .data$col_augmented,
        .data[[column]]
      )
    ) |>
    # use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
    # need to use `"col_augmented"` instead of `.data$col_augmented`
    dplyr::select(-"col_augmented") |>
    # rests on the assumption that augmented columns will play a special role
    # and we are not interested in the absolute totals
    dplyr::filter(
      # we do not keep the `Overall ...` bigN's as we only want the group ones
      !(.data[[column]] == pattern & .data$stat_name != "bigN")
    )

  output
}

order_rows_n_first <- function(x) {
  output <- x |>
    dplyr::mutate(
      stat_name_order = dplyr::if_else(
        .data$stat_name == "N",
        1,
        2
      )
    ) |>
    dplyr::arrange(
      .data$variable,
      .data$stat_name_order
    ) |>
    # use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
    # need to use `"stat_name_order"` instead of `.data$stat_name_order`
    dplyr::select(
      -"stat_name_order"
    )

  output
}
