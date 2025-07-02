#' Prepare card for tfrmt
#'
#' What does the preparation function need to do?
#   *
#'  * Is `bigN` only used for categorical variables?
#'
#' @param x (card) card object
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

#' Extract card metadata
#'
#' `extract_card_metadata()` reverse engineers the `ard_stack()` call based on
#' its output. It extracts the grouping variable, continuous and categorical
#' variables and their statistics.
#'
#' @param x (card) card object
#'
#' @returns a list made up of 5 character vectors:
#'   * `grouping_variables`: names of the grouping variables
#'   * `continuous_variables`
#'   * `categorical_variables`
#'   * `continuous_variables_stats`
#'   * `categorical_variables_stats`
#'
#' @export
#'
#' @examples
extract_card_metadata <- function(x) {

  # extract metadata (`.by`, variables, and stats) from the card object
  # .by
  ard_by_var <- extract_grouping_variables(x)

  # extract variables
  ard_continuous_variables <- extract_variables(x, type = "continuous")
  ard_categorical_variables <- extract_variables(x, type = "categorical")

  # extract stats
  ard_continuous_stats <- extract_stats(x, type = "continuous")
  ard_categorical_stats <- extract_stats(x, type = "categorical")

  output <- list(
    grouping_variables = ard_by_var,
    continuous_variables = ard_continuous_variables,
    categorical_variables = ard_categorical_variables,
    continuous_variables_stats = ard_continuous_stats,
    categorical_variables_stats = ard_categorical_stats
  )

  output
}

extract_grouping_variables <- function(x) {
browser()
  grouping_variables <- x |>
    dplyr::select(
      tidyselect::contains("group")
    ) |>
    dplyr::select(
      !tidyselect::contains("level")
    ) |>
    dplyr::distinct() |>
    stats::na.omit() |>
    tidyr::pivot_longer(
      cols = tidyselect::everything()
    ) |>
    # we want group1 to be first, followed by group2 etc ...
    dplyr::arrange(
      .data$name
    ) |>
    dplyr::pull(
      .data$value
    )
}

extract_variables <- function(x, type = c("continuous", "categorical")) {

  type <- rlang::arg_match(type)

  output <- x |>
    dplyr::filter(
      .data$context == type
    ) |>
    dplyr::distinct(.data$variable) |>
    dplyr::pull(.data$variable)

  output
}

extract_stats <- function(x, type = c("continuous", "categorical")) {

  type <- rlang::arg_match(type)

  categorical_stats <- c("n", "p")
  common_stats <- "N"
  unique_stats <- unique(x$stat_name)

  continuous_ard_stats <- setdiff(unique_stats, categorical_stats)
  output <- continuous_ard_stats

  if (type == "categorical") {
    categorical_ard_stats <- unique_stats |>
      setdiff(continuous_ard_stats) |>
      union(common_stats)
    output <- categorical_ard_stats
  }

  output
}
