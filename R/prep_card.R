#' Prepare card for tfrmt
#'
#' What does the preparation function do?
#'  * `prep_unite_vars()`: brings all categorical variables levels into a single
#'  column, called `variable_level` (where applicable).
#'  * `prep_big_n()`: recodes the `"n"` `stat_name` into `"bigN"` for the
#'  desired columns.
#'  * `prep_label()`: creates a `label` column from `variable_level` for
#'  categorical variables and `stat_label` for all other variable types.
#'  * `prep_fill_pairwise()`: in a hierarchical stack replaces NA in one column
#'  conditional on the presence of data in another column.
#'
#' @inheritParams tfrmt
#' @inheritParams shuffle_card
#' @param column (character) variable(s) to use for column names.
#' @param group (character) grouping variables to use in the formatted table.
#' @param variables (character) incoming `cards` variables.
#' @param fill (character) value to fill with. Defaults to `"Any {colname}"`.
#'   Not used if `fill_from` is specified.
#' @param fill_from (character) Indicates when doing pair-wise filling whether
#' to fill from the column to the left. Defaults to `NULL`. Can be either `NULL`
#' or `"left"`.
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_card <- function(x,
                      column = NULL,
                      group = NULL,
                      variables = NULL,
                      fill_overall = "Overall {colname}",
                      fill = "Any {colname}",
                      fill_from = NULL) {

  # TODO check the error is propagated from the right caller env

  if (!is.null(column) & !is.character(column)) {
    cli::cli_abort(
      "{.arg column} must be a character vector."
    )
  }

  ard_args <- attr(x, "args")

  shuffled_card <- x

  if (!is_shuffled_card(x)) {
    shuffled_card <- shuffle_card(
      x,
      # TODO message about switching the value for `by`
      by = ard_args$by %||% c(column, group),
      fill_overall = fill_overall,
      fill_hierarchical_overall = fill
    )
  }

  # TODO get the logic to work with strings and then maybe add support for
  # symbols / unquoted strings
  # with tfrmt_find_args(..., env = environment(), parent_env = caller_env())

  if (has_attributes(shuffled_card)) {
    shuffled_card <- shuffled_card |>
      # remove attributes for now
      # TODO add some logic to deal with them - useful for labelled columns
      dplyr::filter(
        .data$context != "attributes"
      )
  }

  ard_vars <- c(
    "context",
    "stat_variable",
    "stat_name",
    "stat_label",
    "stat"
  )

  vars_to_unite <- setdiff(names(shuffled_card), c(ard_vars, column))

  output <- shuffled_card |>
    prep_unite_vars(vars = vars_to_unite) |>
    prep_big_n(vars = column) |>
    prep_label() |>
    prep_fill_pairwise(
      vars = ard_args$variables %||% variables,
      fill = fill,
      fill_from = fill_from
    )

  output
}

#' Unite variables
#'
#' A wrapper around `tidyr::unite()` which pastes several columns into one.
#' In addition it checks the output is identical to `dplyr::coalesce()`. If not
#' the input is returned unchanged. Useful for uniting sparsely populated
#' columns, for example when processing a `shuffled_card` created with
#' `card::ard_stack()`.
#'
#' If the data is the result of a hierarchical ard stack, the input is returned
#' unchanged.
#'
#' @param x (data.frame) a shuffled card
#' @param vars (character) a vector of variables to unite. If a single variable
#'   is supplied, the input is returned unchanged.
#' @inheritParams tidyr::unite
#'
#' @returns a data.frame with an additional column, called `variable_level` or
#'   the input unchanged.
#' @export
#'
#' @examples
prep_unite_vars <- function(x, vars, remove = TRUE) {

  if ("hierarchical" %in% unique(x$context)) {
    return(x)
  }

  # we do cannot unite a single variable
  if (length(vars) == 1) {
    return(x)
  }

  interim <- x |>
    mutate(
      var_level_coalesced = coalesce(
        !!!rlang::syms(vars)
      )
    ) |>
    tidyr::unite(
      col = "var_level_untd",
      tidyselect::all_of(vars),
      na.rm = TRUE,
      remove = remove
    ) |>
    dplyr::mutate(
      var_level_untd = dplyr::if_else(
        .data$var_level_untd == "",
        NA_character_,
        .data$var_level_untd
      )
    )

  if (!identical(interim$var_level_untd, interim$var_level_coalesced)) {
    return(x)
  }

  output <- interim |>
    dplyr::select(
      -"var_level_coalesced"
    ) |>
    dplyr::rename(
      "variable_level" = "var_level_untd",
    )

  output
}

#' Prepare `bigN` stat variables
#'
#' `prep_big_n()` does 2 things:
#'   * it recodes the `"n"` `stat_name` into `bigN` for the desired variables,
#'   and
#'   * it drops all other `stat_names` for the same variables.
#'
#' If your `tfrmt` contains a [big_n_structure()] you pass the tfrmt `column` to
#' `prep_big_n()` via `vars`.
#'
#' @param x (data.frame)
#' @param vars (character) a vector of variables to prepare `bigN` for.
#'
#' @returns a data.frame with the same columns as the input. The `stat_name`
#'   column is modified.
#' @export
#'
#' @examples
prep_big_n <- function(x, vars) {
  output <- x |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        .data$context == "total_n" ~ "bigN",
        # we only want to keep the subgroup totals, which get recoded to bigN
        .data$stat_variable %in% vars & .data$stat_name == "n" ~ "bigN",
        # we only want the bigN for overall -> we remove "out"
        .data$stat_variable %in% vars & .data$stat_name != "n" ~ "out",
        TRUE ~ .data$stat_name
      )
    ) |>
    dplyr::filter(
      .data$stat_name != "out"
    )

  output
}

#' Prepare label
#'
#' Adds a `label` column which is a combination of `stat_label` (for continuous
#' variables) and `variable_level` (for categorical ones) if these 2 columns are
#' present in the input data frame.
#'
#' @param x (data.frame) a data.frame downstream of `shuffle_card()`. Does not
#'   necessarily need to be a `shuffled_card` object.
#'
#' @returns a data.frame with a `label` column (if the input has the required
#'   columns) or the input unchanged.
#' @export
#'
#' @examples
prep_label <- function(x) {

  if (!all(c("variable_level", "stat_label") %in% names(x))) {
    return(x)
  }

  output <- x |>
    dplyr::mutate(
      label = .data$stat_label,
      label = dplyr::if_else(
        .data$context == "categorical",
        .data$variable_level,
        .data$label
      )
    )

  output
}

# replace_na with a given value (defaults to "Any <column-name>") or with values
# from the column to the left when the preceding column is not NA
prep_fill_pairwise <- function(x,
                               vars,
                               fill = "auto",
                               fill_from = NULL) {

  if (!rlang::is_character(fill)) {
    cli::cli_abort(
      "{.arg fill} must be a character."
    )
  }

  if (length(vars) < 2) {
    return(x)
  }

  if (fill == "Any {colname}") {
    fill <- "auto"
  }

  pair_list <- generate_pairs(vars)

  output <- x

  for (i in seq_along(pair_list)) {
    output <- replace_na_pairwise(
      output,
      pair = pair_list[[i]],
      fill = fill,
      fill_from = fill_from
    )
  }

  output
}

# prep_fill_pairwise does pairwise conditional replacement of NAs. generate_pairs
# builds those pairs
generate_pairs <- function(x) {
  output <- tibble::tibble(x = x) |>
    dplyr::mutate(
      x_lead = dplyr::lead(x)
    ) |>
    tidyr::drop_na() |>
    purrr::pmap(c, use.names = FALSE)

  output
}



# replace missing values in one variable if a another variable is not NA
# this is the function used by prep_fill_pairwise to iterate over the pairs of
# columns
replace_na_pairwise <- function(x,
                                pair,
                                fill = "auto",
                                fill_from = NULL) {

  if (!is.null(fill_from) && fill_from != "left") {
    cli::cli_abort(
      '{.arg fill_from} must either be `NULL` or `"left"`'
    )
  }

  variables_syms <- rlang::syms(pair)

  if (fill == "auto") {
    fill <- glue::glue("Any {variables_syms[[2]]}") |>
      as.character()
  }

  if (!is.null(fill_from) && fill_from == "left") {
    fill <- rlang::quo(as.character(!!variables_syms[[1]]))
  }

  output <- x |>
    dplyr::mutate(
      !!variables_syms[[2]] := dplyr::if_else(
        is.na(!!variables_syms[[2]]) & !is.na(!!variables_syms[[1]]),
        !!fill,
        !!variables_syms[[2]]
      )
    )

  output
}

# does the shuffled card have attributes (was the card created with
# `attributes = TRUE`)
# useful for ensuring column labels are persistent
has_attributes <- function(x) {
  shuffled_card_attributes_df <- x |>
    dplyr::filter(
      .data$context == "attributes"
    )

  output <- nrow(shuffled_card_attributes_df) > 0

  output
}
