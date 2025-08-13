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
#' df <- data.frame(
#'   stat_name = c("n", "max", "min", rep(c("n", "N", "p"), times = 2)),
#'   context = rep(c("continuous", "hierarchical", "categorical"), each = 3),
#'   stat_variable = rep(c("a", "b", "c"), each = 3)
#' ) |>
#'   dplyr::bind_rows(
#'     data.frame(
#'       stat_name = "n",
#'       context = "total_n",
#'       stat_variable = "d"
#'     )
#'   )
#'
#' prep_big_n(
#'   df,
#'   vars = c("b", "c")
#' )
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
#' df <- data.frame(
#'   variable_level = c("d", "e", "f"),
#'   stat_label = c("a", "b", "c"),
#'   context = c("categorical", "continuous", "hierarchical")
#' )
#'
#' prep_label(df)
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
#' Replace `NA` in pairs
#'
#' Replace `NA` values in one column conditional on the same row having a
#' non-NA value in a different column.
#'
#' The user supplies a vector of columns from which the pairs will be extracted
#' with a rolling window. For example `vars <- c("A", "B", "C")` will generate
#' 2 pairs `("A", "B")` and `("B", "C")`. Therefore the order of the variables
#' matters.
#'
#' In each pair the second column `B` will be filled if `A` is not missing. One
#' can choose the value to fill with:
#'   * `"Any {colname}"`, in this case evaluating to `"Any B"` is the default.
#'   * Any other value. For example `"Any event"` for an adverse effects table.
#'   * the value of pair's first column. In this case, the value of `A`.
#'
#' @param x (data.frame) a shuffled card.
#' @param vars (characters) a vector of variables to generate pairs from.
#' @param fill (character) value to replace with. Defaults to `"Any {colname}"`,
#'   in which case `colname` will be replaced with the name of the column.
#' @param fill_from (character) indicating whether to fill from the left column
#'   of the pair.
#'
#' @returns a data.frame with the same columns as the input, but in which some
#'   the desired columns have been filled pairwise.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(1, 2, NA),
#'   y = c("a", NA, "b"),
#'   z = rep(NA, 3)
#' )
#'
#' prep_fill_pairwise(
#'   df,
#'   vars = c("x", "y")
#' )
#'
#' prep_fill_pairwise(
#'   df,
#'   vars = c("x", "y"),
#'   fill = "foo"
#' )
#'
#' #' prep_fill_pairwise(
#'   df,
#'   vars = c("x", "y", "z"),
#'   fill_from = "left"
#' )
prep_fill_pairwise <- function(x,
                               vars,
                               fill = "Any {colname}",
                               fill_from = NULL) {

  if (length(vars) < 2) {
    return(x)
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

#' Generate pairs for pairwise filling
#'
#' [prep_fill_pairwise()] does pairwise conditional replacement of `NA`s.
#' `generate_pairs()` builds those pairs.
#'
#' @param x (character) a vector of 2 or more column names
#' @inheritParams cli::cli_abort
#'
#' @returns a list of length 2 character vectors (pairs of column names)
#' @keywords internal
#'
#' @examples
#' tfrmt:::generate_pairs(c("foo", "bar", "baz"))
generate_pairs <- function(x, call = rlang::caller_env()) {

  if (!rlang::is_character(x)) {
    cli::cli_abort(
      "{.arg x} must be a character vector. You have supplied \\
      {.obj_type_friendly {x}}."
    )
  }

  if (length(x) < 2) {
    cli::cli_abort(
      "{.arg x} must contain at least 2 column names. It contains {length(x)}.",
      call = call
    )
  }

  output <- tibble::tibble(x = x) |>
    dplyr::mutate(
      x_lead = dplyr::lead(x)
    ) |>
    tidyr::drop_na() |>
    purrr::pmap(c, use.names = FALSE)

  output
}

#' Replace `NA`s pairwise conditionally
#'
#' Replace missing values in one variable if a another variable is not `NA`.
#' This is the function used by [prep_fill_pairwise()] to iterate over the pairs
#' of columns.
#'
#' @param x (data.frame) a shuffled card.
#' @param pair (character) a vector of exactly 2 column names.
#' @inheritParams prep_card
#' @inheritParams cli::cli_abort
#'
#' @returns a list of length 2 character vectors (pairs of column names)
#' @keywords internal
#'
#' @examples
#' replace_na_pairwise(
#'   data.frame(
#'   x = c(1, 2, NA),
#'   y = c("a", NA, "b"),
#'   z = rep(NA, 3)
#'   ),
#'   pair = c("y", "z")
#' )
replace_na_pairwise <- function(x,
                                pair,
                                fill = "Any {colname}",
                                fill_from = NULL,
                                call = rlang::caller_env()) {

  if (!rlang::is_character(pair)) {
    cli::cli_abort(
      "{.arg pair} must be a character vector. You have supplied \\
      {.obj_type_friendly {pair}}.",
      call = call
    )
  }

  if (length(pair) != 2) {
    cli::cli_abort(
      "{.arg pair} must contain exactly 2 elements. The one you supplied has \\
      {length(pair)}.",
      call = call
    )
  }

  if (!rlang::is_scalar_character(fill)) {
    cli::cli_abort(
      "{.arg fill} must be a character vector of length 1.",
      call = call
    )
  }

  if (!is.null(fill_from) && fill_from != "left") {
    cli::cli_abort(
      '{.arg fill_from} must either be `NULL` or `"left"`. \\
      {.code "{fill_from}"} is not an accepted value.',
      call = call
    )
  }

  variables_syms <- rlang::syms(pair)

  if (fill == "Any {colname}") {
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
