#' Combine variables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' A wrapper around `tidyr::unite()` which pastes several columns into one.
#' In addition it checks the output is identical to `dplyr::coalesce()`. If not
#' identical, the input data.frame is returned unchanged. Useful for uniting
#' sparsely populated columns, for example when processing a `shuffled_card`
#' created with [cards::ard_stack()].
#'
#' If the data is the result of a hierarchical ard stack (with
#' [cards::ard_stack_hierarchical()] or [cards::ard_stack_hierarchical_count()]),
#' the input is returned unchanged.
#'
#' @param df (data.frame)
#' @param vars (character) a vector of variables to unite. If a single variable
#'   is supplied, the input is returned unchanged.
#' @inheritParams tidyr::unite
#'
#' @returns a data.frame with an additional column, called `variable_level` or
#'   the input unchanged.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   a = 1:6,
#'   context = rep("categorical", 6),
#'   b = c("a", rep(NA, 5)),
#'   c = c(NA, "b", rep(NA, 4)),
#'   d = c(NA, NA, "c", rep(NA, 3)),
#'   e = c(NA, NA, NA, "d", rep(NA, 2)),
#'   f = c(NA, NA, NA, NA, "e", NA),
#'   g = c(rep(NA, 5), "f")
#' )
#'
#' prep_combine_vars(
#'   df,
#'   vars = c("b", "c", "d", "e", "f", "g")
#' )
prep_combine_vars <- function(df, vars, remove = TRUE) {

  if ("hierarchical" %in% unique(df$context)) {
    return(df)
  }

  # we do cannot unite a single variable
  if (length(vars) == 1) {
    return(df)
  }

  interim <- df |>
    dplyr::mutate(
      var_level_coalesced = coalesce(
        !!!rlang::syms(vars)
      )
    ) |>
    tidyr::unite(
      col = "var_level_untd",
      dplyr::all_of(vars),
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
    return(df)
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
#' @description `r lifecycle::badge('experimental')`\cr
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
prep_big_n <- function(df, vars) {
  output <- df |>
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
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' Adds a `label` column which is a combination of `stat_label` (for continuous
#' variables) and `variable_level` (for categorical ones) if these 2 columns are
#' present in the input data frame.
#'
#' @param df (data.frame)
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
prep_label <- function(df) {

  if (!all(c("variable_level", "stat_label") %in% names(df))) {
    return(df)
  }

  output <- df |>
    dplyr::mutate(
      label = .data$stat_label,
      label = dplyr::if_else(
        .data$context %in% c("categorical", "tabulate"),
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
#' @description `r lifecycle::badge('experimental')`\cr
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
#' @param df (data.frame)
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
#' prep_hierarchical_fill(
#'   df,
#'   vars = c("x", "y")
#' )
#'
#' prep_hierarchical_fill(
#'   df,
#'   vars = c("x", "y"),
#'   fill = "foo"
#' )
#'
#' prep_hierarchical_fill(
#'   df,
#'   vars = c("x", "y", "z"),
#'   fill_from = "left"
#' )
prep_hierarchical_fill <- function(df,
                                   vars,
                                   fill = "Any {colname}",
                                   fill_from = NULL) {

  if (length(vars) < 2) {
    return(df)
  }

  pair_list <- generate_pairs(vars)

  output <- df

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
#' [prep_hierarchical_fill()] does pairwise conditional replacement of `NA`s.
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
#' This is the function used by [prep_hierarchical_fill()] to iterate over the
#' pairs of columns.
#'
#' @param x (data.frame) a shuffled card.
#' @param pair (character) a vector of exactly 2 column names.
#' @inheritParams prep_hierarchical_fill
#' @inheritParams cli::cli_abort
#'
#' @returns a list of length 2 character vectors (pairs of column names)
#' @keywords internal
#'
#' @examples
#' tfrmt:::replace_na_pairwise(
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

#' Check `card` has attributes
#'
#' Checks if `card` was created with `attributes = TRUE`. Useful when working
#' with labelled columns and we want to ensure the labels are persistent. Also
#' useful if we want to trim the incoming shuffled card.
#'
#' @param x (data.frame) a shuffled card
#'
#' @returns `TRUE` if the input data.frame has `"attributes"` in the `context`
#'   column.
#' @keywords internal
#'
#' @examples
#' df <- data.frame(
#'   context = c("categorical", "attributes"),
#'   a = 1:2
#' )
#'
#' tfrmt:::is_card_with_attributes(df)
is_card_with_attributes <- function(x) {
  shuffled_card_attributes_df <- x |>
    dplyr::filter(
      .data$context == "attributes"
    )

  output <- nrow(shuffled_card_attributes_df) > 0

  output
}
