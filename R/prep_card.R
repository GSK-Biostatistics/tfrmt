#' Prepare card for tfrmt
#'
#' What does the preparation function do?
#'  * `unite_data_vars()`: brings all categorical variables levels into a single
#'  column, called `variable_level` (where applicable).
#'  * `process_big_n()` identifies the bigN columns and changes `stat_name` to
#'  `"bigN"`
#'  * `process_categorical_vars()`: once we have bigN, it renames some of the
#'  values in the categorical variable columns
#'  * `fill_pairwise()`: in a hierarchical stack fills NA in one column based on
#'  the presence of data in another column
#'
#' @inheritParams shuffle_card
#' @param by (character) name of column(s) to use as header.
#' @param variables (character) `cards` variables
#' @param fill_from (character) Indicates when doing pair-wise filling whether
#' to fill from the column to the left. Defaults to `NULL`. Can be either `NULL`
#' or `"left"`.
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_card <- function(x,
                      by,
                      variables = NULL,
                      fill_overall = "Overall {colname}",
                      fill_hierarchical_overall = "Any {colname}",
                      fill_from = NULL) {

  # TODO prep_card only works with shuffled_cards
  # TODO class the output of shuffle_card()
  # and then shuffle if the object is not shuffled
  # TODO check the error is propagated from the right caller env
  # TODO check prep_card works both on its own and immediately following shuffle_
# browser()

  ard_args <- attr(x, "args")

  shuffled_card <- x

  if (!is_shuffled_card(x)) {
    shuffled_card <- shuffle_card(
      x,
      # message about switching the value for by
      by = ard_args$by %||% by,
      fill_overall = fill_overall,
      fill_hierarchical_overall = fill_hierarchical_overall
    )
  }

  if (!is.character(by)) {
    cli::cli_abort(
      "{.arg by} must be a character vector."
    )
  }

  # TODO get the logic to work with strings and then add support for symbols /
  # unquoted strings

  # TODO priority for extracting context - e.g. by variables, etc:
  #   1. direct passing of args
  #   2. from attributes
  #   3. tfrmt object

  # by <- rlang::enquo(by)

  # a <- tfrmt_find_args(..., env = environment(), parent_env = caller_env())
  # by_quo <- a$by |>
  #   rlang::quos_auto_name()
  # by_char <- names(by_quo)

  if (has_attributes(shuffled_card)) {
    shuffled_card <- shuffled_card |>
      # remove attributes for now
      # TODO add some logic to deal with them
      dplyr::filter(
        .data$context != "attributes"
      )
  }

  interim <- shuffled_card

  output <- interim |>
    unite_data_vars(by) |>
    # process_labels() |>
    process_big_n(by) |>
    process_categorical_vars(by) |>
    fill_pairwise(
      variables = ard_args$variables %||% variables,
      fill_hierarchical_overall = fill_hierarchical_overall,
      fill_from = fill_from
    )

  output
}

# does the shuffled card have the `args` attribute - allows us to extract some
# of the arguments of the original ard call
has_args <- function(x) {
  args <- attr(x, "args")

  output <- !rlang::is_empty(args)

  output
}

# replace_na with a given value (defaults to "Any <column-name>") or with values
# from the column to the left when the preceding column is not NA
fill_pairwise <- function(x,
                          variables,
                          fill_hierarchical_overall = "auto",
                          fill_from = NULL) {

  if (!rlang::is_character(fill_hierarchical_overall)) {
    cli::cli_abort(
      "{.arg fill_hierarchical_overall} must be a character."
    )
  }

  if (length(variables) < 2) {
    return(x)
  }

  if (fill_hierarchical_overall == "Any {colname}") {
    fill_hierarchical_overall <- "auto"
  }

  pair_list <- generate_pairs(variables)

  output <- x

  for (i in seq_along(pair_list)) {
    output <- replace_na_pair(
      output,
      pair = pair_list[[i]],
      fill_hierarchical_overall = fill_hierarchical_overall,
      fill_from = fill_from
    )
  }

  output
}

# fill_pairwise does pairwise conditional replacement of NAs. generate_pairs
# builds those pairs
generate_pairs <- function(x) {
  # TODO drop names
  output <- tibble::tibble(x = x) |>
    dplyr::mutate(
      x_lead = dplyr::lead(x)
    ) |>
    tidyr::drop_na() |>
    purrr::pmap(c)

  output
}

# replace missing values in one variable if a another variable is not NA
# this is the function used by fill_pairwise to iterate over the pairs of
# columns
replace_na_pair <- function(x,
                            pair,
                            fill_hierarchical_overall = "auto",
                            fill_from = NULL) {

  if (!is.null(fill_from) && fill_from != "left") {
    cli::cli_abort(
      '{.arg fill_from} must either be `NULL` or `"left"`'
    )
  }

  variables_syms <- rlang::syms(pair)

  if (fill_hierarchical_overall == "auto") {
    fill_hierarchical_overall <- glue::glue("Any {variables_syms[[2]]}") |>
      as.character()
  }

  if (!is.null(fill_from) && fill_from == "left") {
    fill_hierarchical_overall <- rlang::quo(as.character(!!variables_syms[[1]]))
  }

  output <- x |>
    dplyr::mutate(
      !!variables_syms[[2]] := dplyr::if_else(
        is.na(!!variables_syms[[2]]) & !is.na(!!variables_syms[[1]]),
        !!fill_hierarchical_overall,
        !!variables_syms[[2]]
      )
    )

  output
}

# `by` here is the same value as the `column` argument
# from `tfrmt(..., column = , ...)`
process_big_n <- function(x, by) {
  # browser()
  output <- x |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        .data$context == "total_n" ~ "bigN",
        # we only want to keep the subgroup totals, which get recoded to bigN
        .data$stat_variable %in% by & .data$stat_name == "n" ~ "bigN",
        # we only want the bigN for overall -> we remove "out"
        .data$stat_variable %in% by & .data$stat_name != "n" ~ "out",
        TRUE ~ .data$stat_name
      )
    ) |>
    dplyr::filter(
      .data$stat_name != "out"
    )

  output
}

# convenience wrapper around tidyr::unite to create variable_level from the
# individual columns (where applicable)
unite_data_vars <- function(x, by) {

  if ("hierarchical" %in% unique(x$context)) {
    return(x)
  }


  # browser()
  ard_vars <- c(
    "context",
    "stat_variable",
    "stat_name",
    "stat_label",
    "stat"
  )

  data_vars <- setdiff(names(x), c(ard_vars, by))

  interim <- x |>
    mutate(
      variable_level_coalesced = coalesce(
        !!!rlang::syms(data_vars)
      )
    ) |>
    tidyr::unite(
      col = "variable_level_untd",
      tidyselect::all_of(data_vars),
      na.rm = TRUE,
      remove = FALSE
    ) |>
    dplyr::mutate(
      variable_level_untd = dplyr::if_else(
        .data$variable_level_untd == "",
        NA_character_,
        .data$variable_level_untd
      )
    )

  # for ard_strata we expect there is a difference between unite and coalesce
  # and we shouldn't do it
  if (identical(interim$variable_level_untd, interim$variable_level_coalesced)) {
    output <- interim |>
      # drop the individual data columns and reorder the remaining ones
      dplyr::select(
        -tidyselect::all_of(
          data_vars
        ),
        -"variable_level_coalesced"
      ) |>
      dplyr::select(
        tidyselect::all_of(by),
        "stat_variable",
        "variable_level" = "variable_level_untd",
        tidyselect::everything()
      )
  } else {
    output <- x
  }

  # output <- x |>
  #   tidyr::unite(
  #     col = "variable_level",
  #     tidyselect::all_of(data_vars),
  #     na.rm = TRUE,
  #     remove = FALSE
  #   ) |>
  #   # drop the individual data columns and reorder the remaining ones
  #   dplyr::select(
  #     -tidyselect::all_of(
  #       data_vars
  #     )
  #   ) |>
  #   dplyr::select(
  #     tidyselect::all_of(by),
  #     "stat_variable",
  #     "variable_level",
  #     tidyselect::everything()
  #   )

  output
}



# mostly for compatibility with the current approach
# it derives and fills a new column, called label (most problematic for
# categorical variables)
process_categorical_vars <- function(x, by) {
  # browser()
  categorical_vars <- x |>
    dplyr::filter(
      .data$context == "categorical"
    ) |>
    dplyr::distinct(
      .data$stat_variable
    ) |>
    dplyr::pull() |>
    setdiff(by)

  if (rlang::is_empty(categorical_vars) || !"variable_level" %in% names(x)) {
    return(x)
  }

  output <- x |>
    dplyr::mutate(
      label = .data$stat_label,
      label = dplyr::if_else(
        .data$stat_name == "N",
        "n",
        .data$label
      )
    ) |>
    dplyr::mutate(
      variable_level = dplyr::if_else(
        .data$stat_name == "N",
        NA,
        .data$variable_level
      )
    ) |>
    unique() |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$context == "categorical" & .data$stat_name %in% c("n", "p"),
        .data$variable_level,
        .data$label
      ),
      # technically this transformation is not needed, the tfrmt table displays
      # correctly without it
      label = dplyr::if_else(
        .data$stat_name == "bigN" & .data$context == "categorical",
        !!rlang::sym(by),
        .data$label
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

# for use with labelled variables
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
      .data$context == "attributes",
      .data$stat_label == "Variable Label"
    ) |>
    dplyr::select(
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
