#' Prepare card for tfrmt
#'
#' What does the preparation function do?
#'   * `unite_data_vars()`: bring all categorical variables levels into a single
#'     column, called `variable_level` (where applicable).
#'  * `process_big_n()` identifies the bigN columns and changes `stat_name` to
#'  `"bigN"`
#'  * `process_categorical_vars()`: once we have bigN, it renames some of the
#'  values in the categorical variable columns
#'  * `fill_variables()`: in a hierarchical stack fills NA in one column
#'  (with `"Any <column_name>"`) based on the presence of data in another column
#'
#' @inheritParams shuffle_card
#' @param column column(s) to use as header.
#' @param variables `cards` variables
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x, column, variables) {

  browser()

  ard_args <- attr(x, "args")

  shuffled_card <- shuffle_card(
    x,
    by = ard_args$by %||% column,
    variables = ard_args$variables %||% variables
  )

  if (!is.character(column)) {
    cli::cli_abort(
      "{.arg column} must be a character vector."
    )
  }

  # TODO get the logic to work with strings and then add support for symbols /
  # unquoted strings

  # TODO priority for extracting context - e.g. by variables, etc:
  #   1. direct passing of args
  #   2. from attributes
  #   3. tfrmt object

  # column <- rlang::enquo(column)

  # a <- tfrmt_find_args(..., env = environment(), parent_env = caller_env())
  # columns_quo <- a$column |>
  #   rlang::quos_auto_name()
  # columns_char <- names(columns_quo)

  if (has_attributes(x)) {
    x <- x |>
      # remove attributes for now
      # TODO add some logic to deal with them
      dplyr::filter(
        .data$context != "attributes"
      )
  }

  interim <- x

  # don't unite for hierarchical stack
  if (!"hierarchical" %in% unique(x$context)) {
    interim <- unite_data_vars(x, column)
  }

  output <- interim |>
    # process_labels() |>
    process_big_n(column) |>
    process_categorical_vars(column)

  if (has_args(x)) {
    variables <- attr(x, "args")[["variables"]]
    # TODO fill_variables could be connected to the "hierarchical" logic above
    # (we are interested in pairwise conditional filling primarily in a
    # hierarchical context)
    output <- fill_variables(output, variables = variables)
  }

  output
}

# does the shuffled card have the `args` attribute - allows us to extract some
# of the arguments of the original ard call
has_args <- function(x) {
  args <- attr(x, "args")

  output <- !rlang::is_empty(args)

  output
}

# replace_na with "Any <column-name>" where applicable (where the preceding
# column) is not NA
fill_variables <- function(x, variables) {

  if (length(variables) < 2) {
    return(x)
  }

  pair_list <- generate_pairs(variables)

  output <- x

  # TODO need to test if the approach works for more than 2 variables
  for (i in seq_along(pair_list)) {
    output <- replace_na_pair(output, pair_list[[i]])
  }

  output
}

# fill_variables does pairwise conditional replacement of NAs. generate_pairs
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
# this is the function used by fill_variables to iterate over the pairs of
# columns
replace_na_pair <- function(x, pair) {

  variables_syms <- rlang::syms(pair)

  output <- x |>
    dplyr::mutate(
      !!variables_syms[[2]] := dplyr::if_else(
        is.na(!!variables_syms[[2]]) & !is.na(!!variables_syms[[1]]),
        glue::glue("Any {variables_syms[[2]]}"),
        !!variables_syms[[2]]
      ),
      !!variables_syms[[2]] := as.character(!!variables_syms[[2]])
    )

  output
}

# `column` here is the same value as the `column` argument
# from `tfrmt(..., column = , ...)`
process_big_n <- function(x, column) {

  output <- x |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        .data$context == "total_n" ~ "bigN",
        # we only want to keep the subgroup totals, which get recoded to bigN
        .data$stat_variable %in% column & .data$stat_name == "n" ~ "bigN",
        # we only want the bigN for overall -> we remove "out"
        .data$stat_variable %in% column & .data$stat_name != "n" ~ "out",
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
unite_data_vars <- function(x, column) {

  ard_vars <- c(
    "context",
    "stat_variable",
    "stat_name",
    "stat_label",
    "stat"
  )

  data_vars <- setdiff(names(x), c(ard_vars, column))

  output <- x |>
    tidyr::unite(
      col = "variable_level",
      tidyselect::all_of(data_vars),
      na.rm = TRUE,
      remove = FALSE
    ) |>
    # drop the individual data columns and reorder the remaining ones
    dplyr::select(
      -tidyselect::all_of(
        data_vars
      )
    ) |>
    dplyr::select(
      tidyselect::all_of(column),
      "stat_variable",
      "variable_level",
      tidyselect::everything()
    )

  output
}

# mostly for compatibility with the current approach
# it derives and fills a new column, called label (most problematic for
# categorical variables)
process_categorical_vars <- function(x, column) {

  categorical_vars <- x |>
    dplyr::filter(
      .data$context == "categorical"
    ) |>
    dplyr::distinct(
      .data$stat_variable
    ) |>
    dplyr::pull() |>
    setdiff(column)

  if (rlang::is_empty(categorical_vars)) {
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
        !!rlang::sym(column),
        .data$label
      )
    )

  output
}

# does the shuffled card have attributes (useful for ensuring column labels are
# persistent)
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
