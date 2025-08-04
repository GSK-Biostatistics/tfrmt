#' Prepare card for tfrmt
#'
#' What does the preparation function need to do?
#'  * `unite_data_vars()`: bring all data variables into a single column
#'  (`variable`) and their levels into `variable_level`
#'  * `prepare_big_n()`:
#'
#'
#' @param x a shuffled `card` object.
#' @param column column(s) to use as header.
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x, column) {

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
      dplyr::filter(context != "attributes")
  }

  we_need_to_unite <- TRUE

  # we don't need unite for hierarchical stack
  if ("hierarchical" %in% unique(x$context)) {
    we_need_to_unite <- FALSE
  }

  interim <- x

  if (we_need_to_unite) {
    interim <- unite_data_vars(x, column)
  }

  output <- interim |>
    fill_column(column) |>
    # process_labels() |>
    # process big N by (header) column, not grouping variables
    process_big_n(column) |>
    process_categorical_vars(column)

  if (has_args(x)) {
    variables <- attr(x, "args")[["variables"]]
    output <- fill_variables(output, variables = variables)
  }

  output
}


has_attributes <- function(x) {
  shuffled_card_attributes_df <- x |>
    dplyr::filter(
      .data$context == "attributes"
    )

  output <- nrow(shuffled_card_attributes_df) > 0

  output
}

has_args <- function(x) {
  args <- attr(x, "args")

  output <- !rlang::is_empty(args)

  output
}

# replace_na in the main column(s) with "Overall <column_name>"
fill_column <- function(x, column) {

  replacement_vals <- column |>
    purrr::set_names(column) |>
    purrr::map(~ glue::glue("Overall {.x}"))

  output <- tidyr::replace_na(x, replacement_vals)

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

generate_pairs <- function(x) {
  output <- tibble::tibble(x = x) |>
    dplyr::mutate(
      x_lead = dplyr::lead(x)
    ) |>
    na.omit() |>
    purrr::pmap(c)

  output
}

# replace missing values in one variable if a another variable is not NA
replace_na_pair <- function(x, pair) {

  variables_syms <- rlang::syms(pair)

  output <- x |>
    mutate(
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
      stat_variable,
      variable_level,
      tidyselect::everything()
    )

  output
}

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


# for use with labelled variables
process_labels <- function(x) {

  if (!has_attributes(x)) {
    output <- x |>
      dplyr::mutate(
        variable_label = NA_character_
      )

    return(output)
  }

  # TODO this does not work in this current context as we do not have variable
  # so the 2 branches effectively return different objects

  variable_labels <- x |>
    dplyr::filter(
      .data$context == "attributes",
      .data$stat_label == "Variable Label"
    ) |>
    dplyr::select(
      # Use of .data in tidyselect expressions deprecated in tidyselect 1.2.0.
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
