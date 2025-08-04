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
# browser()
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

fill_column <- function(x, column) {
  browser()

  column_sym <- rlang::sym(column)

  output <- x |> dplyr::mutate(
    !!column_sym := tidyr::replace_na(
      !!column_sym,
      glue::glue("Overall {column}")
    ),
    !!column := as.character(!!column_sym)
  )

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

process_big_n_hierarchical <- function(x, column) {
  # browser()
  ard_vars <- c(
    "context",
    "stat_variable",
    "stat_name",
    "stat_label",
    "stat"
  )

  data_vars <- setdiff(names(x), c(ard_vars, column))

  output <- x |>
    mutate(
      stat_name = case_when(
        .data$context == "total_n" ~ "bigN",
        # we only want to keep the subgroup totals, which get recoded to bigN
        .data$stat_variable %in% column & .data$stat_name == "n" ~ "bigN",
        .data$stat_variable %in% column & .data$stat_name != "n" ~ "out",
        .default = stat_name
      )
    ) |>
    dplyr::filter(stat_name != "out") |>
    select(
      all_of(column),
      all_of(data_vars),
      stat_name,
      stat
    ) |>
    mutate(
      AETERM = dplyr::if_else(
        is.na(AETERM) & stat_name != "bigN",
        "Any AETERM",
        AETERM
      )
    )

  output
}

# `column` here is the same value as the `column` argument
# from `tfrmt(..., column = , ...)`
process_big_n <- function(x, column) {
# browser()

  # in some cases we need to unite / coalesce before we can proceed to prepping
  # bigN


  output <- x |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        .data$context == "total_n" ~ "bigN",
        # we only want to keep the subgroup totals, which get recoded to bigN
        # .data$stat_variable == column & .data$stat_name == "n" ~ "bigN",
        .data$stat_variable %in% column & .data$stat_name == "n" ~ "bigN",
        # we only want the bigN for overall -> we remove "out"
        .data$stat_variable %in% column & .data$stat_name != "n" ~ "out",
        TRUE ~ stat_name
      )
    ) |>
    dplyr::filter(stat_name != "out") #|>
    # dplyr::mutate(
    #   !!column := tidyr::replace_na(
    #     !!column,
    #     "Total"
    #   )
    # ) |>
    # dplyr::mutate(label = stat_label) |>
    # dplyr::mutate(
    #   label = dplyr::case_when(
    #     .data$stat_name == "N" ~ "n",
    #     # for identity with the current approach that has the `column` levels
    #     # in label
    #     .data$stat_name == "bigN" & .data$context != "total_n" ~ !!column,
    #     TRUE ~ .data$label
    #   )
    # ) |>
    # # subgroup totals (n) are displayed once per variable - they are grouping
    # # variable specific, not variable level specific
    # # unique() will remove duplicates
    # dplyr::mutate(
    #   variable_level = dplyr::if_else(
    #     .data$stat_name == "N",
    #     NA,
    #     .data$variable_level
    #   )
    # ) |>
    # unique()

  # enhance label
  # output <- interim |>
  #   dplyr::mutate(
  #     label = dplyr::if_else(
  #       context == "categorical" & stat_name %in% c("n", "p"),
  #       variable_level,
  #       label
  #     )
  #   ) |>
  #   dplyr::select(-variable_level) |>
    # dplyr::select(
    #   all_of(column),
    #   # use of .data in tidyselect expressions deprecated in tidyselect 1.2.0.
    #   # we need to use `"variable"` instead of `.data$variable`
    #   # "variable",
    #   "stat_variable",
    #   # "label",
    #   "stat_name",
    #   "stat_label",
    #   "stat",
    #   "context"
    # )
  #   order_rows_n_first()

  output
  # interim
}


unite_data_vars <- function(x, column) {
# browser()

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

# some basic ordering. order_plan can always re-order.
order_rows_n_first <- function(x) {
  output <- x |>
    mutate(
      ord1 = forcats::fct_inorder(stat_variable),
      ord2 = if_else(label == "n", 1, 2)
    ) |>
    arrange(ord1, ord2) |>
    select(-ord1, -ord2)

  output
}

process_categorical_vars <- function(x, column) {

  browser()

  categorical_vars <- x |>
    dplyr::filter(context == "categorical") |>
    dplyr::distinct(stat_variable) |>
    dplyr::pull() |>
    setdiff(column)

  if (rlang::is_empty(categorical_vars)) {
    return(x)
  }

  output <- x |>
    mutate(
      label = stat_label,
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
        context == "categorical" & stat_name %in% c("n", "p"),
        variable_level,
        label
      )
    )

  output
}
