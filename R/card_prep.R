#' Prepare card for tfrmt
#'
#' What does the preparation function need to do?
#'  * `unite_data_vars()`: bring all data variables into a single column
#'  (`variable`) and their levels into `variable_level`
#'  * `prepare_big_n()`:
#'
#'
#' @param x a shuffled `card` object.
#' @param column column to use as header.
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x, column = NULL) {

  # TODO priority for extracting context - e.g. by variables, etc:
  #   1. direct passing of args
  #   2. from attributes
  #   3. tfrmt object

  column <- rlang::ensym(column)

  output <- x |>
    unite_data_vars(column) |>
    # process_labels() |>
    # process big N by (header) column, not grouping variables
    process_big_n(column) |>
    dplyr::select(
      all_of(column),
      # use of .data in tidyselect expressions deprecated in tidyselect 1.2.0.
      # we need to use `"variable"` instead of `.data$variable`
      "variable",
      "label",
      "stat_name",
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

# `column` here is the same value as the `column` argument
# from `tfrmt(..., column = , ...)`
process_big_n <- function(x, column = NULL) {

  # this corresponds to bigN structure

  if (is.null(column)) {
    return(x)
  }

  if (length(column) != 1) {
    stop(
      "`process_big_n` supports a single column.",
      call. = FALSE
    )
  }

  ard_vars <- c(
    "..ard_vars..",
    "context",
    "stat_name",
    "stat_label",
    "stat",
    "variable_label"
  )

  data_vars <- setdiff(names(x), c(ard_vars, column))

  # TODO move this outside process_big_n
  # df_united_vars <- unite_data_vars(x, column, data_vars)

  # browser()
  output <- x |>
    dplyr::mutate(
      stat_name = dplyr::case_when(
        .data$..ard_vars.. == "..ard_total_n.." ~ "bigN",
        # we only want to keep the subgroup totals, which get recoded to bigN
        is.na(.data$variable) & .data$stat_name == "n" ~ "bigN",
        is.na(.data$variable) & .data$stat_name != "n" ~ "out",
        TRUE ~ stat_name
      )
    ) |>
    # we only want the bigN for overall
    dplyr::filter(stat_name != "out") |>
    dplyr::mutate(
      !!column := dplyr::case_when(
        .data$..ard_vars.. == "..ard_total_n.." ~ glue::glue("Overall {column}"),
        is.na(!!column) & variable_level == "..cards_overall.." ~ glue::glue("Overall {column}"),
        TRUE ~ !!column
      )
    ) |>
    dplyr::select(-`..ard_vars..`) |>
    # we need "..cards_overall.." for the above `case_when()`
    dplyr::mutate(
      # we overwrite "..card_overall.." (present for continuous variables)
      # variable_level with the variable name
      variable_level = dplyr::if_else(
        context == "continuous",
        variable,
        variable_level
      )
    ) |>
    dplyr::mutate(
      !!column := dplyr::if_else(
        is.na(!!column) & !is.na(variable),
        glue::glue("Overall {column}"),
        !!column
      ),
      !!column := as.character(!!column)
    ) |>
    dplyr::mutate(label = stat_label) |>
    dplyr::mutate(
      label = dplyr::case_when(
        .data$stat_name == "N" ~ "n",
        # for identity with the current approach that has the `column` levels
        # in label
        .data$stat_name == "bigN" & .data$context != "total_n" ~ !!column,
        TRUE ~ .data$label
      ),
      label = as.character(label)
    ) |>
    # subgroup totals (n) are displayed once per variable - they are grouping
    # variable specific, not variable level specific
    # unique() will remove duplicates
    dplyr::mutate(
      variable_level = dplyr::if_else(
        .data$stat_name == "N",
        NA,
        .data$variable_level
      )
    ) |>
    # we "need"/ use variable_label in prep_tfrmt
    # select(-all_of(data_vars), -variable_label)
    unique()

  # derive label
  output1 <- output |>
    dplyr::mutate(
      label = dplyr::if_else(
        context == "categorical" & stat_name %in% c("n", "p"),
        variable_level,
        label
      )
    ) |>
    dplyr::select(-variable_level) |>
    dplyr::mutate(
      variable = dplyr::if_else(
        is.na(variable),
        rlang::as_name(column),
        variable
      )
    )

  output1
}

unite_data_vars <- function(x, column) {

  ard_vars <- c(
    "..ard_vars..",
    "context",
    "stat_name",
    "stat_label",
    "stat",
    "variable_label"
  )

  data_vars <- setdiff(names(x), c(ard_vars, column))

  .capture_var_name <- function(x, cur_col = dplyr::cur_column()) {
    output <- dplyr::if_else(
      !is.na(x),
      cur_col,
      NA_character_
    )

    output
  }

  output <- x |>
    tidyr::unite(
      col = "variable_level",
      tidyselect::all_of(data_vars),
      na.rm = TRUE,
      remove = FALSE
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(data_vars), .capture_var_name
      )
    ) |>
    dplyr::mutate(
      variable = dplyr::coalesce(
        !!!rlang::syms(data_vars)
      )
    ) |>
    dplyr::select(
      -tidyselect::all_of(
        data_vars
      )
    ) |>
    dplyr::select(
      tidyselect::all_of(column),
      variable,
      variable_level,
      tidyselect::everything()
    )

  output
}

# some basic ordering. order_plan can always re-order.
order_rows_n_first <- function(x) {
  output <- x |>
    mutate(
      ord1 = forcats::fct_inorder(variable),
      ord2 = if_else(label == "n", 1, 2)
    ) |>
    arrange(ord1, ord2) |>
    select(-ord1, -ord2)

  output
}
