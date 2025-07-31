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
#' @param variables (character) variables.
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x, column = NULL, variables = NULL) {

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

  output1 <- x |>
    # process_labels() |>
    # process big N by (header) column, not grouping variables
    process_big_n(column = column)

  # browser()

  output2 <- output1 |>
    # dplyr::mutate(
    #   variable = dplyr::coalesce(
    #     .data$variable_label,
    #     .data$variable
    #   )
    # ) |>
    # unique() |>
    dplyr::select(
      # before
      all_of(column),
      # needed for ae_t04
      # use of .data in tidyselect expressions deprecated in tidyselect 1.2.0.
      # we need to use `"variable"` instead of `.data$variable`
      "variable",
      "label",
      "stat_name",
      "stat"
    ) |>
    order_rows_n_first()

  output2
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
browser()
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
# browser()
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

  # TODO pass column as symbol
  col_sym <- rlang::sym(column)

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
  df_united_vars <- unite_data_vars(x, col_sym, data_vars)

  # browser()
  output <- df_united_vars |>
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
      !!col_sym := dplyr::case_when(
        .data$..ard_vars.. == "..ard_total_n.." ~ glue::glue("Overall {col_sym}"),
        is.na(!!col_sym) & variable_level == "..cards_overall.." ~ glue::glue("Overall {col_sym}"),
        TRUE ~ !!col_sym
      )
    ) |>
    dplyr::select(-`..ard_vars..`) |>
    # we need "..cards_overall.." for the above `case_when()`
    dplyr::mutate(
      # we overwrite "..card_overal.." (present for continuous variables)
      # variable_level with the variable name
      variable_level = dplyr::if_else(
        context == "continuous",
        variable,
        variable_level
      )
    ) |>
    dplyr::mutate(
      !!col_sym := dplyr::if_else(
        is.na(!!col_sym) & !is.na(variable),
        glue::glue("Overall {col_sym}"),
        !!col_sym
      ),
      !!col_sym := as.character(!!col_sym)
    ) |>
    dplyr::mutate(label = stat_label) |>
    dplyr::mutate(
      label = dplyr::case_when(
        .data$stat_name == "N" ~ "n",
        # for identity with the current approach that has the `column` levels
        # in label
        .data$stat_name == "bigN" & .data$context != "total_n" ~ !!col_sym,
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
        rlang::as_name(col_sym),
        variable
      )
    )

  output1
}

unite_data_vars <- function(x, col_sym, data_vars) {

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
      tidyselect::all_of(col_sym),
      variable,
      variable_level,
      tidyselect::everything()
    )

  output
}

# some basic ordering. order_plan can alway re-order.
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
