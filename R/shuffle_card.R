#' Shuffle `cards`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function ingests an ARD object and shuffles the information to prepare
#' for analysis. Helpful for streamlining across multiple ARDs.
#'
#' @param x an ARD data frame of class 'card'
#' @param by Grouping variable(s) used in calculations. Defaults to `NULL`. If
#'   available (i.e. if `x` comes from a stacking function), `attributes(x)$by`
#'   will be used instead of `by`.
#' @param trim logical representing whether or not to trim away `fmt_fun`,
#'   `error`, and `warning` columns
#' @param order_rows logical representing whether or not to apply
#'   `cards::tidy_ard_row_order()` to sort the rows
#' @param fill_overall scalar to fill missing grouping or variable levels. If a
#'   character is passed, then it is processed with [glue::glue()] where the
#'   colname element is available to inject into the string,
#'   e.g. `Overall {colname}` may resolve to `"Overall AGE"` for an `AGE`
#'   column. Default is `Overall {colname}`. If `NA` then no fill will occur.
#' @param fill_hierarchical_overall scalar to fill variable levels for overall
#'   hierarchical calculations. If a character is passed, then it is processed
#'   with [glue::glue()] where the colname element is available to inject into
#'   the string, e.g. `Any {colname}` may resolve to `"Any AESOC"` for an
#'   `AESOC` column. Default is `Any {colname}`. If `NA` then no fill will
#'   occur.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' cards::bind_ard(
#'   cards::ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1"),
#'   cards::ard_categorical(cards::ADSL, variables = "ARM")
#' ) |>
#'   shuffle_card()
#' }
shuffle_card <- function(x,
                         by = NULL,
                         trim = TRUE,
                         order_rows = TRUE,
                         fill_overall = "Overall {colname}",
                         fill_hierarchical_overall = "Any {colname}") {

  if (!requireNamespace("cards", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg cards} package must be installed to use {.fn shuffle_card}."
    )
  }

  if (!inherits(x, "card")) {
    cli::cli_abort(
      "{.arg x} argument must be class {.cls card}, not {.obj_type_friendly {x}}"
    )
  }
  if (!inherits(trim, "logical")) {
    cli::cli_abort(
      "{.arg trim} argument must be class {.cls logical}}, not \\
      {.obj_type_friendly {trim}}"
    )
  }

  ard_args <- attributes(x)$args
  by <- .process_by(x, by)

  # make sure columns are in order & add index for retaining order
  if (isTRUE(order_rows)){
    x <- x |>
      cards::tidy_ard_row_order()
  }
  dat_cards <- x |>
    cards::tidy_ard_column_order() |>
    dplyr::mutate(
      ..cards_idx.. = dplyr::row_number()
    )

  # split up the data into data/variable info & cards info
  vars_ard <- dat_cards |>
    dplyr::select(
      cards::all_ard_groups(),
      cards::all_ard_variables()
    ) |>
    names()

  # process the data/variable info
  dat_cards_grps_processed <- dat_cards |>
    dplyr::mutate(
      dplyr::across(
        c(
          cards::all_ard_groups("levels"),
          cards::all_ard_variables("levels")
        ),
        ~ lapply(., \(x) if (!is.null(x)) as.character(x))
      ),
      stat_variable = .data$variable
    ) |>
    dplyr::relocate(
      "stat_variable",
      .after = "context"
    ) |>
    cards::rename_ard_columns(fill = "..cards_overall..") |>
    dplyr::select(-any_of(c("..ard_total_n..", "..ard_hierarchical_overall..")))

  dat_cards_out <- dat_cards_grps_processed |>
    # unlist the list-columns
    cards::unlist_ard_columns() |>
    .fill_overall_grp_values(by, fill_overall, fill_hierarchical_overall) |>
    dplyr::arrange(.data$..cards_idx..) |>
    dplyr::select(-"..cards_idx..")

  output <- dat_cards_out

  if (trim) {
    output <- dat_cards_out |>
      .trim_ard()
  }

  # re-attach the args attribute
  attr(output, "args") <- ard_args

  output
}

#' Trim ARD
#'
#' This function ingests an data.frame and trims columns for
#' downstream use
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#'
#' @return a tibble
#' @keywords internal
.trim_ard <- function(x) {

  # detect any warning/error messages and notify user
  .detect_msgs(x, "warning", "error")
  # flatten ard table for easier viewing
  x |>
    dplyr::select(-any_of(c("fmt_fun", "fmt_fn","warning", "error")))
}


#' Detect Columns with Non-Null Contents
#'
#' Function looks for non-null contents in requested columns and notifies user
#' before removal. Specifically used for detecting messages.
#'
#' @param x a data frame
#' @param ... columns to search within
#'
#' @keywords internal
.detect_msgs <- function(x, ...) {
  dots <- rlang::dots_list(...)

  lapply(dots, function(var) {
    if (any(!map_lgl(x[[var]], is.null))) {
      cli::cli_inform(
        "{.val {var}} column contains messages that will be removed."
      )
    }
  })
}


#' Process `by` variable
#'
#' @param x a data frame
#' @param by Grouping variable(s) used in calculations. Defaults to `NULL`.
#'
#' @returns character string if `by` variable present
#' @keywords internal
.process_by <- function(x, by){

  ard_attributes <- attributes(x)
  ard_args <- ard_attributes$args
  if (!is.null(by)){
    if (!is.null(ard_args$by) && !identical(ard_args$by, by)){
      cli::cli_inform(
        "Mismatch between attributes of {.arg x} and supplied value to \\
        {.arg by}. Attributes will be used in lieu of {.arg by}",
        env = rlang::caller_env())
    } else {
      ard_args$by <- by
    }
  }

  ard_args$by
}
#' Fill Overall Group Variables
#'
#' This function fills the missing values of grouping variables with
#' `"Overall <variable_name>"` or `"Any <variable_name>"`where relevant.
#' Specifically, it will modify grouping values from rows with likely overall
#' calculations present (e.g. non-missing variable/variable_level, missing group
#' variables, and evidence that the `variable` has been computed by group in
#' other rows). `"Overall"` values will be populated only for grouping variables
#' that have been used in other calculations of the same variable and statistics.
#' `"Any"` will be used if it is likely to be a hierarchical calculation.
#'
#' @inheritParams shuffle_card
#'
#' @return data frame
#'
#' @keywords internal
.fill_overall_grp_values <- function(x,
                                     by,
                                     fill_overall,
                                     fill_hierarchical_overall) {

  grp_vars <- by
  vars_cards_protected <- c(
    "context",
    "stat_variable",
    "stat_name",
    "stat_label",
    "stat",
    "fmt_fun",
    "fmt_fn",
    "warning",
    "error",
    "..cards_idx.."
  )

  # determine grouping and merging variables
  id_vars <- setdiff(names(x), unique(c(vars_cards_protected, grp_vars)))

  if (!is_empty(grp_vars) && !is_empty(id_vars)) {

    # replace NA group values with "..cards_overall.." where it is likely to be
    # an overall calculation
    for (g in grp_vars) {

      # rows with missing group
      x_missing_by <- x |>
        dplyr::filter(is.na(.data[[g]]))

      # rows with non-missing group
      x_nonmissing_by <- x |>
        dplyr::filter(!is.na(.data[[g]]) & !.data[[g]] == "..cards_overall..")

      if (nrow(x_missing_by) > 0 && nrow(x_nonmissing_by) > 0) {
        x_missing_by_replaced <- x_missing_by |>
          dplyr::rows_update(
            x_nonmissing_by |>
              dplyr::mutate(
                !!g := dplyr::if_else(
                  !is.na(.data[[g]]),
                  "..cards_overall..",
                  .data[[g]]
                )
              ) |>
              dplyr::select(
                -dplyr::any_of(
                  c(
                    setdiff(
                      names(x),
                      c(g, id_vars)
                    )
                  )
                )
              ) |>
              dplyr::distinct(),
            by = id_vars,
            unmatched = "ignore"
          )

        x <- dplyr::rows_update(
          x,
          x_missing_by_replaced,
          by = "..cards_idx.."
        )
      }
    }

    # replace NA variables with "..hierarchical_overall.." when present
    x <- x |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(
            id_vars
          ),
          ~ ifelse(
            is.na(.x) & .data$stat_variable == "..ard_hierarchical_overall..",
            "..hierarchical_overall..",
            .x
          )
        )
      )
  }

  # replace `"..cards_overall.."` and `"..hierarchical_overall.."` with fill
  # values
  output <- x |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(
          c(
            grp_vars,
            setdiff(
              id_vars,
              "..cards_idx.."
            )
          )
        ),
        ~ .derive_overall_labels(
          .x,
          colname = dplyr::cur_column(),
          fill_overall,
          fill_hierarchical_overall
        )
      )
    )

  output
}

#' Derive overall labels
#'
#' Transform the `"..cards_overall.."` and `"..hierarchical_overall.."` labels
#' into `"Overall <variable_name>"` and `"Any <variable_name>"` respectively.
#' Also it ensures the labels are unique (in case they already exist) with
#' `make.unique()` which appends a sequence number.
#'
#' @param x (character) content of target (current) column
#' @param colname (character) name of current column
#' @inheritParams shuffle_card
#'
#' @returns a character vector
#'
#' @keywords internal
.derive_overall_labels <- function(x,
                                   colname = dplyr::cur_column(),
                                   fill_overall,
                                   fill_hierarchical_overall) {

  glue_overall <- ifelse(
    is.na(fill_overall),
    NA,
    glue::glue(fill_overall)
  )
  glue_any <- ifelse(
    is.na(fill_hierarchical_overall),
    NA,
    glue::glue(fill_hierarchical_overall)
  )

  overall_val <- c(unique(x), glue_overall) |>
    make.unique() |>
    dplyr::last()
  any_val <- c(unique(x), glue_any) |>
    make.unique() |>
    dplyr::last()

  if (!is.na(glue_overall) && overall_val != glue_overall) {
    cli::cli_alert_info(
      "{.val {glue_overall}} already exists in the {.code {colname}} column. \\
      Using {.val {overall_val}}."
    )
  }

  if (!is.na(glue_any) && any_val != glue_any) {
    cli::cli_alert_info(
      "{.val {glue_any}} already exists in the {.code {colname}} column. Using\\
       {.val {any_val}}."
    )
  }

  output <- dplyr::case_when(
    x == "..cards_overall.." ~ overall_val,
    x == "..hierarchical_overall.." ~ any_val,
    TRUE ~ x
  )

  output
}
