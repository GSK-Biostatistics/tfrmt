#' Shuffle {cards}
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function ingests an ARD object and shuffles the information to prepare for analysis.
#' Helpful for streamlining across multiple ARDs. Combines each group/group_level into 1
#' column, back fills missing grouping values from the variable levels where possible, and
#' optionally trims statistics-level metadata.
#'
#' @param x an ARD data frame of class 'card'
#' @param trim logical representing whether or not to trim away statistic-level metadata
#'
#' @return a tibble
#' @export
#'
#' @examples
#' cards::bind_ard(
#'   cards::ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
#'   cards::ard_categorical(ADSL, variables = "ARM")
#' ) |>
#'   shuffle_card()
shuffle_card <- function(x, by = NULL, trim = TRUE) {

  stopifnot(inherits(x, "card")) # switch to cli
  stopifnot(inherits(trim, "logical"))  # switch to cli

  ard_attributes <- attributes(x)
  ard_args <- ard_attributes$args
  if (!is.null(by)){
    ard_args$by <- by
  }

  # make sure columns are in order & add index for retaining order
  dat_cards <- x |>
    cards::tidy_ard_column_order() |>
    cards::tidy_ard_row_order() |>
    dplyr::mutate(..cards_idx.. = dplyr::row_number())

  # fill stat label if missing
  dat_cards <- dat_cards |>
    dplyr::mutate(dplyr::across(any_of("stat_label"), ~ dplyr::coalesce(.x, stat_name)))

  # split up the data into data/variable info & cards info
  vars_ard <- dat_cards |>
    dplyr::select(cards::all_ard_groups(), cards::all_ard_variables()) |>
    names()

  vars_protected <- setdiff(names(dat_cards), vars_ard)

  # process the data/variable info
  dat_cards_grps_processed <- dat_cards |>
    dplyr::mutate(
      dplyr::across(
        c(cards::all_ard_groups("levels"), cards::all_ard_variables("levels")),
        ~ lapply(., \(x) if (!is.null(x)) as.character(x) else NA_character_)
      )) |>
    cards::rename_ard_columns(fill = "..cards_overall..")


  dat_cards_out <- dat_cards_grps_processed |>
    # unlist the list-columns
    cards::unlist_ard_columns() |>
    .coalesce_ard_vars() |>
    .fill_overall_grp_values(vars_protected, ard_args) |>
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


#' Coalesce protected `..ard_*` variables into a single column, `..ard_vars..`
#'
#' @param x a data frame
#'
#' @return a tibble
#' @noRd
.coalesce_ard_vars <- function(x){

  ard_vars <- intersect(c("..ard_total_n..", "..ard_hierarchical_overall.."), names(x))
  if (length(ard_vars)>0){
    x <- x |>
      dplyr::mutate(across(all_of(ard_vars), ~ ifelse(!is.na(.x), dplyr::cur_column(), .x))) |>
      dplyr::mutate(..ard_vars.. = do.call(dplyr::coalesce, across(all_of(ard_vars))))|>
      dplyr::select(-all_of(ard_vars))
  } else {
    x <- x |>
      dplyr::mutate(..ard_vars.. = NA)
  }

  x  |>
    dplyr::relocate("..ard_vars..", .before = "..cards_idx..")

}

#' Trim ARD
#'
#' This function ingests an ARD object and trims columns and rows for downstream use in
#' displays. The resulting data frame contains only numeric results, no supplemental
#' information about errors/warnings, and unnested list columns.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#'
#' @return a tibble
#' @noRd
.trim_ard <- function(x) {

  # detect any warning/error messages and notify user
  .detect_msgs(x, "warning", "error")
  # flatten ard table for easier viewing ---------------------------------------
  x |>
    dplyr::select(-c("fmt_fun", "warning", "error"))
}


#' Detect Columns with Non-Null Contents
#'
#' Function looks for non-null contents in requested columns and notifies user
#' before removal. Specifically used for detecting messages.
#'
#' @param x a data frame
#' @param ... columns to search within
#'
#' @noRd
.detect_msgs <- function(x, ...) {
  dots <- rlang::dots_list(...)

  lapply(dots, function(var) {
    if (any(!map_lgl(x[[var]], is.null))) {
      cli::cli_inform("{.val {var}} column contains messages that will be removed.")
    }
  })
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
#' @param x a data frame
#' @param ard_args list of args passed from ard_* calls
#'
#' @return data frame
.fill_overall_grp_values <- function(x, vars_protected, ard_args) {

  # determine grouping and merging variables
  grp_vars <- ard_args$by
  id_vars <- setdiff(names(x), unique(c(vars_protected, grp_vars, "..ard_vars..")))

  if (!is_empty(grp_vars)){

    # replace NA group values with "..cards_overall.." where it is likely to be an overall calculation
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
              dplyr::mutate(!!g := ifelse(!is.na(.data[[g]]), "..cards_overall..", .data[[g]])) |>
              dplyr::select(-any_of(c(setdiff(names(x), c(g, id_vars))))) |>
              dplyr::distinct(),
            by = id_vars,
            unmatched = "ignore"
          )

        x <- dplyr::rows_update(x, x_missing_by_replaced, by = "..cards_idx..")
      }
    }

    # replace NA group values with "..cards_overall.." or "..hierarchical_overall.."
    # where it is likely to be a group or subgroup calculation
    x <- x |>
      dplyr::mutate(across(all_of(grp_vars),
                           ~ifelse(is.na(.x),
                                   ..ard_vars..,
                                   .x)))|>
      dplyr::mutate(across(all_of(id_vars),
                           ~ifelse(is.na(.x) & .data$..ard_vars.. == "..ard_hierarchical_overall..",
                                   "..hierarchical_overall..",
                                   .x)))
  }

  # replace `"..cards_overall.."` group values with "Overall <colname>" and
  # `"..hierarchical_overall.."` with `"Any <colname>"`
  output <- x |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(
          c(grp_vars, id_vars)
        ),
        .derive_overall_labels
      )
    ) |>
    dplyr::select(-any_of("..ard_vars.."))

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
#' @param cur_col (character) name of current column
#'
#' @returns a character vector
#'
#' @noRd
.derive_overall_labels <- function(x, cur_col = dplyr::cur_column()) {
  glue_overall <- glue::glue("Overall {cur_col}")
  glue_any <- glue::glue("Any {cur_col}")

  overall_val <- c(unique(x), glue_overall) |>
    make.unique() |>
    dplyr::last()
  any_val <- c(unique(x), glue_any) |>
    make.unique() |>
    dplyr::last()

  if (overall_val != glue_overall) {
    cli::cli_alert_info(
      "{.val {glue_overall}} already exists in the {.code {cur_col}} column. \\
      Using {.val {overall_val}}."
    )
  }

  if (any_val != glue_any) {
    cli::cli_alert_info(
      "{.val {glue_any}} already exists in the {.code {cur_col}} column. Using\\
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
