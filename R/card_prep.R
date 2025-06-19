#' Prepare card for tfrmt
#'
#' @param x (card) card object
#'
#' @returns a `data.frame`
#' @export
#'
#' @examples
prep_tfrmt <- function(x) {
  browser()

  card_metadata <- extract_card_metadata(x)

  interim_x <- x |>
    shuffle_ard(trim = TRUE)

  interim_x |>
    # derive `label`
    mutate(
      label = coalesce(variable_level, stat_label)
    ) |>
    mutate(
      by_var = if_else(
        # variable == "ARM",
        # TODO can we have multiple grouping variables? If yes, handle the
        # situation when card_metadata$grouping_variables has more than 1 var
        variable == card_metadata$grouping_variables,
        label,
        # ARM
        !!sym(card_metadata$grouping_variables)
      ),
      by_var = if_else(
        is.na(by_var) | variable == "..ard_total_n..",
        "Total",
        by_var
      )
    ) |>
    mutate(
      stat_name2 = if_else(
        (variable == "ARM" & stat_name == "n") | variable == "..ard_total_n..",
        "bigN",
        stat_name
      ),
      stat_name3 = case_when(
        variable == "ARM" & stat_name == "n" ~ "bigN",
        variable == "..ard_total_n.." ~ "bigN",
        TRUE ~ stat_name
      ),
      label = ifelse(stat_name=="N", "n", label)
    ) |>
    # filter(!(variable=="ARM" & stat_name !="bigN")) |>
    View()
  mutate(
    # if seems like this coalescence is taking place almost always (when we have
    # continuous variables)
    label = if_else(is.na(label), stat_label, label),

    # Add big N labels
    ARM = if_else(variable == "ARM", label, ARM),
    # ID total trt
    ARM = ifelse(is.na(ARM) | variable == "..ard_total_n..",
                 "Total", ARM),
    # unique stat names for big N's
    stat_name = if_else((variable=="ARM" & stat_name=="n") | variable=="..ard_total_n..","bigN", stat_name),
    # relabel the label for n's
    label = ifelse(stat_name=="N", "n", label)
  )
}

#' Extract card metadata
#'
#' `extract_card_metadata()` reverse engineers the `ard_stack()` call based on
#' its output. It extracts the grouping variable, continuous and categorical
#' variables and their statistics.
#'
#' @param x (card) card object
#'
#' @returns a list made up of 5 character vectors:
#'   * `grouping_variables`: names of the grouping variables
#'   * `continuous_variables`
#'   * `categorical_variables`
#'   * `continuous_variables_stats`
#'   * `categorical_variables_stats`
#'
#' @export
#'
#' @examples
extract_card_metadata <- function(x) {

  # TODO this likely needs quite a bit of refining

  # extract metadata (`.by`, variables, and stats) from the card object
  # .by
  ard_by_var <- extract_grouping_variables(x)

  # continuous variables
  continuous_variables <- x |>
    filter(
      context == "continuous"
    ) |>
    distinct(variable) |>
    pull(variable)

  # categorical variables
  categorical_variables <- x |>
    filter(
      context == "categorical"
    ) |>
    distinct(variable) |>
    pull(variable)

  # categorical and continuous variables stats
  categorical_stats <- c("n", "p")
  common_stats <- "N"

  ard_stats_cont_vars <- x$stat_name |>
    unique() |>
    setdiff(categorical_stats)

  ard_stats_cat_vars <- x$stat_name |>
    unique() |>
    setdiff(ard_stats_cont_vars) |>
    append(common_stats, after = 0L)

  output <- list(
    grouping_variables = ard_by_var,
    continuous_variables = continuous_variables,
    categorical_variables = categorical_variables,
    continuous_variables_stats = ard_stats_cont_vars,
    categorical_variables_stats = ard_stats_cat_vars
  )

  output
}

extract_grouping_variables <- function(x) {

  grouping_variables <- x |>
    dplyr::select(
      tidyselect::contains("group")
    ) |>
    dplyr::select(
      !tidyselect::contains("level")
    ) |>
    dplyr::distinct() |>
    stats::na.omit() |>
    tidyr::pivot_longer(
      cols = tidyselect::everything()
    ) |>
    # we want group1 to be first, followed by group2 etc ...
    dplyr::arrange(
      name
    ) |>
    dplyr::pull(
      value
    )

  grouping_variables
}
