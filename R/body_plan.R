#' Table Body Plan
#'
#' Define the formatting of the body contents of the table through a series of
#' frmt_structures. Structures get applied in order from bottom up, so the last
#' added structure is the first applied.
#'
#' @seealso [frmt_structure()] defines which rows the formats will be applied
#'   to, and [frmt()], [frmt_combine()], and [frmt_when()] define the format
#'   semantics.
#'
#'   \href{https://gsk-biostatistics.github.io/tfrmt/articles/body_plan.html}{Link to related article}
#'
#' @param ... list of frmt_structures defining the body formatting
#'
#' @return body_plan object
#'
#' @examples
#'
#'   tfrmt_spec<- tfrmt(
#'       title = "Table Title",
#'       body_plan = body_plan(
#'         frmt_structure(
#'           group_val = c("group1"),
#'           label_val = ".default",
#'           frmt("XXX")
#'         )
#'       )
#'      )
#'
#' @export
#'
body_plan <- function(...) {
    frmt_structure_list <- list(...)

    for (struct_idx in seq_along(frmt_structure_list)) {
        if (!is_frmt_structure(frmt_structure_list[[struct_idx]])) {
            stop(paste0(
                "Entry number ",
                struct_idx,
                " is not an object of class `frmt_structure`."
            ))
        }
    }

    structure(
        frmt_structure_list,
        class = c("body_plan", "frmt_table")
    )
}


#' Build contents of body (group/label value-specific) plan based on significant digits specifications
#'
#' @param data significant digits data for a given set of group/label values
#' @param tfrmt tfrmt object
#' @param param_defaults parameter-level significant digits specifications
#' @param missing missing option to be included in all `frmt`s
#'
#' @return list of `frmt_structure` objects
#' @noRd
body_plan_builder <- function(
    data,
    group,
    label,
    param_defaults,
    missing = NULL
) {
    # prep params for frmt functions
    param_tbl <- seq_along(param_defaults) %>%
        purrr::map_dfr(
            ~ tibble::tibble(
                param_display = names(param_defaults)[.x],
                sigdig = list(param_defaults[[.x]] + data$sigdig[[1]]),
                pos = .x
            )
        ) %>%
        dplyr::mutate(
            contains_glue = stringr::str_detect(
                .data$param_display,
                "\\{.*\\}"
            ), # is this to be a frmt_combine
            param = purrr::map2(
                .data$param_display,
                .data$contains_glue,
                ~ if (.y) {
                    stringr::str_extract_all(
                        .x,
                        "(?<=\\{)[^\\}]+(?=\\})"
                    ) %>%
                        unlist()
                } else {
                    .x
                }
            ),
            single_glue_to_frmt = purrr::pmap_chr(
                list(.data$contains_glue, .data$param, .data$param_display),
                function(a, b, c) {
                    if (a && length(b) == 1) c else NA_character_
                }
            )
        ) %>%
        tidyr::unnest(
            tidyselect::everything()
        ) %>%
        dplyr::mutate(
            frmt_string = purrr::map2_chr(
                .data$sigdig,
                .data$single_glue_to_frmt,
                sigdig_frmt_string
            )
        )

    frmt_vec <- param_tbl %>%
        dplyr::group_by(.data$pos) %>%
        dplyr::group_split() %>%
        purrr::map(function(x) {
            if (sum(x$contains_glue) > 1) {
                frmt_combine_builder(
                    x$param_display[[1]],
                    x$param,
                    x$frmt_string,
                    missing
                )
            } else {
                frmt_builder(x$param, x$frmt_string, missing)
            }
        })

    frmt_vec <- do.call(c, frmt_vec)

    # group/label names from tfrmt
    grp_names <- if (length(group) == 0) {
        character(0)
    } else {
        purrr::map_chr(group, rlang::as_name)
    }
    lbl_names <- if (rlang::quo_is_missing(label)) {
        character(0)
    } else {
        rlang::as_name(label)
    }

    # sigdig value
    sigdig <- data$sigdig[[1]]

    which_grp <- grp_names[grp_names %in% names(data)]
    which_lbl <- lbl_names[lbl_names %in% names(data)]

    if (length(which_grp) > 0) {
        group_val <- data[, which_grp] %>%
            as.list() %>%
            purrr::map(unique)

        if (length(grp_names) > length(group_val)) {
            group_val_to_add <- grp_names[!grp_names %in% names(group_val)]
            group_list_to_add <- rep(".default", length(group_val_to_add)) %>%
                as.list() %>%
                stats::setNames(group_val_to_add)
            group_val <- c(group_val, group_list_to_add)[grp_names]
        }
    } else {
        group_val <- ".default"
    }

    if (length(which_lbl) > 0) {
        label_val <- data[, which_lbl, drop = TRUE] %>% unique()
        label_val <- if (any(label_val == ".default")) {
            ".default"
        } else {
            label_val
        }
    } else {
        label_val <- ".default"
    }

    frmt_structure_builder(group_val, label_val, frmt_vec)
}
