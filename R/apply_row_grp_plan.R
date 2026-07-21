#' Apply row group structure formatting to data
#'
#' @param .data data
#' @param row_grp_struct_list list of row group structure objects
#' @param group symbolic list of grouping
#' @param label symbolic label column
#'
#' @noRd
apply_row_grp_struct <- function(
    .data,
    row_grp_struct_list,
    group,
    label = NULL,
    ...
) {
    # Locate which groups need which formatting
    # determine which rows each block applies to
    .data <- .data %>%
        dplyr::mutate(TEMP_row = dplyr::row_number())

    # for each structure object, (1) split the data on any default values, (2) split the data on specific data values
    # get nested list object:
    #  length = number of structures, each element contains list of data splits (row indices)
    TEMP_appl_row <- row_grp_struct_list %>%
        map(function(struct) {
            grping <- expr_to_grouping(struct, group)

            split_dat <- .data %>%
                dplyr::group_by(
                    dplyr::across(
                        tidyselect::all_of(
                            grping
                        )
                    )
                ) %>%
                dplyr::group_split()
            map(split_dat, function(dat) {
                struct_val_idx(struct, dat, group, label)
            }) %>%
                list_flatten()
        })

    TEMP_block_to_apply <- row_grp_struct_list %>% map(~ .$block_to_apply)

    # similar to frmts, only allow 1 element_block for a given row
    #   - within block-specific data, split data further by grouping vars
    dat_plus_block <- tibble(
        TEMP_appl_row,
        TEMP_block_to_apply
    ) %>%
        dplyr::mutate(TEMP_block_rank = dplyr::row_number()) %>%
        # unnest to 1 rec per data chunk
        unnest_longer(
            TEMP_appl_row,
            indices_to = "TEMP_chunk_num",
            transform = unlist
        ) %>%
        # unnest to 1 rec per data row, to handle where chunk >1 row
        unnest(TEMP_appl_row) %>%
        dplyr::group_by(TEMP_appl_row) %>%
        dplyr::arrange(
            TEMP_appl_row,
            dplyr::desc(.data$TEMP_block_rank)
        ) %>%
        dplyr::slice(1) %>%
        dplyr::left_join(.data, ., by = c("TEMP_row" = "TEMP_appl_row")) %>%
        dplyr::group_by(
            .data$TEMP_block_rank,
            .data$TEMP_chunk_num,
            .data$TEMP_block_to_apply
        ) %>%
        nest()

    # get max character width for each column in the full data
    dat_max_widths <- .data %>%
        dplyr::summarise(
            dplyr::across(
                tidyselect::everything(),
                function(x) {
                    if (is.character(x)) {
                        str_split(x, "\\n") %>%
                            unlist() %>%
                            nchar() %>%
                            max(na.rm = TRUE)
                    } else {
                        max(nchar(x), na.rm = TRUE)
                    }
                }
            )
        )

    # apply group block function to data subsets
    add_ln_df <- map2_dfr(
        dat_plus_block$data,
        dat_plus_block$TEMP_block_to_apply,
        function(x, y) {
            if (is.null(y)) {
                x
            } else {
                apply_grp_block(
                    .data = x,
                    group = group,
                    element_block = y,
                    widths = dat_max_widths
                )
            }
        }
    ) %>%
        dplyr::arrange(.data$TEMP_row) %>%
        dplyr::select(-"TEMP_row")

    add_ln_df
}


#' Apply row group block labelled to data
#'
#' @param .data data
#' @param element_row_grp_loc element object specifying row group label location
#' @param group symbolic list of grouping
#' @param label symbolic label column
#'
#' @noRd
apply_row_grp_lbl <- function(
    .data,
    element_row_grp_loc,
    group,
    label = NULL,
    ...
) {
    # store values of label column
    lbl_col <- eval_tidy(label, .data)

    # check if lbl_col contains NA
    if (anyNA(lbl_col)) {
        stop(paste0(
            "`label` column ",
            quo_name(label),
            " contains NA values. For group-level summary data, `label` and the relevant `group` values should match."
        ))
    }

    # check which group/label columns are available

    grps_avail <- eval_tidyselect_on_colvec(group, names(.data))

    if (
        length(grps_avail) == 0 ||
            is_empty(label) ||
            element_row_grp_loc$location %in%
                c("gtdefault", "noprint", "column")
    ) {
        add_ln_df <- .data
    } else {
        #  combine any grouping columns that need combining into label
        add_ln_df <- .data %>%
            combine_group_cols(as_vars(grps_avail), label, element_row_grp_loc)
    }
    add_ln_df
}

#' Apply row group element blocks
#'
#' @param .data data, but only what is getting changed
#' @param group list of the group parameters
#' @param element_block element_block to be applied
#' @param widths tibble of widths for each column
#'
#' @return dataset with element block applied
#'
#' @noRd
apply_grp_block <- function(.data, group, element_block, widths) {
    if (!is.null(element_block$post_space)) {
        # create add-on row
        # utilize TEMP_row to retain the ordering
        grp_row_add <- .data %>%
            dplyr::slice(dplyr::n()) %>%
            dplyr::mutate(
                dplyr::across(
                    c(
                        -map_chr(group, as_name),
                        -tidyselect::where(is.numeric)
                    ),
                    ~ replace(
                        .x,
                        values = fill_post_space(
                            post_space = element_block$post_space,
                            fill = element_block$fill,
                            width = widths[[dplyr::cur_column()]]
                        )
                    )
                ),
                TEMP_row = .data$TEMP_row + 0.1
            )

        # combine with original data
        dplyr::bind_rows(.data, grp_row_add) %>%
            fill(!!!group) %>%
            dplyr::mutate(..tfrmt_post_space_row = .data$TEMP_row %% 1 != 0)
    } else {
        .data
    }
}


#' Fill the cell value with post space character
#'
#' @param post_space Character value for post space
#' @param fill Whether to recycle value in `post_space` to match data width
#' @param width width to make the post_space value in order to fill the cell
#'
#' @return character value containing post space value modified to fill cell
#' @noRd
#'
fill_post_space <- function(post_space, fill, width) {
    ## if only white space, no need to make wider for visuals
    if (grepl("^\\s*$", post_space)) {
        return(" ")
    }

    length_post_space <- nchar(post_space)

    if (fill) {
        reps <- ceiling(width / length_post_space)
        fill_val <- strrep(post_space, reps) %>% str_sub(1, width)
    } else {
        fill_val <- str_sub(post_space, 1, width) # truncate to data width if needed
    }

    return(fill_val)
}


#' Combine group cols into the Labels
#'
#' @param .data Pre-processed data that just needs columns combining
#' @param group list of the group parameters
#' @param label label symbol should only be one
#' @param element_row_grp_loc row group location element. If null then will just indent
#'
#' @return dataset with the group columns combines
#' @noRd
combine_group_cols <- function(
    .data,
    group,
    label,
    element_row_grp_loc = NULL
) {
    orig_group_names <- map_chr(group, as_name)
    top_grouping <- group #used for spliting in case of spanning label

    .data <- .data %>%
        dplyr::mutate(..tfrmt_row_grp_lbl = FALSE)

    # ensure label is character
    .data <- .data %>%
        dplyr::mutate(
            dplyr::across(!!label, ~ as.character(.x))
        )

    if (is.null(element_row_grp_loc)) {
        indent <- "  "
    } else if (
        element_row_grp_loc$location == "spanning" && length(group) > 0
    ) {
        group <- group[-1]
        indent <- element_row_grp_loc$indent
    } else {
        indent <- element_row_grp_loc$indent
    }

    while (length(group) > 0 && !is.null(label)) {
        split_dat <- .data %>%
            dplyr::group_by(run_id = dplyr::consecutive_id(!!!top_grouping)) %>%
            dplyr::group_split() %>%
            map(~ dplyr::select(.x, -run_id))

        .data <- split_dat %>%
            map_dfr(function(lone_dat) {
                lone_dat_summ <- lone_dat %>%
                    dplyr::mutate(
                        ..tfrmt_summary_row = str_trim(
                            !!label,
                            side = "left"
                        ) ==
                            str_trim(!!dplyr::last(group), side = "left")
                    )

                if (any(lone_dat_summ$..tfrmt_summary_row) == FALSE) {
                    # if the set of rows contains NO group-level summary data, create an
                    # extra row to be added

                    # first containing grouping/label values
                    new_row <- lone_dat %>%
                        dplyr::select(!!!top_grouping, !!label) %>%
                        dplyr::mutate(!!label := !!dplyr::last(group)) %>%
                        dplyr::distinct()

                    # next all of the other variables (as missing)
                    new_row <- lone_dat %>%
                        dplyr::select(
                            -tidyselect::any_of(
                                names(new_row)
                            )
                        ) %>%
                        dplyr::slice(0) %>%
                        tibble::add_row() %>%
                        dplyr::mutate(
                            dplyr::across(
                                #convert NULL to NA in list-cols
                                tidyselect::where(is.list),
                                ~ map(
                                    .x,
                                    ~ . %||% NA_character_
                                )
                            )
                        ) %>%
                        dplyr::bind_cols(new_row, .) %>%
                        dplyr::mutate(..tfrmt_row_grp_lbl = TRUE)
                } else {
                    new_row <- tibble()
                }

                lone_dat_summ %>%
                    # only indent if not a summary row
                    dplyr::mutate(
                        !!label := ifelse(
                            .data$..tfrmt_summary_row == TRUE,
                            !!label,
                            str_c(indent, !!label)
                        )
                    ) %>%
                    dplyr::select(-"..tfrmt_summary_row") %>%
                    dplyr::bind_rows(new_row, .)
            })
        group <- group[-length(group)]
        top_grouping <- top_grouping[-length(top_grouping)]
    }

    .data %>%
        dplyr::mutate(
            dplyr::across(
                tidyselect::any_of(
                    orig_group_names
                ),
                ~ as.character(.x)
            )
        )
}

#' Remove row groups based on element_row_grp_loc and grouping
#'
#' @param .data data
#' @param element_row_grp_loc element object specifying row group label location
#' @param group symbolic list of grouping
#' @param label symbolic label column
#'
#' @noRd
remove_grp_cols <- function(.data, element_row_grp_loc, group, label = NULL) {
    # check which group/label columns are available
    grps_avail <- eval_tidyselect_on_colvec(group, names(.data))

    if (
        length(grps_avail) == 0 ||
            element_row_grp_loc$location %in% c("gtdefault", "column")
    ) {
        add_ln_df <- .data
    } else {
        group <- as_vars(grps_avail)

        # Either drop group columns ("no print"), or format them w/ label
        if (element_row_grp_loc$location == "noprint") {
            add_ln_df <- .data %>% dplyr::select(-c(!!!group))
        } else if (element_row_grp_loc$location == "indented") {
            add_ln_df <- .data %>%
                dplyr::select(-c(!!!group))
        } else if (length(group) == 1) {
            #Using the grouping in gt + a single grouping
            add_ln_df <- .data %>%
                dplyr::group_by(!!group[[1]])
        } else {
            # Using the grouping in gt, but needs to drop all groups in label
            add_ln_df <- .data %>%
                dplyr::select(-c(!!!group[-1])) %>%
                dplyr::group_by(!!group[[1]])
        }
    }
    add_ln_df
}


#' Remove trailing post-space rows and helper column
#' @param .data processed wide tbl
#' @noRd
apply_post_space_trim <- function(.data) {
    target_col <- "..tfrmt_post_space_row"

    if (target_col %in% names(.data)) {
        # If the very last row was tagged as a spacer, drop it
        if (isTRUE(dplyr::last(.data[[target_col]]))) {
            .data <- .data %>%
                dplyr::slice(-dplyr::n())
        }
        # Always drop the helper column before returning
        .data <- .data %>%
            dplyr::select(
                -tidyselect::all_of(target_col)
            )
    }

    .data
}
