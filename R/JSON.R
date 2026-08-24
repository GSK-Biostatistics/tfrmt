#' Print to JSON
#'
#' @param tfrmt tfrmt to print
#' @param path file path to save JSON to. If not provided the JSON will just print to the console
#'
#' @return JSON
#' @export
#' @examples
#' tfrmt(
#'   label = label,
#'   column = column,
#'   param = param,
#'   value=value) |>
#'   tfrmt_to_json()
tfrmt_to_json <- function(tfrmt, path = NULL) {
    if (!is_tfrmt(tfrmt)) {
        stop("Needs tfrmt")
    }
    output <- as_json(tfrmt)

    if (!is.null(path)) {
        message(paste0("Writing json file out to:\n", path))
        write(output, path)
    } else {
        return(output)
    }
}


#' Convert tfrmt elements
#'
#' This collection of functions is used to create a json or json ready objects.
#' While primarily intended for internal use these functions can be used externally
#'
#' @param x tfrmt or tfrmt element
#'
#' @return as_json.tfrmt() will return a json object all other methods will
#'   return a list that is cleaned so it can be directly converted to a json
#'   object
#'
#' @keywords internal
#' @export
as_json <- function(x) {
    UseMethod("as_json", x)
}

#' @export
as_json.tfrmt <- function(x) {
    # Prepare each element to get converted to JSON
    tfrmt_nm <- names(x)

    out <- vector("list", length = length(x))
    for (i in seq_along(x)) {
        out[[i]] <- x[[i]] %>%
            as_json()
    }
    names(out) <- tfrmt_nm

    # Converts list to a json object
    output_json <- out %>%
        jsonlite::toJSON(pretty = TRUE, force = TRUE)

    # Removing names added in by jsonlite
    # only change spaces at the beginning of the row
    json_split <- output_json %>%
        stringr::str_split(stringr::fixed("\n")) %>%
        unlist()
    # Needs updating
    to_replace <- stringr::str_which(json_split, '^\\s+\"\\s(\\.\\d+)?\"')
    json_split[to_replace] <- json_split[to_replace] %>%
        stringr::str_replace_all('\"\\s(\\.\\d+)?\"', '\"\"')
    json_clean <- stringr::str_c(json_split, collapse = "\n")
    if (jsonlite::validate(json_clean)) {
        class(json_clean) <- "json"
    } else {
        stop("Error when creating the json object")
    }
    json_clean
}

#' @export
as_json.default <- function(x) {
    x
}

#' @export
as_json.quosures <- function(x) {
    purrr::map_chr(x, rlang::as_label)
}


#' @export
as_json.quosure <- function(x) {
    out <- x %>%
        rlang::as_label()
    if (out != "<empty>") {
        return(out)
    }
}

#' @export
as_json.body_plan <- function(x) {
    purrr::map(x, as_json)
}

#' @export
as_json.frmt_structure <- function(x) {
    x$frmt_to_apply <- purrr::map(x$frmt_to_apply, as_json)

    list(
        group_val = x$group_val,
        label_val = x$label_val,
        param_val = x$param_val
    ) %>%
        c(x$frmt_to_apply[[1]][1])
}

#' @export
as_json.frmt <- function(x) {
    if (!is.null(x$transform)) {
        x$transform <- deparse(x$transform) %>%
            stringr::str_c(collapse = "")
    }
    list(frmt = x)
}

#' @export
as_json.frmt_when <- function(x) {
    lhs <- purrr::map_chr(x$frmt_ls, f_lhs_as_char)
    rhs <- x$frmt_ls |>
        purrr::map(rlang::f_rhs) |>
        purrr::map(as_json)
    names(rhs) <- lhs
    list(frmt_when = list(frmt_ls = rhs, missing = x$missing))
}

#' @export
as_json.frmt_combine <- function(x) {
    frmts <- x$frmt_ls %>%
        purrr::map(as_json)
    list(
        frmt_combine = list(
            expression = x$expression,
            frmt_ls = frmts,
            missing = x$missing
        )
    )
}

#' @export
as_json.col_plan <- function(x) {
    if (is.null(x)) {
        c()
    } else {
        dot_ls <- x$dots %>%
            purrr::map(as_json)

        if (!is.null(names(dot_ls))) {
            names(dot_ls) <- names(x$dots) %>% ifelse(nzchar(.), ., " ")
        }
        list(col_plan = list(dots = dot_ls, .drop = x$.drop))
    }
}

#' @export
as_json.span_structure <- function(x) {
    purrr::map(
        x,
        purrr::map_chr,
        as_json
    ) %>%
        list(span_structure = .)
}


#' @export
as_json.col_style_plan <- function(x) {
    x %>%
        purrr::map(as_json)
}

#' @export
as_json.col_style_structure <- function(x) {
    x$cols <- x$cols %>%
        purrr::map(as_json)
    x
}

#' @export
as_json.page_plan <- function(x) {
    if (!is.null(x$transform)) {
        x$transform <- rlang::expr_text(x$transform)
    }

    x
}

#' json to tfrmt
#'
#' Reader to read JSON files/objects into tfrmt objects
#' @param path location of the json file to read in
#'
#' @param json json object to read in. By default this is null. This function
#'   will read in json object preferentially. So if both a path and a json
#'   object are supplied the json object will be read in.
#'
#' @export
json_to_tfrmt <- function(path = NULL, json = NULL) {
    if (!is.null(json)) {
        dirty_list <- jsonlite::parse_json(json)
    } else if (!is.null(path)) {
        dirty_list <- jsonlite::read_json(path)
    } else {
        stop("Path or json object needed")
    }

    json_nm <- names(dirty_list)
    simple_vars <- c(
        "group",
        "label",
        "param",
        "value",
        "column",
        "title",
        "subtitle",
        "sorting_cols"
    )

    simple_params <- dirty_list[intersect(simple_vars, json_nm)] %>%
        purrr::map(unlist)

    rgp <- ls_to_row_grp_plan(dirty_list$row_grp_plan)
    bp <- ls_to_body_plan(dirty_list$body_plan)
    csp <- ls_to_col_style_plan(dirty_list$col_style_plan)
    cp <- ls_to_col_plan(dirty_list$col_plan)
    bn <- ls_to_big_n(dirty_list$big_n)
    fnp <- ls_to_footnote_plan(dirty_list$footnote_plan)
    pp <- ls_to_page_plan(dirty_list$page_plan)

    all_params <- c(
        simple_params,
        list(row_grp_plan = rgp),
        list(body_plan = bp),
        list(big_n = bn),
        list(footnote_plan = fnp),
        list(col_plan = cp),
        list(col_style_plan = csp),
        list(page_plan = pp)
    ) %>%
        purrr::discard(is.null)
    do.call(tfrmt, all_params)
}

ls_to_row_grp_plan <- function(ls) {
    if (!is.null(ls)) {
        struct_ls <- ls$struct_list %>%
            purrr::map(function(struct) {
                el_block <- do.call(
                    element_block,
                    struct$block_to_apply %>% purrr::map(unlist)
                )
                if (!is.null(names(struct$group_val))) {
                    group_val <- purrr::map(struct$group_val, unlist)
                } else {
                    group_val <- unlist(struct$group_val)
                }
                do.call(
                    row_grp_structure,
                    list(group_val = group_val, element_block = el_block)
                )
            })

        label_loc <- do.call(
            element_row_grp_loc,
            ls$label_loc %>% purrr::map(unlist)
        )
        ls <- do.call(row_grp_plan, c(struct_ls, list(label_loc = label_loc)))
    }
    ls
}

ls_to_body_plan <- function(ls) {
    if (!is.null(ls)) {
        frmts_ls <- ls %>%
            purrr::map(function(struct) {
                frmt_loc <- stringr::str_which(names(struct), "frmt*")
                type <- names(struct)[frmt_loc]
                frmt_val <- do.call(
                    paste0("ls_to_", type),
                    list(x = struct[[type]])
                ) %>%
                    list()
                group_val <- simplify_group_val(struct$group_val)
                arg_list <- c(
                    list(group_val = group_val),
                    list(label_val = unlist(struct$label_val)),
                    frmt_val
                )
                out_struct <- do.call(frmt_structure, arg_list)
                out_struct$param_val <- unlist(struct$param_val)
                out_struct
            })

        do.call(body_plan, frmts_ls)
    }
}

ls_to_frmt <- function(x) {
    x <- x %>%
        purrr::map(unlist)

    if (!is.null(x$transform)) {
        x$transform <- str2lang(x$transform)
    }
    do.call(
        frmt,
        list(
            expression = x$expression,
            missing = unlist(x$missing),
            scientific = x$scientific,
            transform = x$transform
        )
    )
}

ls_to_frmt_combine <- function(x) {
    fmts <- x$frmt_ls %>%
        purrr::map(function(a_frmt) {
            do.call(paste0("ls_to_", names(a_frmt)), list(x = a_frmt[[1]]))
        })
    x <- x %>%
        purrr::map(unlist)
    do.call(
        frmt_combine,
        c(list(expression = x$expression), fmts, list(missing = x$missing))
    )
}

ls_to_frmt_when <- function(x) {
    fmts <- x$frmt_ls %>%
        purrr::map(function(a_frmt) {
            if (!is.null(names(a_frmt))) {
                out <- do.call(
                    paste0("ls_to_", names(a_frmt)),
                    list(x = a_frmt[[1]])
                ) %>%
                    as.character()
            } else {
                out <- stringr::str_c("'", a_frmt[[1]], "'")
            }
            out
        })

    lhs <- dplyr::if_else(
        names(fmts) == "TRUE",
        names(fmts),
        stringr::str_c("'", names(fmts), "'")
    )
    formula_ls <- stringr::str_c(lhs, " ~ ", fmts) %>%
        purrr::map(stats::as.formula)

    do.call(frmt_when, c(formula_ls, list(missing = unlist(x$missing))))
}

ls_to_big_n <- function(ls) {
    if (!is.null(ls)) {
        n_frmt <- do.call(ls_to_frmt, list(ls$n_frmt))
        do.call(
            big_n_structure,
            list(
                param_val = unlist(ls$param_val),
                n_frmt = n_frmt,
                by_page = unlist(ls$by_page)
            )
        )
    }
}

ls_to_footnote_plan <- function(ls) {
    if (!is.null(ls)) {
        struct_ls <- ls$struct_list %>%
            purrr::map(function(struct) {
                group_val <- simplify_group_val(struct$group_val)
                column_val <- simplify_group_val(struct$column_val)
                unlisted <- struct %>%
                    purrr::map(unlist)
                do.call(
                    footnote_structure,
                    list(
                        footnote_text = unlisted$footnote_text,
                        group_val = group_val,
                        label_val = unlisted$label_val,
                        column_val = column_val
                    )
                )
            })

        do.call(
            footnote_plan,
            c(
                struct_ls,
                list(
                    marks = unlist(ls$marks),
                    order = unlist(ls$order)
                )
            )
        )
    }
}

ls_to_col_plan <- function(ls) {
    if (!is.null(ls)) {
        dots <- ls$col_plan$dots %>%
            purrr::map(function(el) {
                if (!is.null(names(el)) && names(el) == "span_structure") {
                    el$span_structure %>%
                        ls_to_span_structure() %>%
                        as.character() %>%
                        rlang::parse_expr()
                } else {
                    el[[1]] %>%
                        stringr::str_replace_all(stringr::fixed("\""), "'") %>%
                        char_as_quo() %>%
                        rlang::quo_get_expr()
                }
            })

        do.call(col_plan, c(dots, list(.drop = unlist(ls$col_plan$.drop))))
    }
}

ls_to_span_structure <- function(ls) {
    span_ls <- ls %>%
        purrr::map(
            ~ unlist(.) %>%
                stringr::str_c("'", ., "'") %>%
                stringr::str_c(collapse = ", ") %>%
                stringr::str_c("c(", ., ")") %>%
                rlang::parse_expr()
        )

    do.call(span_structure, span_ls)
}

ls_to_col_style_plan <- function(ls) {
    if (!is.null(ls)) {
        struct_ls <- ls %>%
            purrr::map(
                function(struct) {
                    stuct_in <- purrr::map(struct, unlist)
                    names(stuct_in) <- names(stuct_in) %>%
                        stringr::str_replace(stringr::fixed("cols"), "col")
                    cols_val <- struct[["cols"]][[1]]
                    if (
                        !is.null(names(cols_val)) &&
                            names(cols_val) == "span_structure"
                    ) {
                        stuct_in[["col"]] <- cols_val[[1]] %>%
                            ls_to_span_structure() %>%
                            as.character() %>%
                            rlang::parse_expr()
                    } else {
                        stuct_in[["col"]] <- purrr::map_chr(
                            stuct_in[["col"]],
                            ~ char_as_quo(.x) %>%
                                rlang::quo_get_expr() %>%
                                rlang::expr_text()
                        ) %>%
                            stringr::str_c(collapse = ", ") %>%
                            paste0("vars(", ., ")") %>%
                            rlang::parse_expr()
                    }
                    do.call(col_style_structure, stuct_in)
                }
            )

        do.call(col_style_plan, struct_ls)
    }
}

ls_to_page_plan <- function(ls) {
    if (!is.null(ls)) {
        struct_ls <- ls$struct_list |>
            purrr::map(
                function(struct) {
                    group_val <- simplify_group_val(struct$group_val)

                    unlisted <- purrr::map(struct, unlist)

                    do.call(
                        page_structure,
                        list(
                            group_val = group_val,
                            label_val = unlisted$label_val
                        )
                    )
                }
            )

        transform <- unlist(ls$transform)

        if (!is.null(transform)) {
            transform <- rlang::parse_expr(transform)
        }

        do.call(
            page_plan,
            c(
                struct_ls,
                list(
                    note_loc = unlist(ls$note_loc),
                    max_rows = unlist(ls$max_rows),
                    transform = transform
                )
            )
        )
    }
}

simplify_group_val <- function(group_ls) {
    if (length(group_ls) == 0) {
        group_val <- NULL
    } else if (!is.null(names(group_ls))) {
        group_val <- purrr::map(group_ls, unlist)
    } else {
        group_val <- unlist(group_ls)
    }
    group_val
}
