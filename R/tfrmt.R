#' Table Format
#'
#' tfrmt, or "table format" is a way to pre-define the non-data components of
#' your tables, and how the data will be handled once added: i.e. title,
#' footers, headers, span headers, and cell formats. In addition, tfrmt's can be
#' layered, building from one table format to the next. For cases where only one
#' value can be used, the newly defined tfrmt accepts the latest tfrmt
#'
#' @param tfrmt_obj a tfrmt object to base this new format off of
#' @param group what are the grouping vars of the input dataset
#' @param label what is the label column of the input dataset
#' @param param what is the param column of the input dataset
#' @param value what is the value column of the input dataset
#' @param column what is the column names column in the input dataset
#' @param title title of the table
#' @param subtitle subtitle of the table
#' @param row_grp_plan plan of the row groups blocking. Takes a [row_grp_plan()]
#' @param body_plan combination and formatting of the input data. Takes a [body_plan()]
#' @param col_style_plan how to style columns including alignment (left, right, character) and width. Takes a [col_style_plan()]
#' @param col_plan a col_plan object which is used to select, rename, and nest
#'   columns. Takes a [col_plan()]
#' @param sorting_cols which columns determine sorting of output
#' @param big_n how to format subject totals ("big Ns") for inclusion in the column labels. Takes a [big_n_structure()]
#' @param footnote_plan footnotes to be added to the table. Takes a [footnote_plan()]
#' @param page_plan pagination splits to be applied to the table. Takes a [page_plan()]
#' @param ... These dots are for future extensions and must be empty.
#'
#' @return tfrmt object
#'
#' @details
#'
#'  ## NSE and Argument Evaluation
#'
#'   - tfrmt allows users to pass `vars`, `quo`, and unquoted expressions to a
#'   variety of arguments, such as `group`, `label`, `param`, `value`,
#'   `column`, and `sorting_cols`. Users accustomed to tidyverse semantics
#'   should be familiar with this behaviour. However, there is an important
#'   behaviour difference between tfrmt and normal tidyverse functions. Because
#'   the data are not a part of tfrmt, it does not know when a value being
#'   passed to it is intended to be an unquoted expression representing a column
#'   name or an object from the environment. As such, it preferentially uses the
#'   value from the environment over preserving the entry as an expression. For
#'   example, if you have an object "my_object" in your
#'   environment with the value "Hello world", and try to create a tfrmt as
#'   `tfrmt(column = my_object)`, it will take the value of "my_object" over
#'   assuming the column argument is an unquoted expression and view the entry
#'   to `column` as "Hello World". To pass "my_object" to tfrmt as a column name, use
#'   quotes around the value: `tfrmt(columnn = "my_object")`.
#'
#'   - Additionally, unquoted expressions that match `tfrmt`'s other
#'   argument names can cause unexpected results. It is recommended
#'   to put quotes around the value as such:
#'   `tfrmt(label = "group")`. In this case, the quoting will prevent `tfrmt`
#'   from assigning its `group` input value to the `label` value.
#'
#' @seealso \href{https://gsk-biostatistics.github.io/tfrmt/articles/building_blocks.html}{Link to related article}
#'
#' @section Images:
#' Here are some example outputs:
#' \if{html}{\out{
#' `r "<img src=\"https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/images/tfrmt-groups-three-cols-cropped.jpg\" alt =\"Example showing no groups, one group and two groups\" style=\"width:100\\%;\">"`
#' }}
#'
#' @rdname tfrmt
#'
#' @export
#' @examples
#'
#' tfrmt_spec <- tfrmt(
#'   label = label,
#'   column = column,
#'   param = param,
#'   value=value)
#'
#' tfrmt_spec <- tfrmt(
#'   label = label,
#'   column = column,
#'   param = param,
#'   value=value,
#' # Set the formatting for values
#'   body_plan = body_plan(
#'     frmt_structure(
#'       group_val = ".default",
#'       label_val = ".default",
#'       frmt_combine("{n} {pct}",
#'            n = frmt("xxx"),
#'            pct = frmt_when(
#'                 "==100" ~ "(100%)",
#'                 "==0" ~ "",
#'                 TRUE ~ frmt("(xx.x %)")
#'                 )
#'            )
#'     )
#'   ),
#' # Specify column styling plan
#'   col_style_plan = col_style_plan(
#'     col_style_structure(col = vars(everything()), align = c(".",","," "))
#'   ))
#'
#' tfrmt_spec <- tfrmt(
#'   group = group,
#'   label = label,
#'   column = column,
#'   param = param,
#'   value=value,
#'   sorting_cols = c(ord1, ord2),
#'   # specify value formatting
#'   body_plan = body_plan(
#'     frmt_structure(
#'       group_val = ".default",
#'       label_val = ".default",
#'       frmt_combine("{n} {pct}",
#'            n = frmt("xxx"),
#'            pct = frmt_when(
#'                 "==100" ~ "(100%)",
#'                 "==0" ~ "",
#'                 TRUE ~ frmt("(xx.x %)")
#'                 )
#'            )
#'     ),
#'     frmt_structure(
#'         group_val = ".default",
#'         label_val = "n",
#'         frmt("xxx")
#'    ),
#'     frmt_structure(
#'         group_val = ".default",
#'         label_val = c("Mean", "Median", "Min","Max"),
#'         frmt("xxx.x")
#'      ),
#'     frmt_structure(
#'         group_val = ".default",
#'         label_val = "SD",
#'         frmt("xxx.xx")
#'     ),
#'     frmt_structure(
#'         group_val = ".default",
#'         label_val = ".default",
#'         p = frmt("")
#'     ),
#'     frmt_structure(
#'         group_val = ".default",
#'         label_val = c("n","<65 yrs","<12 months","<25"),
#'         p = frmt_when(
#'             ">0.99" ~ ">0.99",
#'             "<0.001" ~ "<0.001",
#'             TRUE ~ frmt("x.xxx", missing = "")
#'         )
#'     )
#'   ),
#'   # remove extra cols
#'   col_plan = col_plan(-grp,
#'                       -starts_with("ord") ),
#'   # Specify column styling plan
#'   col_style_plan = col_style_plan(
#'     col_style_structure(col = vars(everything()), align = c(".",","," "))
#'   ),
#'
#'   # Specify row group plan
#'   row_grp_plan = row_grp_plan(
#'     row_grp_structure(
#'          group_val = ".default",
#'          element_block(post_space = " ")
#'     ),
#'     label_loc = element_row_grp_loc(location = "column")
#'   )
#'
#' )
#'
#'
tfrmt <- function(
    tfrmt_obj,
    group = rlang::quos(),
    label = rlang::quo(),
    param = rlang::quo(),
    value = rlang::quo(),
    column = rlang::quos(),
    title,
    subtitle,
    row_grp_plan, #the style between blocking
    body_plan,
    col_style_plan,
    col_plan,
    sorting_cols,
    big_n,
    footnote_plan,
    page_plan,
    ...
) {
    tfrmt_el <- tfrmt_find_args(
        ...,
        env = environment(),
        parent_env = rlang::caller_env()
    )

    new_tfrmt <- structure(
        tfrmt_el,
        class = "tfrmt"
    )

    # check non-null plan parameters are supplied with plan function
    check_plan(new_tfrmt, "body_plan")
    check_plan(new_tfrmt, "col_plan")
    check_plan(new_tfrmt, "col_style_plan")
    check_plan(new_tfrmt, "row_grp_plan")
    check_plan(new_tfrmt, "footnote_plan")
    check_plan(new_tfrmt, "page_plan")

    # check non-null big_n is supplied a big_n_structure
    check_big_n(new_tfrmt)

    if (!missing(tfrmt_obj)) {
        new_tfrmt <- layer_tfrmt(
            tfrmt_obj,
            new_tfrmt
        )
    }

    ## check to confirm user has not defined multiple columns and
    ## any span_structures in col_plan
    check_column_and_col_plan(new_tfrmt)
    check_group_var_consistency(new_tfrmt)
    check_col_style_row_grp_consistency(new_tfrmt)
    check_footnote_plan(new_tfrmt)

    new_tfrmt
}

is_tfrmt <- function(x) {
    inherits(x, "tfrmt")
}

check_tfrmt <- function(
    tfrmt,
    arg = rlang::caller_arg(tfrmt),
    call = rlang::caller_env(),
    allow_null = FALSE
) {
    if (!missing(tfrmt)) {
        if (is_tfrmt(tfrmt)) {
            return(invisible(NULL))
        }

        if (allow_null && is.null(tfrmt)) {
            return(invisible(NULL))
        }
    }

    rlang::stop_input_type(
        tfrmt,
        "a tfrmt object",
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

tfrmt_find_args <- function(
    ...,
    env = parent.frame(),
    parent_env = parent.env(env)
) {
    ## get args of parent function
    arg_parent <- names(formals(sys.function(sys.parent(1))))
    ## don't try to get the tftmt obj
    args <- setdiff(arg_parent, c("tfrmt_obj", "..."))

    ## get the values from the parent env. turn the
    ## as_var_args call into vars
    ## and as_quo_args into length one quo's
    vals <- quo_get(
        args,
        as_var_args = c("group", "column", "sorting_cols"),
        as_quo_args = c("label", "param", "value"),
        envir = env,
        parent_env = parent_env
    )

    ## remove the "missing" values from vals
    vals <- vals[!sapply(vals, is_missing)]

    dot_subs <- as.list(substitute(substitute(...)))[-1]
    for (dot_name in names(dot_subs)) {
        compare_dot_args_against_formals(dot_arg = dot_name, formals = args)
        vals[[dot_name]] <- tryCatch(
            eval(dot_subs[[dot_name]], envir = env, enclos = parent_env),
            error = function(e) {
                stop(e$message, call. = FALSE)
            }
        )
    }

    vals
}

quo_get <- function(
    args,
    as_var_args = NULL,
    as_quo_args = NULL,
    envir = parent.frame(),
    parent_env = parent.env(envir),
    allow_tidy_select = FALSE
) {
    arg_set <- lapply(args, function(arg) {
        ## try to get arg call
        arg_call <- do.call("substitute", list(as.symbol(arg)), envir = envir)

        if (missing(arg_call)) {
            ## args not defined can quietly return empty expressions.
            return(quote(expr = ))
        } else {
            if (identical(arg_call, rlang::quo()) || identical(arg_call, rlang::quos())) {
                return(arg_call)
            }
            # don't try to eval quosures if it is intended to be a quosure
            if (rlang::is_quosure(arg_call) && arg %in% c(as_quo_args)) {
                arg_call_results <- list(result = arg_call, error = NULL)
            } else {
                # try to safely evaluate arg call
                arg_call_results_envir <- purrr::safely(rlang::eval_tidy)(
                    arg_call,
                    env = envir
                )
                arg_call_results_parent_env <- purrr::safely(rlang::eval_tidy)(
                    arg_call,
                    env = parent_env
                )

                if (is.null(arg_call_results_parent_env$error)) {
                    arg_call_results <- arg_call_results_parent_env
                } else {
                    arg_call_results <- arg_call_results_envir
                }
            }

            ## if expression evaluation was successful, move forward to confirming is correct class and returning the value
            if (is.null(arg_call_results$error)) {
                if (arg %in% c(as_var_args, as_quo_args)) {
                    ## for arg_var_args, we expect not a function. this means arguments can be
                    ## entered such as `col`. convert into final forms respectively
                    if (
                        !(is.function(arg_call_results$result) ||
                            is_basic_list(arg_call_results$result))
                    ) {
                        if (arg %in% as_var_args) {
                            return(as_quosures(arg_call_results$result))
                        } else {
                            return(
                                as_length_one_quo(
                                    arg_call_results$result,
                                    arg = as.character(arg)
                                )
                            )
                        }
                    }
                } else {
                    ## return value as normal if not a var or quo arg
                    return(arg_call_results$result)
                }
            }

            ## Now assume expression evaluation was a failure/returned incorrect value

            ## if not a var or quo and failed, return informative error message
            if (arg %in% c(as_var_args, as_quo_args)) {
                arg_call <- trim_vars_quo_c(arg_call)

                ## check if argcall is tidyselect call, give feedback that is invalid if so
                if (any(purrr::map_lgl(arg_call, is_valid_tidyselect_call))) {
                    if (!allow_tidy_select) {
                        rlang::abort(
                            message = "Tidyselect selection helpers are not acceptable to use in this context. Please provide a specific column to use.",
                            class = "invalid_tidyselect_use"
                        )
                    }
                }

                if (arg %in% as_var_args) {
                    check_var_arg_call_valid(
                        arg_call,
                        arg,
                        allow_tidy_select = allow_tidy_select
                    )
                    arg_val <- as_quosures(rlang::quos(
                        !!!arg_call,
                        .env = envir
                    ))
                } else {
                    arg_val <- as_length_one_quo(
                        rlang::quos(!!!arg_call, .env = envir),
                        arg = as.character(arg)
                    )
                }

                return(arg_val)
            } else {
                rlang::abort(
                    paste0(
                        "Error in evaluating argument `",
                        arg,
                        "`:\n",
                        paste0(" ", arg_call_results$error, collapse = "")
                    ),
                    call = rlang::frame_call(frame = envir)
                )
            }
        }
    })

    names(arg_set) <- args
    arg_set
}


check_var_arg_call_valid <- function(var_list, arg, allow_tidy_select = FALSE) {
    var_list_is_name <- sapply(var_list, is.name)
    var_list_is_tidyselect <- sapply(var_list, is_valid_tidyselect_call)

    if (!all(var_list_is_name | (var_list_is_tidyselect & allow_tidy_select))) {
        new_arg_call <- paste0(
            "vars(",
            paste(sapply(var_list, as.character), collapse = ","),
            ")"
        )

        rlang::abort(
            paste0(
                "Entries for `",
                arg,
                "` argument must be a character vector or unquoted column name.\n",
                "  Consider updating the argument input to `",
                arg,
                "` to:\n\t",
                new_arg_call
            ),
            class = "group_vars_error"
        )
    }
}


trim_vars_quo_c <- function(x) {
    x_list <- as.list(x)
    if (as.character(x_list[[1]]) %in% c("c", "quo", "rlang::quo")) {
        x_list[-1]
    } else {
        list(x)
    }
}

is_basic_list <- function(x) {
    is.list(x) & !rlang::is_quosures(x)
}

is_missing <- function(x) {
    identical(x, quote(expr = ))
}

as_length_one_quo <- function(x, ...) {
    UseMethod("as_length_one_quo", x)
}

#' @export
#' @keywords internal
as_length_one_quo.quosure <- function(x, ...) {
    x
}

#' @export
#' @keywords internal
as_length_one_quo.quosures <- function(x, ..., arg = NULL) {
    if (length(x) == 0) {
        rlang::quo()
    } else {
        if (length(x) > 1) {
            rlang::warn(
                paste0(
                    "Passed more than one quosure to the argument `",
                    arg,
                    "`. Selecting the first entry."
                ),
                class = "quo_greater_length_one"
            )
        }
        x[[1]]
    }
}

#' @export
#' @keywords internal
as_length_one_quo.character <- function(x, ...) {
    rlang::quo(!!rlang::sym(x))
}

as_quosures <- function(x) {
    UseMethod("as_quosures", x)
}

#' @export
#' @keywords internal
as_quosures.quosures <- function(x) {
    x
}

#' @export
#' @keywords internal
as_quosures.quosure <- function(x) {
    rlang::quos(!!x)
}

#' @export
#' @keywords internal
as_quosures.character <- function(x) {
    rlang::quos(
        !!!lapply(x, function(x_val) {
            rlang::quo(!!rlang::sym(x_val))
        })
    )
}

compare_dot_args_against_formals <- function(dot_arg, formals) {
    arg_message <- paste0(
        "Argument '",
        dot_arg,
        "' passed to tfrmt is not a recognized argument."
    )
    fuzzy_arg_match <- agrep(
        dot_arg,
        formals,
        ignore.case = TRUE,
        value = TRUE,
        max.distance = 0.25
    )
    if (length(fuzzy_arg_match)) {
        arg_message <- paste0(
            arg_message,
            "\n",
            "Did you intend to use the argument `",
            fuzzy_arg_match[[1]],
            "`?"
        )
    }

    rlang::inform(
        arg_message,
        class = "tfrmt_unrecognized_argument_inform"
    )
}
