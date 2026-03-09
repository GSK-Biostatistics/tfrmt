test_that("extract_data works for a single gt_tbl object", {

data_demog_test <- data_demog |>
  filter(rowlbl1 %in% c("Age (y)" , "Sex"),
         column != "p-value")

tfrmt_dem <- tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
                                                                                n = frmt("xxx"),
                                                                                pct = frmt_when(
                                                                                  "==100" ~ "",
                                                                                  "==0" ~ "",
                                                                                  TRUE ~ frmt("(xx.x %)")
                                                                                )
    )),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min", "Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx"))
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  )
  )|>
  print_to_gt(data_demog_test)

tfrmt_data_extracted <- extract_data(tfrmt_dem)
tfrmt_data_manual <- tfrmt_dem[["_data"]]

expect_s3_class(tfrmt_data_extracted, "data.frame")
expect_equal(tfrmt_data_extracted, tfrmt_data_manual)
expect_true(nrow(tfrmt_data_extracted) > 0)
})



test_that("extract_data works for a gt_group object (paged tables)", {

data_demog_test <- data_demog |>
    filter(rowlbl1 %in% c("Age (y)" , "Sex"),
           column != "p-value")

tfrmt_paged <- tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine(
        "{n} {pct}",
        n = frmt("xxx"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default",
      label_val = "n",
      frmt("xxx")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = c("Mean", "Median", "Min", "Max"),
      frmt("xxx.x")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = "SD",
      frmt("xxx.xx")
    )
  ),
  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  ),
  page_plan = page_plan(
    page_structure(
      group_val = list(
        rowlbl1 = ".default"
      )
    ),
    note_loc = "source_note"
  )
) |>
  print_to_gt(data_demog_test)

#  Extract using function vs manual (using purrr::map)
gt_group_data_extracted <- extract_data(tfrmt_paged)
gt_group_manual <- purrr::map(tfrmt_paged$gt_tbls$gt_tbl, ~ .x[["_data"]])

#  Assertions
expect_type(gt_group_data_extracted, "list")
expect_length(gt_group_data_extracted, length(gt_group_manual))
expect_equal(gt_group_data_extracted, gt_group_manual)

# Check that each element in the list is a data frame
expect_s3_class(gt_group_data_extracted[[1]], "data.frame")
})

test_that("extract_data throws an error for invalid inputs", {
  expect_error(extract_data(iris), "Input must be a 'gt_tbl' or 'gt_group' object")
  expect_error(extract_data(NULL), "Input must be a 'gt_tbl' or 'gt_group' object")
})
