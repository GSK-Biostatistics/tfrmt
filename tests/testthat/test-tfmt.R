test_that("basic tfrmt - title", {

  t_frmt <- tfrmt(
    title = "Table Title"
  )

  expect_s3_class(t_frmt, "tfrmt")

  expect_equal(t_frmt$title, "Table Title")
  expect_equal(t_frmt$group,vars())
  expect_equal(t_frmt$label,quo())
  expect_equal(t_frmt$param,quo())
  expect_equal(t_frmt$values,quo())
  expect_equal(t_frmt$column,vars())

})

test_that("basic tfrmt - selecting group/label/param/values/column - quo", {

  t_frmt <- tfrmt(
    title = "Table Title",
    group = vars(row_label1),
    label = quo(row_label2),
    param = quo(param),
    values = quo(values),
    column = vars(column)
  )

  expect_s3_class(t_frmt,"tfrmt")

  expect_equal( t_frmt$title, "Table Title")
  expect_equal( t_frmt$group, vars(row_label1))
  expect_equal( t_frmt$label, quo(row_label2))
  expect_equal( t_frmt$param, quo(param))
  expect_equal( t_frmt$values, quo(values))
  expect_equal( t_frmt$column, vars(column))
})

test_that("basic tfrmt - selecting group/label/param/values/column - quo into var entries", {

  t_frmt <- tfrmt(
    title = "Table Title",
    group = quo(row_label1),
    label = quo(row_label2),
    param = quo(param),
    values = quo(values),
    column = quo(column)
  )

  expect_s3_class(t_frmt,"tfrmt")

  expect_equal( t_frmt$title, "Table Title")
  expect_equal( t_frmt$group, vars(row_label1))
  expect_equal( t_frmt$label, quo(row_label2))
  expect_equal( t_frmt$param, quo(param))
  expect_equal( t_frmt$values, quo(values))
  expect_equal( t_frmt$column, vars(column))
})

test_that("basic tfrmt - selecting group/label/param/values/column - char", {

  t_frmt <- tfrmt(
    title = "Table Title",
    group = c("row_label1"),
    label = c("row_label2"),
    param = c("param"),
    values = c("values"),
    column = c("column")
  )

  expect_s3_class(t_frmt,"tfrmt")

  expect_equal( t_frmt$title, "Table Title")
  expect_equal( t_frmt$group, vars(row_label1), ignore_attr = TRUE )
  expect_equal( t_frmt$label, quo(row_label2), ignore_attr = TRUE)
  expect_equal( t_frmt$param, quo(param), ignore_attr = TRUE)
  expect_equal( t_frmt$values, quo(values), ignore_attr = TRUE)
  expect_equal( t_frmt$column, vars(column), ignore_attr = TRUE)
})

test_that("basic tfrmt - selecting group/label/param/values/column - bare", {

  t_frmt <- tfrmt(
    title = "Table Title",
    group = c(row_label1, row_label4),
    label = row_label2,
    param = param,
    values = values,
    column = column
  )

  expect_s3_class(t_frmt,"tfrmt")
  expect_equal( t_frmt$title, "Table Title")
  expect_equal( t_frmt$group, vars(row_label1, row_label4), ignore_attr = TRUE)
  expect_equal( t_frmt$label, quo(row_label2), ignore_attr = TRUE)
  expect_equal( t_frmt$param, quo(param), ignore_attr = TRUE)
  expect_equal( t_frmt$values, quo(values), ignore_attr = TRUE)
  expect_equal( t_frmt$column, vars(column), ignore_attr = TRUE)
})

test_that("basic tfrmt - length one quo warning", {

  single_warning <- capture_warnings({
    tfrmt(
      title = "Table Title",
      group = row_label1,
      label = vars(row_label2,row_label3),
      param = param,
      values = values,
      column = column
    )
  })

  expect_equal(
    single_warning,
    paste0(
      "Passed more than one quosure to the argument `",
      "label",
      "`. Selecting the first entry."
    )
  )

  multi_warning <- capture_warnings({
    tfrmt(
      title = "Table Title",
      group = row_label1,
      label = vars(row_label2,row_label3),
      param = vars(param, param2),
      values = vars(values, values2),
      column = vars(column, column2)
    )
  })

  expect_equal(
    multi_warning,
    c(
      "Passed more than one quosure to the argument `label`. Selecting the first entry.",
      "Passed more than one quosure to the argument `param`. Selecting the first entry.",
      "Passed more than one quosure to the argument `values`. Selecting the first entry."
    )
  )

})

test_that("basic tfrmt - bare/char mix error", {

  expect_error(
    tfrmt(
      title = "Table Title",
      group = c(row_label1, "row_label4"),
      label = row_label2,
      param = param,
      values = values,
      column = column
    ),
    paste0(
      "Entries for `",
      "group",
      "` argument must be vars(), a character vector, or unquoted column name.\n",
      "  Consider updating the argument input to `",
      "group",
      "` to:\n\t",
      "vars(row_label1,row_label4)"
    ),
    fixed = TRUE
  )

})

test_that("layering tfrmt - default table elements - func/tfrmt",{

  t_frmt_title <- tfrmt(
    title = "Table Title"
  )

  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(
        subtitle = "Table Subtitle"
    ))

  expect_s3_class(
    t_frmt_layered,
    "tfrmt"
  )

  expect_equal(
    t_frmt_layered$title,
    "Table Title"
  )

  expect_equal(
    t_frmt_layered$subtitle,
    "Table Subtitle"
  )

  expect_equal(t_frmt_layered$group,vars())
  expect_equal(t_frmt_layered$label,quo())
  expect_equal(t_frmt_layered$param,quo())
  expect_equal(t_frmt_layered$values,quo())
  expect_equal(t_frmt_layered$column,vars())

})

test_that("layering tfrmt - select latest table elements",{

  t_frmt_title <- tfrmt(
    title = "Table Title"
  )

  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle"
    ))


  expect_s3_class( t_frmt_layered,"tfrmt"  )
  expect_equal( t_frmt_layered$title, "Table Title 2")
  expect_equal( t_frmt_layered$subtitle, "Table Subtitle")

})

test_that("layering tfrmt - body style elements",{

  t_frmt_title <- tfrmt(
    title = "Table Title",
    body_plan = body_plan(
      frmt_structure(
        group_val = c("group1"),
        label_val = ".default",
        frmt("XXX")
        )
      )
    )


  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(
        body_plan = body_plan(
          frmt_structure(
            group_val = c("group2"),
            label_val = ".default",
            frmt("xx.x")
          )
        )
      )
    )

  expect_s3_class(t_frmt_layered, "tfrmt")

  expect_equal( t_frmt_layered$title, "Table Title")
  expect_equal(t_frmt_layered$body_plan,
               body_plan(frmt_structure(group_val = "group1",
                                              label_val = ".default",
                                              frmt("XXX")),
                               frmt_structure(group_val = "group2",
                                              label_val = ".default",
                                              frmt("xx.x"))))

})

test_that("layering tfrmt - body style elements - multiple",{

  t_frmt_title <- tfrmt(
    title = "Table Title",
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("XXX")
      )
    )
  )

  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(subtitle = "Table Subtitle",
            body_plan = body_plan(
             frmt_structure(group_val = "group2",label_val = ".default", frmt("xx.x")),
             frmt_structure(group_val = "group3",label_val = ".default", frmt("xx.xx"))
             )
           )
    )

  expect_s3_class(t_frmt_layered,"tfrmt")


  expect_equal(t_frmt_layered$title, "Table Title")
  expect_equal(t_frmt_layered$subtitle, "Table Subtitle")

  expect_equal(
    t_frmt_layered$body_plan,
    body_plan(
      frmt_structure(group_val = "group1",label_val = ".default",frmt("XXX")),
      frmt_structure(group_val = "group2",label_val = ".default",frmt("xx.x")),
      frmt_structure(group_val = "group3",label_val = ".default",frmt("xx.xx"))
    )
  )

})

test_that("layering tfrmt - body style elements - join_body_style FALSE",{

  t_frmt_title <- tfrmt(
    title = "Table Title",
    body_plan = body_plan(
      frmt_structure(
        group_val = "group1",
        label_val = ".default",
        frmt("XXX")
      )
    )
  )

  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(subtitle = "Table Subtitle",
            body_plan = body_plan(
             frmt_structure(group_val = "group2",label_val = ".default",frmt("xx.x")),
             frmt_structure(group_val = "group3",label_val = ".default",frmt("xx.xx"))
           )
      ),
      join_body_plans = FALSE
    )


  expect_s3_class(t_frmt_layered,"tfrmt")


  expect_equal(t_frmt_layered$title, "Table Title")
  expect_equal(t_frmt_layered$subtitle, "Table Subtitle")

  expect_equal(
    t_frmt_layered$body_plan,
    body_plan(
      frmt_structure(group_val = "group2",label_val = ".default",frmt("xx.x")),
      frmt_structure(group_val = "group3",label_val = ".default",frmt("xx.xx"))
    )
  )

})

test_that("layering tfrmt - keeping original var/quo",{

  t_frmt_o <- tfrmt(
    title = "Table Title",
    group = c(Group1, Group2),
    label = label1
  )

  t_frmt_layered <- t_frmt_o %>%
    layer_tfrmt(
      tfrmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle"
      ))

  expect_s3_class( t_frmt_layered,"tfrmt"  )
  expect_equal( t_frmt_layered$title, "Table Title 2")
  expect_equal( t_frmt_layered$subtitle, "Table Subtitle")
  expect_equal( t_frmt_layered$group, vars(Group1, Group2), ignore_attr = TRUE )
  expect_equal( t_frmt_layered$label, quo(label1), ignore_attr = TRUE )

})

test_that("layering tfrmt - Mixing var/quo",{

  t_frmt_o <- tfrmt(
    title = "Table Title",
    group = c(Group1, Group2),
    label = label1
  )

  t_frmt_layered <- t_frmt_o %>%
    layer_tfrmt(
      tfrmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle",
        label = label3

      ))

  expect_s3_class( t_frmt_layered,"tfrmt"  )
  expect_equal( t_frmt_layered$title, "Table Title 2")
  expect_equal( t_frmt_layered$subtitle, "Table Subtitle")
  expect_equal( t_frmt_layered$group, vars(Group1, Group2), ignore_attr = TRUE )
  expect_equal( t_frmt_layered$label, quo(label3), ignore_attr = TRUE )

})
