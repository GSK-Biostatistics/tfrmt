test_that("basic tfmt - title", {

  t_fmt <- tfmt(
    title = "Table Title"
  )

  expect_s3_class(t_fmt, "tfmt")

  expect_equal(t_fmt$title, "Table Title")
  expect_equal(t_fmt$group,vars())
  expect_equal(t_fmt$label,quo())
  expect_equal(t_fmt$param,quo())
  expect_equal(t_fmt$values,quo())
  expect_equal(t_fmt$column,quo())

})

test_that("basic tfmt - selecting group/label/param/values/column - quo", {

  t_fmt <- tfmt(
    title = "Table Title",
    group = vars(row_label1),
    label = quo(row_label2),
    param = quo(param),
    values = quo(values),
    column = quo(column)
  )

  expect_s3_class(t_fmt,"tfmt")

  expect_equal( t_fmt$title, "Table Title")
  expect_equal( t_fmt$group, vars(row_label1))
  expect_equal( t_fmt$label, quo(row_label2))
  expect_equal( t_fmt$param, quo(param))
  expect_equal( t_fmt$values, quo(values))
  expect_equal( t_fmt$column, quo(column))
})

test_that("basic tfmt - selecting group/label/param/values/column - char", {

  t_fmt <- tfmt(
    title = "Table Title",
    group = c("row_label1"),
    label = c("row_label2"),
    param = c("param"),
    values = c("values"),
    column = c("column")
  )

  expect_s3_class(t_fmt,"tfmt")

  expect_equal( t_fmt$title, "Table Title")
  expect_equal( t_fmt$group, vars(row_label1), ignore_attr = TRUE )
  expect_equal( t_fmt$label, quo(row_label2), ignore_attr = TRUE)
  expect_equal( t_fmt$param, quo(param), ignore_attr = TRUE)
  expect_equal( t_fmt$values, quo(values), ignore_attr = TRUE)
  expect_equal( t_fmt$column, quo(column), ignore_attr = TRUE)
})

test_that("basic tfmt - selecting group/label/param/values/column - bare", {

  t_fmt <- tfmt(
    title = "Table Title",
    group = c(row_label1, row_label4),
    label = row_label2,
    param = param,
    values = values,
    column = column
  )

  expect_s3_class(t_fmt,"tfmt")

  expect_equal( t_fmt$title, "Table Title")
  expect_equal( t_fmt$group, vars(row_label1, row_label4), ignore_attr = TRUE)
  expect_equal( t_fmt$label, quo(row_label2), ignore_attr = TRUE)
  expect_equal( t_fmt$param, quo(param), ignore_attr = TRUE)
  expect_equal( t_fmt$values, quo(values), ignore_attr = TRUE)
  expect_equal( t_fmt$column, quo(column), ignore_attr = TRUE)

})

test_that("basic tfmt - length one quo warning", {

  single_warning <- capture_warnings({
    tfmt(
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
    tfmt(
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
      "Passed more than one quosure to the argument `values`. Selecting the first entry.",
      "Passed more than one quosure to the argument `column`. Selecting the first entry."
    )
  )

})

test_that("basic tfmt - bare/char mix error", {

  expect_error(
    tfmt(
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

test_that("layering tfmt - default table elements - func/tfmt",{

  t_fmt_title <- tfmt(
    title = "Table Title"
  )

  t_fmt_layered <- t_fmt_title %>%
    layer_tfmt(
      tfmt(
        subtitle = "Table Subtitle"
    ))

  expect_s3_class(
    t_fmt_layered,
    "tfmt"
  )

  expect_equal(
    t_fmt_layered$title,
    "Table Title"
  )

  expect_equal(
    t_fmt_layered$subtitle,
    "Table Subtitle"
  )

  expect_equal(t_fmt_layered$group,vars())
  expect_equal(t_fmt_layered$label,quo())
  expect_equal(t_fmt_layered$param,quo())
  expect_equal(t_fmt_layered$values,quo())
  expect_equal(t_fmt_layered$column,quo())

})

test_that("layering tfmt - select latest table elements",{

  t_fmt_title <- tfmt(
    title = "Table Title"
  )

  t_fmt_layered <- t_fmt_title %>%
    layer_tfmt(
      tfmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle"
    ))


  expect_s3_class( t_fmt_layered,"tfmt"  )
  expect_equal( t_fmt_layered$title, "Table Title 2")
  expect_equal( t_fmt_layered$subtitle, "Table Subtitle")

})

test_that("layering tfmt - body style elements",{

  t_fmt_title <- tfmt(
    title = "Table Title",
    body_style = element_style(
      fmt_str(
        group = c("group1"),
        fmt(rounding = "XXX")
        )
      )
    )


  t_fmt_layered <- t_fmt_title %>%
    layer_tfmt(
      tfmt(
        body_style = element_style(
          fmt_str(
            group = c("group2"),
            fmt("xx.x")
          )
        )
      )
    )

  expect_s3_class(t_fmt_layered, "tfmt")

  expect_equal( t_fmt_layered$title, "Table Title")
  expect_equal(t_fmt_layered$body_style,
               element_style(fmt_str(group = "group1",
                                     fmt(rounding = "XXX")),
                             fmt_str(group = "group2",
                                     fmt("xx.x"))))

})

test_that("layering tfmt - body style elements - multiple",{

  t_fmt_title <- tfmt(
    title = "Table Title",
    body_style = element_style(
      fmt_str(
        group = "group1",
        fmt(rounding = "XXX")
      )
    )
  )

  t_fmt_layered <- t_fmt_title %>%
    layer_tfmt(
      tfmt(subtitle = "Table Subtitle",
           body_style = element_style(
             fmt_str(group = "group2",fmt("xx.x")),
             fmt_str(group = "group3",fmt("xx.xx"))
             )
           )
    )

  expect_s3_class(t_fmt_layered,"tfmt")


  expect_equal(t_fmt_layered$title, "Table Title")
  expect_equal(t_fmt_layered$subtitle, "Table Subtitle")

  expect_equal(
    t_fmt_layered$body_style,
    element_style(
      fmt_str(group_val = "group1",fmt("XXX")),
      fmt_str(group_val = "group2",fmt("xx.x")),
      fmt_str(group_val = "group3",fmt("xx.xx"))
    )
  )

})

test_that("layering tfmt - body style elements - join_body_style FALSE",{

  t_fmt_title <- tfmt(
    title = "Table Title",
    body_style = element_style(
      fmt_str(
        group_val = "group1",
        fmt(rounding = "XXX")
      )
    )
  )

  t_fmt_layered <- t_fmt_title %>%
    layer_tfmt(
      tfmt(subtitle = "Table Subtitle",
           body_style = element_style(
             fmt_str(group_val = "group2",fmt("xx.x")),
             fmt_str(group_val = "group3",fmt("xx.xx"))
           )
      ),
      join_body_styles = FALSE
    )


  expect_s3_class(t_fmt_layered,"tfmt")


  expect_equal(t_fmt_layered$title, "Table Title")
  expect_equal(t_fmt_layered$subtitle, "Table Subtitle")

  expect_equal(
    t_fmt_layered$body_style,
    element_style(
      fmt_str(group_val = "group2",fmt("xx.x")),
      fmt_str(group_val = "group3",fmt("xx.xx"))
    )
  )

})

test_that("layering tfmt - keeping original var/quo",{

  t_fmt_o <- tfmt(
    title = "Table Title",
    group = c(Group1, Group2),
    label = label1
  )

  t_fmt_layered <- t_fmt_o %>%
    layer_tfmt(
      tfmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle"
      ))

  expect_s3_class( t_fmt_layered,"tfmt"  )
  expect_equal( t_fmt_layered$title, "Table Title 2")
  expect_equal( t_fmt_layered$subtitle, "Table Subtitle")
  expect_equal( t_fmt_layered$group, vars(Group1, Group2), ignore_attr = TRUE )
  expect_equal( t_fmt_layered$label, quo(label1), ignore_attr = TRUE )

})

test_that("layering tfmt - Mixing var/quo",{

  t_fmt_o <- tfmt(
    title = "Table Title",
    group = c(Group1, Group2),
    label = label1
  )

  t_fmt_layered <- t_fmt_o %>%
    layer_tfmt(
      tfmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle",
        label = label3

      ))

  expect_s3_class( t_fmt_layered,"tfmt"  )
  expect_equal( t_fmt_layered$title, "Table Title 2")
  expect_equal( t_fmt_layered$subtitle, "Table Subtitle")
  expect_equal( t_fmt_layered$group, vars(Group1, Group2), ignore_attr = TRUE )
  expect_equal( t_fmt_layered$label, quo(label3), ignore_attr = TRUE )

})
