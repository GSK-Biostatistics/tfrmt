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
    group = vars(row_label1),
    label = row_label2,
    param = param,
    values = values,
    column = column
  )

  expect_s3_class(t_fmt,"tfmt")

  expect_equal( t_fmt$title, "Table Title")
  expect_equal( t_fmt$group, vars(row_label1), ignore_attr = TRUE)
  expect_equal( t_fmt$label, quo(row_label2), ignore_attr = TRUE)
  expect_equal( t_fmt$param, quo(param), ignore_attr = TRUE)
  expect_equal( t_fmt$values, quo(values), ignore_attr = TRUE)
  expect_equal( t_fmt$column, quo(column), ignore_attr = TRUE)
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

  t_fmt_layered_y <- t_fmt_title %>%
    layer_tfmt(
      tfmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle"
    ))

  t_fmt_layered_x <- t_fmt_title %>%
    layer_tfmt(
      tfmt(
        title = "Table Title 2",
        subtitle = "Table Subtitle"
      ),
      preferred = "x")

  expect_s3_class( t_fmt_layered_y,"tfmt"  )
  expect_s3_class( t_fmt_layered_x,"tfmt"  )

  expect_equal( t_fmt_layered_y$title, "Table Title 2")
  expect_equal( t_fmt_layered_x$title, "Table Title")

  expect_equal( t_fmt_layered_y$subtitle, "Table Subtitle")
  expect_equal( t_fmt_layered_x$subtitle, "Table Subtitle")

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


  t_fmt_layered_y <- t_fmt_title %>%
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

  t_fmt_layered_x <- t_fmt_title %>%
    layer_tfmt(
      tfmt(
        body_style = element_style(
          fmt_str(
            group = c("group2"),
            fmt("xx.x")
          )
        )
      ),
      preferred = "x"
    )

  expect_s3_class(t_fmt_layered_y, "tfmt")
  expect_s3_class(t_fmt_layered_x, "tfmt")

  expect_equal( t_fmt_layered_y$title, "Table Title")
  expect_equal( t_fmt_layered_x$title, "Table Title")

  expect_equal(t_fmt_layered_y$body_style,
               element_style(fmt_str(group = "group1",
                                     fmt(rounding = "XXX")),
                             fmt_str(group = "group2",
                                     fmt("xx.x"))))

  expect_equal(t_fmt_layered_x$body_style,
               element_style(fmt_str(group = "group2",
                                     fmt("xx.x")),
                             fmt_str(group = "group1",
                                     fmt(rounding = "XXX"))))

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

test_that("layering tfmt - body style elements - override_body_style",{

  t_fmt_title <- tfmt(
    title = "Table Title",
    body_style = element_style(
      fmt_str(
        group_val = "group1",
        fmt(rounding = "XXX")
      )
    )
  )

  t_fmt_layered_y <- t_fmt_title %>%
    layer_tfmt(
      tfmt(subtitle = "Table Subtitle",
           body_style = element_style(
             fmt_str(group_val = "group2",fmt("xx.x")),
             fmt_str(group_val = "group3",fmt("xx.xx"))
           )
      ),
      override_body_style = TRUE
    )

  t_fmt_layered_x <- t_fmt_title %>%
    layer_tfmt(
      tfmt(subtitle = "Table Subtitle",
           body_style = element_style(
             fmt_str(group_val = "group2",fmt("xx.x")),
             fmt_str(group_val = "group3",fmt("xx.xx"))
           )
      ),
      preferred = "x",
      override_body_style = TRUE
    )

  expect_s3_class(t_fmt_layered_y,"tfmt")
  expect_s3_class(t_fmt_layered_x,"tfmt")


  expect_equal(t_fmt_layered_y$title, "Table Title")
  expect_equal(t_fmt_layered_x$title, "Table Title")
  expect_equal(t_fmt_layered_y$subtitle, "Table Subtitle")
  expect_equal(t_fmt_layered_x$subtitle, "Table Subtitle")

  expect_equal(
    t_fmt_layered_y$body_style,
    element_style(
      fmt_str(group_val = "group2",fmt("xx.x")),
      fmt_str(group_val = "group3",fmt("xx.xx"))
    )
  )

  expect_equal(
    t_fmt_layered_x$body_style,
    element_style(
      fmt_str(group_val = "group1",fmt("XXX"))
    )
  )

})


