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
  expect_equal(t_frmt$column,quo())

})

test_that("basic tfrmt - selecting group/label/param/values/column - quo", {

  t_frmt <- tfrmt(
    title = "Table Title",
    group = vars(row_label1),
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
  expect_equal( t_frmt$column, quo(column))
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
  expect_equal( t_frmt$column, quo(column), ignore_attr = TRUE)
})

test_that("basic tfrmt - selecting group/label/param/values/column - bare", {

  skip("Not functional yet")

  t_frmt <- tfrmt(
    title = "Table Title",
    group = vars(row_label1),
    label = row_label2,
    param = param,
    values = values,
    column = column
  )

  expect_s3_class(t_frmt,"tfrmt")

  expect_equal( t_frmt$title, "Table Title")
  expect_equal( t_frmt$group, vars(row_label1), ignore_attr = TRUE)
  expect_equal( t_frmt$label, quo(row_label2), ignore_attr = TRUE)
  expect_equal( t_frmt$param, quo(param), ignore_attr = TRUE)
  expect_equal( t_frmt$values, quo(values), ignore_attr = TRUE)
  expect_equal( t_frmt$column, quo(column), ignore_attr = TRUE)
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
  expect_equal(t_frmt_layered$column,quo())

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
    body_style = table_body_plan(
      frmt_structure(
        group_val = c("group1"),
        frmt("XXX")
        )
      )
    )


  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(
        body_style = table_body_plan(
          frmt_structure(
            group_val = c("group2"),
            frmt("xx.x")
          )
        )
      )
    )

  expect_s3_class(t_frmt_layered, "tfrmt")

  expect_equal( t_frmt_layered$title, "Table Title")
  expect_equal(t_frmt_layered$body_style,
               table_body_plan(frmt_structure(group_val = "group1",
                                              frmt("XXX")),
                               frmt_structure(group_val = "group2",
                                              frmt("xx.x"))))

})

test_that("layering tfrmt - body style elements - multiple",{

  t_frmt_title <- tfrmt(
    title = "Table Title",
    body_style = table_body_plan(
      frmt_structure(
        group_val = "group1",
        frmt("XXX")
      )
    )
  )

  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(subtitle = "Table Subtitle",
           body_style = table_body_plan(
             frmt_structure(group_val = "group2",frmt("xx.x")),
             frmt_structure(group_val = "group3",frmt("xx.xx"))
             )
           )
    )

  expect_s3_class(t_frmt_layered,"tfrmt")


  expect_equal(t_frmt_layered$title, "Table Title")
  expect_equal(t_frmt_layered$subtitle, "Table Subtitle")

  expect_equal(
    t_frmt_layered$body_style,
    table_body_plan(
      frmt_structure(group_val = "group1",frmt("XXX")),
      frmt_structure(group_val = "group2",frmt("xx.x")),
      frmt_structure(group_val = "group3",frmt("xx.xx"))
    )
  )

})

test_that("layering tfrmt - body style elements - join_body_style FALSE",{

  t_frmt_title <- tfrmt(
    title = "Table Title",
    body_style = table_body_plan(
      frmt_structure(
        group_val = "group1",
        frmt("XXX")
      )
    )
  )

  t_frmt_layered <- t_frmt_title %>%
    layer_tfrmt(
      tfrmt(subtitle = "Table Subtitle",
           body_style = table_body_plan(
             frmt_structure(group_val = "group2",frmt("xx.x")),
             frmt_structure(group_val = "group3",frmt("xx.xx"))
           )
      ),
      join_body_styles = FALSE
    )


  expect_s3_class(t_frmt_layered,"tfrmt")


  expect_equal(t_frmt_layered$title, "Table Title")
  expect_equal(t_frmt_layered$subtitle, "Table Subtitle")

  expect_equal(
    t_frmt_layered$body_style,
    table_body_plan(
      frmt_structure(group_val = "group2",frmt("xx.x")),
      frmt_structure(group_val = "group3",frmt("xx.xx"))
    )
  )

})


