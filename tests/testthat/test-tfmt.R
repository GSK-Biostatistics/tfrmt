test_that("basic tfmt", {

  t_fmt <- tfmt(
    title = "Table Title"
  )

  expect_s3_class(
    t_fmt,
    "tfmt"
  )

  expect_equal(
    t_fmt$title,
    "Table Title"
  )

})

test_that("layering tfmt - default table elements",{

  t_fmt <- tfmt(
    title = "Table Title"
  )

  t_fmt_layered <- tfmt(
    t_fmt,
    subtitle = "Table Subtitle"
  )

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

})

test_that("layering tfmt - select latest table elements",{

  t_fmt <- tfmt(
    title = "Table Title"
  )

  t_fmt_layered <- tfmt(
    t_fmt,
    title = "Table Title 2",
    subtitle = "Table Subtitle"
  )

  expect_s3_class(
    t_fmt_layered,
    "tfmt"
  )

  expect_equal(
    t_fmt_layered$title,
    "Table Title 2"
  )

  expect_equal(
    t_fmt_layered$subtitle,
    "Table Subtitle"
  )

})

test_that("layering tfmt - body style elements",{

  t_fmt <- tfmt(
    title = "Table Title",
    body_style = element_style(
      fmt_str(
        group = c("Age", "Weight"),
        label = "n",
        fmt(rounding = "XXX")
      )
    )
  )

  t_fmt_layered <- tfmt(
    t_fmt,
    subtitle = "Table Subtitle",
    body_style = element_style(
      fmt_str(
        group = c("Age", "Weight"),
        label = "Mean",
        fmt("xx.x")
      )
    )
  )


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

  expect_equal(
    t_fmt_layered$body_style,
    element_style(
      fmt_str(
        group = c("Age", "Weight"),
        label = "n",
        fmt(rounding = "XXX")
      ),
      fmt_str(
        group = c("Age", "Weight"),
        label = "Mean",
        fmt("xx.x")
      )
    )
  )

})

test_that("layering tfmt - body style elements - multiple",{

  t_fmt <- tfmt(
    title = "Table Title",
    body_style = element_style(
      fmt_str(
        group = c("Age", "Weight"),
        label = "n",
        fmt(rounding = "XXX")
      )
    )
  )

  t_fmt_layered <- tfmt(
    t_fmt,
    subtitle = "Table Subtitle",
    body_style = element_style(
      fmt_str(
        group = c("Age", "Weight"),
        label = "Mean",
        fmt("xx.x")
      ),
      fmt_str(
        group = c("Age", "Weight"),
        label = "Median",
        fmt("xx.x")
      )
    )
  )

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

  expect_equal(
    t_fmt_layered$body_style,
    element_style(
      fmt_str(
        group = c("Age", "Weight"),
        label = "n",
        fmt(rounding = "XXX")
      ),
      fmt_str(
        group = c("Age", "Weight"),
        label = "Mean",
        fmt("xx.x")
      ),
      fmt_str(
        group = c("Age", "Weight"),
        label = "Median",
        fmt("xx.x")
      )
    )
  )

})

