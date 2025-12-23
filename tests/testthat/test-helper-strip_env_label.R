test_that("strip_env_label() helper works", {
  expect_identical(
    strip_env_label("env:  0x72db165b8"),
    "env:  <env-address>"
  )

  expect_snapshot(
    strip_env_label(
      "<list_of<quosure>>\n\n[[1]]\n<quosure>\nexpr: ^grp2\nenv:  0x72db165b8\n\n[[2]]\n<quosure>\nexpr: ^lbl\nenv:  0x72db31118"
    ) |>
      cat()
  )
})
