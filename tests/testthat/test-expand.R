test_that("Repo example", {
  foo <- tibble(x1 = c(letters[1:3]),
         x2 = c(1:3)) %>%
    expand(x1, x2)
  expect_equal(foo,
               tibble(x1 = rep(letters[1:3], each = 3),
                      x2 = rep(1:3, times = 3)))
})
