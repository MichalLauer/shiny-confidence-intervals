test_that("Normal parsing works", {
  # Single match
  expect_equal(
    object = text_to_distr("Normal()")$strprint(),
    expected = "Norm(mean = 0, var = 1)"
  )
  expect_equal(
    object = text_to_distr("Normal(mean = 1)")$strprint(),
    expected = "Norm(mean = 1, var = 1)"
  )
  expect_equal(
    object = text_to_distr("Normal(var = 1)")$strprint(),
    expected = "Norm(mean = 0, var = 1)"
  )
  expect_equal(
    object = text_to_distr("Normal(sd = 1)")$strprint(),
    expected = "Norm(mean = 0, sd = 1)"
  )
  expect_equal(
    object = text_to_distr("Normal(mean = 321, var = 88)")$strprint(),
    expected = "Norm(mean = 321, var = 88)"
  )
  expect_equal(
    object = text_to_distr("Normal(mean = 123, sd = 22)")$strprint(),
    expected = "Norm(mean = 123, sd = 22)"
  )
})
