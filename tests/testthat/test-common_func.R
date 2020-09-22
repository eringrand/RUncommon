test_that("len is length", {
  expect_equal(len(1:5), length(1:5))
})

test_that("cohort", {
  month_of_year <- format(Sys.Date(), "%m")

  expect_equal(cohort(12), sy_number(school_year_from_date()))
  expect_equal(cohort(12, 2008), 2008)
  expect_equal(cohort(c(12, 11, 10), 2010), c(2010, 2011, 2012))
})

test_that("cohort - error", {
  expect_error(cohort(2018), regexp = "Grade is not a real grade.")
  expect_error(cohort("K"), regexp = "Grade must be a number.")
})


test_that("yes_no", {
  expect_equal(yes_no(c(1, 0, 1, 0)), c("Yes", "No", "Yes", "No"))
  expect_equal(yes_no(c(NA, 0, 1, NA)), c(NA, "No", "Yes", NA))
})

test_that("round_percent", {
  expect_equal(round_percent(.24601), "24.6%")
  expect_equal(round_percent(.24601, dig = 0), "25%")
  expect_equal(round_percent(.24601, 0), "25%")
  expect_equal(round_percent(.58932, dig = 0), scales::percent(.58932))
  expect_error(round_percent(1.2))
})
