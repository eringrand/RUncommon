context("convert school years")

# sy_number
test_that("SY - 20 conversion works", {
  expect_equal(sy_number("SY16-17"), 2017)
  expect_equal(sy_number("SY89-90", before_2000 = TRUE), 1990)
  }
)

# Change school year
test_that("change_school_year", {
  expect_equal(change_school_year("2018-2019"), "2018-19")
  expect_error(change_school_year("2018-19"))
  }
)

# sy_form
test_that("sy_from", {
  expect_equal(sy_form(2015), "2014-15")
  expect_equal(sy_form(2015, spring_year = FALSE), "2015-16")
  expect_equal(sy_form("2015", spring_year = FALSE), "2015-16")
  expect_equal(sy_form("2016"), "2015-16")
  }
)

test_that("sy_from string too big", {
  expect_error(sy_form("2015-16"))
  expect_error(sy_form(15))
  }
)


# school_year_from_date
test_that("school_year_from_date", {
  expect_equal(school_year_from_date(date = "2019-05-15"), "2018-19")
  expect_equal(school_year_from_date(date = as.POSIXct("2019-05-15")), "2018-19")
  expect_equal(school_year_from_date(), "2018-19")
  }
)

test_that("school_year_from_date errors", {
  expect_error(school_year_from_date(date = 2019-05-15))
  expect_error(school_year_from_date(date = "2019-13-15"))
  expect_error(school_year_from_date(date = "2019-00-15"))
  expect_error(school_year_from_date(date = "2019-01-32"))
  }
)

