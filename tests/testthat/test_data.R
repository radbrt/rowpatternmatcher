test_that("Date format for testdata", {
  expect_equal(class(msft$date), c("POSIXct", "POSIXt"))
  expect_equal(class(stocks$date), c("POSIXct", "POSIXt"))
  expect_equal(nrow(msft), 252)
  expect_equal(nrow(stocks), 756)
})

