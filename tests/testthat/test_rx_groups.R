
test_that("Regex groups in match_rows patterns", {

  expect_equal(
  nrow(match_rows(msft, change, "(UP DOWN){3,}", match_name=mnum)), 12)
})


