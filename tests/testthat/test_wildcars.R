test_that("wildcards", {
  
  expect_gt(
    msft %>% 
      match_rows(change, "11 UP DOWN{3,}", match_name=mnum, wildcards = c("11")) %>% nrow(),
  0)
  
  
})

