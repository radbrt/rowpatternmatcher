test_that("match_rows returns new column name", {
  # raw regex 'regex_row_matcher'
  expect_equal(
stocks %>% 
  group_by(ticker)%>% 
  arrange(date) %>% 
  mutate( defns = 
            case_when(
              adj_close>lag(adj_close) ~ 'UP',
              TRUE ~ 'DOWN'
            )
  ) %>% 
  match_rows(defns, "whatevz UP{4,}", mnum) %>% 
  filter(ticker=='MSFT') %>% 
  nrow(), 77)
})