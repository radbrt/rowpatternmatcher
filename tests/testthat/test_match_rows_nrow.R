# WILDCARD + match_rows
stocks %>% 
  filter(ticker=='MSFT') %>% 
  arrange(date) %>% 
  mutate( defns = 
            case_when(
              adj_close>lag(adj_close) ~ 'UP',
              TRUE ~ 'DOWN'
            )
  ) %>% 
  match_rows(defns, " whatevz UP{4,}") %>% 
  nrow() -> match_row_nrows



# raw regex 'regex_row_matcher'
stocks %>% 
  filter(ticker=='MSFT') %>% 
  arrange(date) %>% 
  mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
  mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
  regex_row_matcher('([D]{4,})', ds) %>% 
  nrow() -> raw_regex_nrow


test_that("match_rows returns number of rows", {
  expect_equal(match_row_nrows, 77L)
  expect_equal(raw_regex_nrow, 8L)
})


