#load(file='../data/stocks.RData')

test_that("match_rows returns number of rows", {
  # WILDCARD + match_rows
  expect_equal(stocks %>% 
                 filter(ticker=='MSFT') %>% 
                 arrange(date) %>% 
                 mutate( defns = 
                           case_when(
                             adj_close>lag(adj_close) ~ 'UP',
                             TRUE ~ 'DOWN'
                           )
                 ) %>% 
                 match_rows(defns, " whatevz UP{4,}") %>% 
                 nrow(), 77)
  
  # raw regex 'regex_row_matcher'
  expect_equal(stocks %>% 
                 filter(ticker=='MSFT') %>% 
                 arrange(date) %>% 
                 mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
                 mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
                 regex_row_matcher('([D]{4,})', ds) %>% 
                 nrow(), 8)
})


