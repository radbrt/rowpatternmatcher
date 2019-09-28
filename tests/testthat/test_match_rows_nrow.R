library(dplyr)
library(stringr)
library(purrr)
library(rlang)

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
                 match_rows(defns, " whatevz UP{4,}", wildcards = "whatevz") %>% 
                 nrow(), 77)
})

test_that("match_rows_raw returns number of rows", {
# raw regex 'regex_row_matcher'
expect_equal(stocks %>% 
               filter(ticker=='MSFT') %>% 
               arrange(date) %>% 
               mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
               mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
               match_rows_raw(ds, '([D]{4,})') %>% 
               nrow(), 8)
})

test_that("match_rows_raw returns new column name", {
  # raw regex 'regex_row_matcher'
  expect_equal(stocks %>% 
                 filter(ticker=='MSFT') %>% 
                 arrange(date) %>% 
                 mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
                 mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
                 match_rows_raw(ds, '([D]{4,})', test_number) %>% 
                 names(), c("date", "open", "high", "low", "close", "adj_close", "volume", "ticker", "ds", "test_number"))
})

test_that("match_rows returns new column name", {
  # raw regex 'regex_row_matcher'
  expect_equal(stocks %>% 
                  filter(ticker=='MSFT') %>% 
                  arrange(date) %>% 
                  mutate( defns = 
                            case_when(
                              adj_close>lag(adj_close) ~ 'UP',
                              TRUE ~ 'DOWN'
                            )
                  ) %>% 
                 match_rows(defns, " whatevz UP{4,}", mr_number)  %>% 
                 names(), c("date", "open", "high", "low", "close", "adj_close", "volume", "ticker", "defns", "mr_number"))
})
