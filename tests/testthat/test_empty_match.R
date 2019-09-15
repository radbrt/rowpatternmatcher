library(dplyr)
library(stringr)
library(purrr)
library(rlang)

test_that("Return empty df if no match", {
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
      match_rows(defns, "whatevz UP{6,}DOWN{6,}", mnum, keep_all_rows=TRUE) %>% 
      ungroup() %>% 
      #filter(ticker=='MSFT') %>% 
      nrow(), 0)
  
  expect_equal(
    stocks %>% 
      group_by(ticker)%>% 
      arrange(date) %>% 
      mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
      mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
      match_rows_raw(ds, '([D]{10,}[U]{10,})', mnum) %>% 
      nrow(), 0)
})