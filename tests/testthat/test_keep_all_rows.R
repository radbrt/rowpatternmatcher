test_that("match_rows works with keep_all_rows", {
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
      match_rows(defns, "whatevz UP{4,}", mnum, keep_all_rows=TRUE) %>% 
      nrow(), nrow(stocks))
  
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
      nrow(),
    stocks %>% 
      group_by(ticker)%>% 
      arrange(date) %>% 
      mutate( defns = 
                case_when(
                  adj_close>lag(adj_close) ~ 'UP',
                  TRUE ~ 'DOWN'
                )
      ) %>% 
      match_rows(defns, "whatevz UP{4,}", mnum, keep_all_rows=TRUE) %>% 
      filter(!is.na(mnum)) %>% 
      nrow())
})


test_that("match_rows_raw works with keep_all_rows", {
  # raw regex 'regex_row_matcher'
  expect_equal(
    stocks %>% 
      group_by(ticker)%>% 
      arrange(date) %>% 
      mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
      mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
      match_rows_raw(ds, '([D]{4,})', mnum,  keep_all_rows=TRUE) %>% 
      #filter(!is.na(mnum)) %>% 
      nrow(),
       nrow(stocks))
  
  expect_equal(
    stocks %>% 
      group_by(ticker)%>% 
      arrange(date) %>% 
      mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
      mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
      match_rows_raw(ds, '([D]{4,})', mnum) %>% 
      nrow(),
    stocks %>% 
      group_by(ticker)%>% 
      arrange(date) %>% 
      mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
      mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
      match_rows_raw(ds, '([D]{4,})', mnum,  keep_all_rows=TRUE) %>% 
      filter(!is.na(mnum)) %>% 
      nrow())
})