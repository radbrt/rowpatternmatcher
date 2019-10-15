
test_that("wildcards are wild", {
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
      match_rows(defns, "whatevz*? UP{4,}", mnum, wildcards=c("whatevz")) %>% 
      filter(ticker=='MSFT') %>% 
      filter(mnum==12) %>% 
      nrow(), 10)
})

test_that("wildcards are greedy", {
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
      match_rows(defns, "whatevz* UP{4,}", mnum, wildcards=c("whatevz")) %>% 
      filter(ticker=='MSFT') %>% 
      filter(mnum==1) %>% 
      nrow(), 225)

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
      match_rows(defns, "whatevz* UP{4,}", mnum, wildcards=c("whatevz")) %>% 
      filter(ticker=='MSFT') %>% 
      filter(mnum==2) %>% 
      nrow(), 0)
  
  expect_gt(
    msft %>% 
      match_rows(change, "11 UP DOWN{3,}", match_name=mnum, wildcards = c("11")) %>% nrow(),
    0)

})










