#' Regex row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param definitions Column containing the definition of rows
#' @param rx Perl-regex pattern to look for, based on the definitions created
#' @param match_name Optional column name for match-number. Defaults to NULL, meaning no column will be created.
#' @param keep_all_rows Boolean allows you to return all rows, uncluding nonmatching rows. 
#' Meaninless if match_name is not set. Default FALSE.
#' @keywords match_recognize
#' @importFrom dplyr stringr purrr rlang
#' @export
#' @examples
#' \dontrun{
#' match_rows_raw(msft, change2, "([D]{4,})", mnum)
#' }
match_rows_raw <- function(df, definitions, rx, match_name=NULL, keep_all_rows=FALSE) {
  definitions <- rlang::enquo(definitions)
  match_name <- rlang::enquo(match_name)
  group_modify(df, ~ match_partition_raw(df=.x, definitions, rx, match_name, keep_all_rows)) 
}


#' Row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param definitions Column containing the definition of rows
#' @param rx Simple regex-like statement to filter for - quoted.
#' @param match_name Optional column name for match-number. Defaults to NULL, meaning no column will be created.
#' @param keep_all_rows Boolean allows you to return all rows, uncluding nonmatching rows. 
#' Meaninless if match_name is not set. Default FALSE.
#' @keywords match_recognize
#' @importFrom dplyr stringr purrr rlang
#' @export
#' @examples
#' \dontrun{
#' match_rows(msft, change, "UP{4,} DOWN{3,}", match_name=mnum)
#' }
match_rows <- function(df, definitions, rx, match_name=NULL, keep_all_rows=FALSE) {
  definitions <- rlang::enquo(definitions)
  match_name <- rlang::enquo(match_name)
  group_modify(df, ~ match_partition(df=.x, definitions, rx, match_name, keep_all_rows)) 
}


#' Conditional aggregation
#'
#' Aggregate a column where some condition is true - meant to be used inside tidy functions. 
#' This is not even a convenience function as it can be done simpler in native tidyverse (se examples).
#' The function exists in order to highlight a programming pattern.
#' @param fun The aggregation function to run
#' @param var The variable to run the function on
#' @param cond The condition to filter on
#' @keywords aggregation
#' @export
#' @examples
#' \dontrun{
#' mutate(msft, fincol = aggregate_where(mean, adj_close, change=="UP") )
#' 
#' # Equivalent to
#' mutate(msft, fincol = mean(adj_close[change=="UP"] ))
#' }
#' 
aggregate_where <- function(fun, var, cond) {
  fun(var[cond])
}


