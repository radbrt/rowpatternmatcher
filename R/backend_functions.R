#' Regex row matcher per partition
#'
#' This function is meant to be called from match_rows_raw which handles partitions.
#' The function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param rx Perl-regex pattern to look for, based on the definitions created
#' @param definitions Column containing the definition of rows
#' @param match_name Optional column name for match-number, defaults to NULL.
#' @param keep_all_rows Boolean allows you to return all rows, uncluding nonmatching rows. 
#' Meaninless if not match_name is set. Default FALSE.
match_partition_raw <- function(df, definitions, rx, match_name=NULL, keep_all_rows=FALSE) {
  
  defstring <- paste(dplyr::pull(df, !!definitions), collapse='')
  
  # returns row ranges e.g. c(1:4,13:17) based on string of definitions and regex-pattern 
  ranges <- row_ranges(defstring, rx)
  
  # If ranges shows no match, return early
  if( length(ranges)==0 ) {
    nulldf <- df
    nulldf[, rlang::quo_name(match_name)] = NA
    return(nulldf[NULL,])
  }
  
  ret_df <- df
  
  #Separate column with match-number (like MATCH_NUMBER in MEASURES)
  if (!rlang::quo_is_null(match_name) ) {
    ret_df[unique(unlist(ranges, use.names = FALSE)), rlang::quo_name(match_name)] = match_number(ranges)
  }

  if(!keep_all_rows) {
    ret_df <- subset_from_ranges(ret_df, ranges)
  }

  
  return(ret_df)
}




#' Match rows per group
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' The function is meant to be called from match_rows, due to quosures it wont work standalone.
#' It is, however, fairly simple to call this from another wrapper function as long as it takes care of quoting.
#' @param df Sorted dataframe to filter (this will often be a partition from a group_by statement)
#' @param definitions Column containing the definition of rows
#' @param rx Simple regex-like statement to filter for - quoted.
#' @param all_nicks All unique elements in `definitions` column
#' @param match_name Optional column name for match-number, defaults to NULL.
#' @param keep_all_rows Boolean allows you to return all rows, uncluding nonmatching rows. 
#' Meaninless if not match_name is set. Default FALSE.
match_partition <- function(df, definitions, rx, all_nicks, match_name=NULL, keep_all_rows=FALSE) {

  # replace definition column (not column itself, but copy) with single-character versions
  # definitions <- c("THIS", "whatevz", "wtf") # column
  # this gives error if pattern include non-occurring definition TODO: add pattern defs to list
  defs_encoded <- purrr::map_chr(dplyr::pull(df, !!definitions), match, all_nicks)
  
  defstring <- paste(defs_encoded, collapse='')
  ranges <- row_ranges(defstring, rx)
  
  ret_df <- df
  
  # If ranges shows no match, return early
  if( length(ranges)==0 ) {
    ret_df[, rlang::quo_name(match_name)] = NA
    return(ret_df[NULL,])
  }
  
  
  #Separate column with match-number (like MATCH_NUMBER in MEASURES)
  if (!rlang::quo_is_null(match_name) ) {
    ret_df[unique(unlist(ranges, use.names = FALSE)), rlang::quo_name(match_name)] = match_number(ranges)
  }
  
  if(!keep_all_rows) {
    ret_df <- subset_from_ranges(ret_df, ranges)
  }
  
  return(ret_df)
}



