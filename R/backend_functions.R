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
  
  defstring <- paste(pull(df, !!definitions), collapse='')
  
  # returns row ranges e.g. c(1:4,13:17) based on string of definitions and regex-pattern 
  ranges <- row_ranges(defstring, rx)
  
  # If ranges shows no match, return early
  if( length(ranges)==0 ) {
    nulldf <- df
    nulldf[, quo_name(match_name)] = NA
    return(nulldf[NULL,])
  }
  
  ret_df <- df
  
  #Separate column with match-number (like MATCH_NUMBER in MEASURES)
  if (!quo_is_null(match_name) ) {
    ret_df[unique(unlist(ranges)), quo_name(match_name)] = match_number(ranges)
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
#' @param match_name Optional column name for match-number, defaults to NULL.
#' @param keep_all_rows Boolean allows you to return all rows, uncluding nonmatching rows. 
#' Meaninless if not match_name is set. Default FALSE.
match_partition <- function(df, definitions, rx, match_name=NULL, keep_all_rows=FALSE) {
  
  coldefs <- sort(unique(pull(df, !!definitions)))
  
  # Pattern
  # Extract nicknames/definitions from pattern
  rx_name_ptn <- "[a-zA-Z][a-zA-Z0-9]*"
  rx_names <- str_extract_all(rx, rx_name_ptn)[[1]]
  rx_parsed <- rx
  wildcards <- setdiff(rx_names, coldefs)
  #print(wildcards)
  if (length(wildcards)>0) {
    for (w in wildcards) {
      rx_parsed <- str_replace_all(rx_parsed, w, '\\.')
    }
  }
  
  # List of all definitions
  all_nicks <- sort(unique(c(rx_names, coldefs)))
  #all_nicks <- setdiff(all_nicks, wildcards)
  if(length(all_nicks)>10) stop("There are more than 10 different definitions, we are not able to handle that yet...")
  
  
  # replace regex with appreviated version
  for (a in all_nicks) {
    rx_parsed <- str_replace_all(rx_parsed, a, as.character(match(a, all_nicks)))
  }
  
  
  # spaces are great when writing pseudoregex, need to remove before parsing as real regex
  rx_parsed <- str_replace_all(rx_parsed, ' ', '')
  
  # replace definition column (not column itself, but copy) with single-character versions
  # coldefs <- c("THIS", "whatevz", "wtf") # column
  defs_encoded <- pull(df, !!definitions)
  for (i in 1:length(defs_encoded)) {
    defs_encoded[i] <- as.character( match(defs_encoded[i], all_nicks) )
  }
  

  defstring <- paste(defs_encoded, collapse='')
  ranges <- row_ranges(defstring, rx_parsed)
  
  # If ranges shows no match, return early
  if( length(ranges)==0 ) {
    nulldf <- df
    nulldf[, quo_name(match_name)] = NA
    return(nulldf[NULL,])
  }

  ret_df <- df
  
  #Separate column with match-number (like MATCH_NUMBER in MEASURES)
  if (!quo_is_null(match_name) ) {
    ret_df[unique(unlist(ranges)), quo_name(match_name)] = match_number(ranges)
  }
  
  if(!keep_all_rows) {
    ret_df <- subset_from_ranges(ret_df, ranges)
  }

  return(ret_df)
}
