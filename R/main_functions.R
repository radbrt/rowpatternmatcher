#' Match ranges
#' DEPRECATED
#' This is a convenience function that recieves two integers (a, b), and returns the range a:b-1
#' @param start The start of the range
#' @param ends The end of the range, endexclusive
ranges_from_starts_lengths <- function(start, ends) {
  return(start:((ends)-1))
}

#' Ranges from pattern
#' returns row ranges e.g. c(1:4,13:17) based on string of definitions and regex-pattern 
#' @param defstring The collapsed column with definitions - one character per row
#' @param ptn The regex-pattern to find
row_ranges <- function(defstring, ptn) {
  result <- str_locate_all(defstring, ptn)[[1]]
  cstarts <- result[, 'start']
  cends <- result[, 'end']
  ranges <- map2(cstarts, cends, ~ .x:.y)
  ranges
}


#' Match number
#' This is an internal function that returns an array of match-numbers, in the case that 
#' there are several matches in one "string". 
#' @param matchlist Output of the mapped `ranges_from_starts_lengths` function
match_number <- function(matchlist) {
  cr <- c()
  for (i in 1:length(matchlist)) {
    cr <- c(cr, rep(i, length(matchlist[[i]])))
  }
  cr
}

#' Regex row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param ptn Perl-regex pattern to look for, based on the definitions created
#' @param defs Column containing the definition of rows
#' @keywords match_recognize
#' @export
#' @examples
#' \dontrun{
#' regex_row_matcher(stocks, "([D]{4,})", ds)
#' }
regex_row_matcher <- function(df, ptn, defs) {
  defs <- enquo(defs)
  defstring <- paste(pull(df, !!defs), collapse='')
  
  # returns row ranges e.g. c(1:4,13:17) based on string of definitions and regex-pattern 
  ranges <- row_ranges(defstring, ptn)

  ret_df <- subset_from_ranges(df, ranges)
  ret_df$match_number <- match_number(ranges)
  return(ret_df)
  
}


#' Subset a dataframe based on list of possibly overlapping ranges of rows
#' @param df Data frame to subset
#' @param ranges List of possibly overlapping ranges of rows to select
subset_from_ranges <- function(df, ranges) {
  unique_row_numbers <- unique(unlist(ranges))
  df[unique_row_numbers,]
}

# match_rows() kanskje mest talende navnet
#' Row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param definitions Column containing the definition of rows
#' @param rx Simple regex-like statement to filter for - quoted.
#' @keywords match_recognize
#' @export
#' @examples
# ex: match_rows(df, my_definitions_col, "UP{4,} DOWN{4,}")
match_rows <- function(df, definitions, rx) {
  #rx <- sort(unique(as.character(strsplit(rx, ' '))))
  
  # Column definitions
  definitions <- enquo(definitions)
  coldefs <- sort(unique(pull(df, !!definitions)))
  
  # Pattern
  # Extract nicknames/definitions from pattern
  rx_name_ptn <- "[a-zA-Z][a-zA-Z0-9]*"
  rx_names <- str_extract_all(rx, rx_name_ptn)[[1]]
  rx_parsed <- rx
  wildcards <- setdiff(rx_names, unique(coldefs))
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
  
  #print(rx_parsed)
  
  # replace definition column (not column itself, but copy) with single-character versions
  # coldefs <- c("THIS", "whatevz", "wtf") # column
  
  defs_encoded <- pull(df, !!definitions)
  for (i in 1:length(defs_encoded)) {
    defs_encoded[i] <- as.character( match(defs_encoded[i], all_nicks) )
  }

  defstring <- paste(defs_encoded, collapse='')
  
  ranges <- row_ranges(defstring, rx_parsed)

  # Subset data from ranges
  ret_df <- subset_from_ranges(df, ranges)
  
  # Separate column with match-number (like MATCH_NUMBER in MEASURES)
  ret_df$match_number <- match_number(ranges)
  return(ret_df)
  
}



