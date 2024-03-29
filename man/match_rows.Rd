% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frontend_functions.R
\name{match_rows}
\alias{match_rows}
\title{Row matcher}
\usage{
match_rows(df, definitions, rx, match_name = NULL, wildcards = NULL,
  keep_all_rows = FALSE)
}
\arguments{
\item{df}{Sorted and grouped dataframe to filter}

\item{definitions}{Column containing the definition of rows}

\item{rx}{Simple regex-like statement to filter for - quoted.}

\item{match_name}{Optional column name for match-number. Defaults to NULL, meaning no column will be created.}

\item{wildcards}{keywords (nicknames) in your pattern (`rx`) that should match any rows}

\item{keep_all_rows}{Boolean allows you to return all rows, uncluding nonmatching rows. 
Meaninless if match_name is not set. Default FALSE.}
}
\description{
This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
}
\examples{
\dontrun{
match_rows(msft, change, "UP{4,} DOWN{3,}", match_name=mnum)
}
}
\keyword{match_recognize}
