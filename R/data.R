#' Stock data for three companies
#' 
#' A dataset containing stock prices for Amazon (AMZN), Microsoft (MSFT) and Apple (AAPL)
#' for the period August 30st 2018 to August 30th 2019. The variables are:
#' 
#' \itemize{
#'   \item date. Trading date (2018-08-30 - 2019-08-30)
#'   \item open. Opening price.
#'   \item high. High price during trading day.
#'   \item low. Low price during trading day.
#'   \item close. Closing price.
#'   \item adj_close. Adjusted closing price.
#'   \item volume. Trade volume.
#'   \item ticker. Name of stock.
#' }
#' 
#' @name stocks
#' @docType data
#' @author Radbrt \email{dev_null@@cerx.co}
#' @references \url{https://finance.yahoo.com/quotes/AMZN,AAPL,MSFT}
#' @keywords datasets
NULL

#' Stock data for Microsoft
#' 
#' A subset of the `stocks` dataset, pre-sorted and with example definition columns for ease of use in examples.
#' The new variables are:
#' 
#' \itemize{
#'   \item change. Character variable cointaining "UP" if `adj_close` is higher than previous day, else "DOWN".
#'   \item change2. Same as `change`, but containing only "U" and "D" for use with `match_rows_raw` function.
#' }
#'
#' @name msft
#' @docType data
#' @author Radbrt \email{dev_null@@cerx.co}
#' @references \url{https://finance.yahoo.com/quotes/AMZN,AAPL,MSFT}
#' @keywords datasets
NULL