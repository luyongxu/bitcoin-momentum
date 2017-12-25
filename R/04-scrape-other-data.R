#' ---
#' title: "Scrape Other Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#' ---

#' # 1. Function
get_yahoo <- function(ticker) { 
  df <- quantmod::getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = "1900-01-01")
  df <- df %>% 
    as_tibble() %>% 
    mutate(date = index(df), 
           ticker = ticker)
  colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted_close", "date", "ticker")
  return(df)
}

#' # 2. Download VIX
vix <- get_yahoo("^VIX") %>% 
  select(date, vix = adjusted_close)


