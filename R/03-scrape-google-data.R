#' ---
#' title: "Scrape Google Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#' ---

#' # 1. Set Download Flag
download_all <- FALSE

#' # 2. Create Date Set
if (download_all == TRUE) dates <- tibble(dates = ymd("2011-01-01") + months(0:120)) %>% filter(dates <= Sys.Date())
if (download_all == FALSE) dates <- tibble(dates = ymd("2017-01-01") + months(0:120)) %>% filter(dates <= Sys.Date())

#' # 3. Google Trends Function
google_trends <- function(query, begin_date, end_date) { 
  df <- gtrends(keyword = "bitcoin", time = str_c(begin_date, " ", end_date))[["interest_over_time"]] %>% 
    select(date, hits) %>% 
    mutate(date = as.Date(date)) %>% 
    as_tibble()
  return(df)
}

#' # 4. Download Google Trends Daily Data
for (month in dates[["dates"]]) { 
  begin_date <- as.Date(month)
  end_date <- as.Date(month) + months(1) - days(1)
  end_date <- as.Date(ifelse(end_date >= Sys.Date(), Sys.Date(), end_date))
  print(str_c("Downloading Google Trends data from ", begin_date, " to ", end_date, "."))
  df <- google_trends("bitcoin", begin_date, end_date)
  write_csv(df, str_c("./data/google-trends-daily/google-trends-daily-", begin_date, "-", end_date, ".csv"))
  Sys.sleep(1)
}

#' # 5. Download Google Trends Monthly Data
monthly <- google_trends("bitcoin", "2011-01-01", Sys.Date()) %>% 
  rename(hits_monthly = hits)

#' # 6. Rebase
bitcoin_google <- list.files("./data/google-trends-daily/") %>% 
  map_df(~ read_csv(str_c("./data/google-trends-daily/", .), col_types = c("Di"))) %>% 
  rename(hits_daily = hits) %>% 
  left_join(monthly) %>% 
  mutate(hits_monthly = na.locf(hits_monthly), 
         hits_daily = hits_daily * hits_monthly / 100)
         
#' # 7. Clean
rm(download_all, dates, google_trends, month, begin_date, end_date, df, monthly)
