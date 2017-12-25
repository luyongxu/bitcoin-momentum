#' ---
#' title: "Explore Google Trends"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#'     fig_width: 8
#'     fig_height: 5
#' ---

#' # 1. Source
source("./R/01-load-packages.R")
download_all <- FALSE

#' # 2. Calibrate Timezone Offset
#' Verify that Google Trends data timezone is calibrated to local time. This requires an offset of 2 hours for users 
#' in EST because the timezone is hardcoded in the gtrendsR package. Compare the last timestamp to current local time 
#' to verify offset is accurate. Verify that the last unixtime matches current UTC time. 
calibrate <- gtrends(keyword = "bitcoin", time = "now 4-H")[["interest_over_time"]] %>% 
  select(datetime = date, hits) %>% 
  mutate(data = "google", 
         datetime = datetime + hours(2), 
         unixtime = as.numeric(datetime)) %>% 
  as_tibble()
tail(calibrate)
print(str_c("Datetime: ", Sys.time()))
print(str_c("Unixtime: ", as.numeric(Sys.time())))

#' # 3. Google Trends Functions 
#' The TXX following the date represents the hours in UTC. The hours are hardcoded to 04:00 UTC time which is equivalent
#' to midnight EST time when daylight savings time is active. 
google_minute <- function(keyword, begin_date, end_date) {
  df <- gtrends(keyword = keyword, time = str_c(begin_date, "T04", " ", end_date, "T04"))[["interest_over_time"]] %>%
    select(datetime = date, hits) %>%
    mutate(data = "google", 
           datetime = datetime + hours(2), 
           unixtime = as.numeric(datetime)) %>%
    as_tibble()
  return(df)
}
google_daily <- function(keyword, begin_date, end_date) { 
  df <- gtrends(keyword = keyword, time = str_c(begin_date, " ", end_date))[["interest_over_time"]] %>% 
    select(date, hits) %>% 
    mutate(date = as.Date(date)) %>% 
    as_tibble()
  return(df)
}

#' # 4. Download Google Trends Minute Data
if (download_all == TRUE) dates <- tibble(dates = ymd("2016-01-01") + days(0:10000)) %>% filter(dates <= Sys.Date() - 1)
if (download_all == FALSE) dates <- tibble(dates = ymd("2017-08-20") + days(0:10000)) %>% filter(dates <= Sys.Date() - 1)
for (day in dates[["dates"]]) {
  begin_date <- as.Date(day)
  end_date <- as.Date(day) + days(1)
  print(str_c("Downloading Google Trends minute data from ", begin_date, " to ", end_date, "."))
  df <- google_minute("bitcoin", begin_date, end_date)
  write_csv(df, str_c("./R/data/google-trends-minute/google-trends-minute-", begin_date, "-", end_date, ".csv"))
  Sys.sleep(1)
}

#' # 5. Download Google Trends Daily Data
if (download_all == TRUE) dates <- tibble(dates = ymd("2011-01-01") + months(0:120)) %>% filter(dates <= Sys.Date())
if (download_all == FALSE) dates <- tibble(dates = ymd("2017-01-01") + months(0:120)) %>% filter(dates <= Sys.Date())
for (month in dates[["dates"]]) { 
  begin_date <- as.Date(month)
  end_date <- as.Date(month) + months(1) - days(1)
  end_date <- as.Date(ifelse(end_date >= Sys.Date(), Sys.Date(), end_date))
  print(str_c("Downloading Google Trends daily data from ", begin_date, " to ", end_date, "."))
  df <- google_daily("bitcoin", begin_date, end_date)
  write_csv(df, str_c("./R/data/google-trends-daily/google-trends-daily-", begin_date, "-", end_date, ".csv"))
  Sys.sleep(1)
}

#' # 6. Download Google Trends Monthly Data
bitcoin_monthly <- google_daily("bitcoin", "2011-01-01", Sys.Date()) %>% 
  rename(hits_monthly = hits)

#' # 7. Clean
rm(calibrate, dates, df, day, begin_date, end_date, month)

#' # 8. Load Google Trends Minute and Daily Data
bitcoin_minute <- list.files("./R/data/google-trends-minute/") %>% 
  map_df(~ read_csv(str_c("./R/data/google-trends-minute/", .), col_types = c("Ticd"))) %>% 
  rename(hits_minute = hits) %>% 
  mutate(datetime = parse_datetime(datetime, locale = locale(tz = "UTC")), 
         day = make_date(year(datetime), month(datetime), day(datetime)), 
         month = make_date(year(datetime), month(datetime), 1))
bitcoin_daily <- list.files("./R/data/google-trends-daily/") %>% 
  map_df(~ read_csv(str_c("./R/data/google-trends-daliy/", .), col_types = c("Di"))) %>% 
  rename(hits_daily = hits)

#' # 9. Rebase
bitcoin_combined <- bitcoin_minute %>% 
  left_join(bitcoin_daily, by = c("day" = "date")) %>% 
  left_join(bitcoin_monthly, by = c("month" = "date")) %>% 
  mutate(hits_daily = hits_daily * hits_monthly / 100, 
         hits_minute = hits_minute * hits_daily / 100)

#' # 10. Plot Google Trends Data
ggplot(bitcoin_combined, aes(x = datetime)) + 
  geom_line(aes(y = hits_minute), colour = "blue")
ggplot(bitcoin_combined, aes(x = datetime)) + 
  geom_line(aes(y = hits_daily), colour = "blue")
ggplot(bitcoin_combined, aes(x = datetime)) + 
  geom_line(aes(y = hits_monthly), colour = "blue")

#' # 11. Download Bitcoin Tick Data From Coinbase
if (download_all == TRUE) {
  download.file(url = "http://api.bitcoincharts.com/v1/csv/coinbaseUSD.csv.gz", 
                destfile = "./R/data/coinbaseUSD.csv.gz")
}
bitcoin_coinbase <- read_csv(file = "./R/data/coinbase/coinbaseUSD.csv.gz", col_names = c("unixtime", "price", "amount")) %>% 
  mutate(data = "bitcoin") %>% 
  filter(price >= 10)

#' # 12. Plot Google Trends Daily and Bitcoin Tick Data
combined <- bind_rows(bitcoin_combined %>% select(unixtime, value = hits_daily, data),
                      bitcoin_coinbase %>% select(unixtime, value = price, data)) %>% 
  mutate(datetime = as.POSIXct(unixtime, origin = "1970-01-01"))
ggplot(combined %>% 
         filter(datetime >= "2017-01-01", datetime <= "2017-07-15"), 
       aes(x = datetime, y = value, colour = data)) + 
  facet_wrap(~ data, ncol = 1, scales = "free_y") + 
  geom_line()
ggplot(combined %>% 
         filter(datetime >= "2017-01-01", datetime <= "2017-07-15") %>% 
         mutate(value = ifelse(data == "google", value * 42, value)), 
       aes(x = datetime, y = value, colour = data)) + 
  geom_line()
ggplot(combined %>% 
         filter(datetime >= "2017-01-01", datetime <= "2017-04-01") %>% 
         mutate(value = ifelse(data == "google", value * 42, value)), 
       aes(x = datetime, y = value, colour = data)) + 
  geom_line()
ggplot(combined %>% 
         filter(datetime >= "2017-04-01", datetime <= "2017-07-15") %>% 
         mutate(value = ifelse(data == "google", value * 42, value)), 
       aes(x = datetime, y = value, colour = data)) + 
  geom_line()

#' # 13. Plot Google Trends Minute and Bitcoin Tick Data
combined <- bind_rows(bitcoin_combined %>% select(unixtime, value = hits_minute, data),
                      bitcoin_coinbase %>% select(unixtime, value = price, data)) %>% 
  mutate(datetime = as.POSIXct(unixtime, origin = "1970-01-01"), 
         value = ifelse(data == "google", roll_meanr(value, 8, fill = NA), value))
ggplot(combined %>% 
         filter(datetime >= "2017-01-01", datetime <= "2017-07-15"), 
       aes(x = datetime, y = value, colour = data)) + 
  facet_wrap(~ data, ncol = 1, scales = "free_y") + 
  geom_line()
ggplot(combined %>% 
         filter(datetime >= "2017-01-01", datetime <= "2017-07-15") %>% 
         mutate(value = ifelse(data == "google", value * 60, value)), 
       aes(x = datetime, y = value, colour = data)) + 
  geom_line(alpha = 0.6)
ggplot(combined %>% 
         filter(datetime >= "2017-01-01", datetime <= "2017-04-01") %>% 
         mutate(value = ifelse(data == "google", value * 60, value)), 
       aes(x = datetime, y = value, colour = data)) + 
  geom_line(alpha = 0.6)
ggplot(combined %>% 
         filter(datetime >= "2017-04-01", datetime <= "2017-07-15") %>% 
         mutate(value = ifelse(data == "google", value * 60, value)), 
       aes(x = datetime, y = value, colour = data)) + 
  geom_line(alpha = 0.6)

