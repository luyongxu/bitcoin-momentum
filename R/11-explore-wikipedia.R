#' ---
#' title: "Explore Wikipedia"
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

#' # 2. Wikipedia
bitcoin_wiki <-article_pageviews(project = "en.wikipedia.org", 
                                 article = "Bitcoin", 
                                 platform = "all", 
                                 start = "2015070100", 
                                 end = "2017083000") %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

print("Downloading bitcoin exchange rate data.")
bitcoin_price <- Quandl("BCHARTS/BITSTAMPUSD") %>% 
  arrange(Date) %>% 
  as_tibble()
colnames(bitcoin_price) <- c("date", "open", "high", "low", "close", "volume_btc", "volume_currency", "weighted_price")


combined <- bitcoin_price %>% 
  left_join(bitcoin_wiki) %>% 
  mutate(views_change_90d = views / lag(views, 90) - 1)
ggplot(bitcoin_wiki, aes(x = date, y = views)) + 
  geom_line()
ggplot(combined %>% filter(date >= "2015-07-01"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = views / 15), colour = "red") + 
  scale_y_log10()


ggplot(combined %>% filter(date >= "2015-07-01"), aes(x = date, y = close, size = views)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  scale_y_log10() + 
  scale_colour_gradient(low = "white", high = "black")
ggplot(combined %>% filter(date >= "2015-07-01"), aes(x = date, y = close, colour = views)) + 
  geom_point() + 
  scale_y_log10() + 
  scale_colour_gradient(low = "white", high = "black")
ggplot(combined %>% filter(date >= "2015-07-01"), aes(x = date, y = close, colour = views_change_90d)) + 
  geom_point(size = 1.5) + 
  scale_y_log10() + 
  scale_colour_gradient(low = "gray", high = "blue")
