#' ---
#' title: "Plot Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#' ---

#' # 1. Source
source("./R/05-engineer-features.R")

#' # 2. Close Change
ggplot(train, aes(x = close_change_02d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 0.0025, alpha = 0.5, position = "identity") + 
  coord_cartesian(xlim = c(-0.20, 0.20))
ggplot(train, aes(x = close_change_02d, y = future_return)) + 
  geom_point(alpha = 0.25, colour = "blue") + 
  coord_cartesian(xlim = c(-0.20, 0.20), ylim = c(-0.10, 0.10)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_smooth(colour = "red")
ggplot(train, aes(x = close_change_16d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") + 
  coord_cartesian(xlim = c(-0.50, 0.50))
ggplot(train, aes(x = close_change_16d, y = future_return)) + 
  geom_point(alpha = 0.25, colour = "blue") + 
  coord_cartesian(xlim = c(-0.50, 0.50), ylim = c(-0.10, 0.10)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_smooth(colour = "red")
ggplot(train, aes(x = close_change_57d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") + 
  coord_cartesian(xlim = c(-0.80, 0.80))
ggplot(train, aes(x = close_change_57d, y = future_return)) + 
  geom_point(alpha = 0.25, colour = "blue") + 
  coord_cartesian(xlim = c(-0.50, 0.50), ylim = c(-0.10, 0.10)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_smooth(colour = "red")

#' # 3. Close Drawdown
ggplot(train, aes(x = close_drawdown, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity")

#' # 4. Close Standard Deviation
ggplot(train, aes(x = close_sd_10d, fill = factor(future_return_sign))) +
  geom_histogram(binwidth = 0.001, position = "identity", alpha = 0.5)
ggplot(train, aes(x = close_sd_90d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 0.001, position = "identity", alpha = 0.5)

#' # 5. Close Positive and Negative Streak 
ggplot(train, aes(x = close_positive_streak, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5)
ggplot(train, aes(x = close_negative_streak, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5)

#' # 6. Number of Positive Days
ggplot(train, aes(x = close_positive_26d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5)
ggplot(train, aes(x = close_positive_63d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5)

#' # 7. Google Trends Level
ggplot(train, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_line(aes(y = hits_daily * 35), colour = "red", alpha = 0.6)
ggplot(train %>% filter(date >= "2017-01-01"), aes(x = date )) + 
  geom_line(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_line(aes(y = hits_daily * 47), colour = "red", alpha = 0.6) + 
  geom_point(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_point(aes(y = hits_daily * 47), colour = "red", alpha = 0.6)
ggplot(train %>% filter(date >= "2016-01-01", date <= "2017-01-01"), aes(x = date )) + 
  geom_line(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_line(aes(y = hits_daily * 47), colour = "red", alpha = 0.6) + 
  geom_point(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_point(aes(y = hits_daily * 47), colour = "red", alpha = 0.6)

#' # 8. Google Trends Change
ggplot(train, aes(x = hits_daily_01d, y = future_return)) + 
  geom_point(alpha = 0.2)
ggplot(train, aes(x = hits_daily_01d, fill = factor(future_return_sign))) + 
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.01) + 
  coord_cartesian(xlim = c(-0.50, 0.50))

#' # 9. Google Trends Smoothed Level
ggplot(train, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_line(aes(y = hits_daily_sma_05d * 47), colour = "red", alpha = 0.8)
ggplot(train %>% filter(date >= "2017-01-01"), aes(x = date )) + 
  geom_line(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_line(aes(y = hits_daily_sma_05d * 47), colour = "red", alpha = 0.6) + 
  geom_point(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_point(aes(y = hits_daily_sma_05d * 47), colour = "red", alpha = 0.6)
ggplot(train %>% filter(date >= "2016-01-01", date <= "2017-01-01"), aes(x = date )) + 
  geom_line(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_line(aes(y = hits_daily_sma_05d * 47), colour = "red", alpha = 0.6) + 
  geom_point(aes(y = close), colour = "blue", alpha = 0.8) + 
  geom_point(aes(y = hits_daily_sma_05d * 47), colour = "red", alpha = 0.6)

#' # 10. Google Trends Drawdown
ggplot(train, aes(x = hits_daily_drawdown, fill = factor(future_return_sign))) + 
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.01)

#' # 11. VIX
ggplot(train, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = vix * 30), colour = "red")
ggplot(train %>% filter(date >= "2017-01-01"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = vix * 150), colour = "red")



