#' ---
#' title: "Evaluate Model"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: false 
#'     fig_width: 10
#'     fig_height: 5 
#' ---

#' # 1. Source
source("./R/07-cross-validate-model.R")

#' # 2. Visualize Signals
#' Predicted probability ranges from 0.40 to 0.66. 
ggplot(signal, aes(x = future_return_pred)) + 
  geom_histogram(binwidth = 0.001)

#' The following plots show the signal overlayed on top of the bitcoin price series. Dark blue indicates days where
#' the model predicts a high probability of positive price movement and dark green indicates day where the model 
#' predicts a low probability of positive price movement. Purple-ish colors represent intermediate probabilities. 
ggplot(signal, aes(x = date, y = close, colour = future_return_pred)) + 
  scale_colour_gradient(low = "green", high = "blue") +
  geom_line()
ggplot(signal, aes(x = date, y = close, colour = future_return_pred)) + 
  scale_colour_gradient(low = "green", high = "blue") +
  geom_line() + 
  scale_y_log10()

#' The following plots show the signal translated into long and flat positions using a 0.50 probability threshold for 
#' going long. 
ggplot(signal, aes(x = date, y = close, colour = signal)) + 
  scale_colour_gradient(low = "green", high = "blue") +
  geom_line()
ggplot(signal, aes(x = date, y = close, colour = signal)) + 
  scale_colour_gradient(low = "green", high = "blue") +
  geom_line() + 
  scale_y_log10()

#' The following plots show the equity curve of the model compared to a buy-and-hold return. 
ggplot(signal, aes(x = date)) + 
  geom_line(aes(y = return_model), colour = "red") + 
  geom_line(aes(y = return_buyhold), colour = "blue")
ggplot(signal, aes(x = date)) + 
  geom_line(aes(y = return_model), colour = "red") + 
  geom_line(aes(y = return_buyhold), colour = "blue") + 
  scale_y_log10()

#' The following plots show the model return minus the buy-and-hold return. 
ggplot(signal, aes(x = date)) + 
  geom_line(aes(y = return_model - return_buyhold), colour = "red")
ggplot(signal, aes(x = date)) + 
  geom_line(aes(y = return_model - return_buyhold + 1), colour = "red") + 
  scale_y_log10()

#' # 3. Descriptive Statistics
#' ## 3.1 Confusion matrix
as_tibble(ConfusionMatrix(signal[["future_return_sign_pred"]], signal[["future_return_sign"]]))
#' ## 3.2 Accuracy
Accuracy(signal[["future_return_sign_pred"]], signal[["future_return_sign"]])
#' ## 3.3 Log loss
LogLoss(signal[["future_return_pred"]], signal[["future_return_sign"]])
#' ## 3.4 Overall return of model versus buy-and-hold from 2014 to present
signal %>% select(return_model) %>% filter(row_number() == n())
signal %>% select(return_buyhold) %>% filter(row_number() == n())
#' ## 3.5 Mean daily return and daily standard deviation 
mean(signal[["close_change_01d"]] * signal[["signal"]])
sd(signal[["close_change_01d"]] * signal[["signal"]])
#' ## 3.6 Annualized return and annualized standard deviation
(1 + mean(signal[["close_change_01d"]] * signal[["signal"]]))^365
sd(signal[["close_change_01d"]] * signal[["signal"]]) * 365^0.5
#' ## 3.7 Annualized sharpe ratio
mean(signal[["close_change_01d"]] * signal[["signal"]]) / sd(signal[["close_change_01d"]] * signal[["signal"]]) * 365^0.5
#' ## 3.8 Maximum Drawdown of model versus buy-and-hold return from 2014 to present
signal %>% 
  mutate(return_model_drawdown = -1 * (1 - return_model / cummax(return_model))) %>% 
  select(return_model_drawdown) %>% 
  min()
signal %>% 
  mutate(return_model_drawdown = -1 * (1 - return_buyhold / cummax(return_buyhold))) %>% 
  select(return_model_drawdown) %>% 
  min()
#' ## 3.9 Daily and annualized semideviation
down <- signal %>% 
  filter(close_change_01d * signal < 0)
sd(down[["close_change_01d"]] * down[["signal"]])
sd(down[["close_change_01d"]] * down[["signal"]]) * 365^0.5
#' ## 3.10 Annualized sortino ratio
mean(signal[["close_change_01d"]] * signal[["signal"]]) / sd(down[["close_change_01d"]] * down[["signal"]]) * 365^0.5

#' # 4. Most Recent Predictions
signal %>% 
  select(date, close, future_return_pred) %>% 
  tail(n = 30)

temp <- signal %>% select(date, close, future_return_pred, future_return_sign_pred) %>% filter(date >= "2017-08-01")
knitr::kable(temp)
ggplot(temp, aes(x = date)) + 
  geom_line(aes(y = close, colour = future_return_sign_pred)) + 
  geom_point(aes(y = close, colour = future_return_sign_pred)) + 
  scale_color_gradient(low = "red", high = "blue")
ggplot(temp, aes(x = date)) + 
  geom_line(aes(y = close, colour = future_return_pred)) + 
  geom_point(aes(y = close, colour = future_return_pred)) + 
  scale_color_gradient2(low = "red", mid = "grey80", high = "blue", midpoint = 0.50)
