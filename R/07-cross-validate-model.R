#' ---
#' title: "Cross Validate Model"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#' ---

#' # 1. Source
source("./R/05-engineer-features.R")

#' # 2. Set folds
train <- train %>% 
  mutate(fold = ifelse(date >= "2011-09-13" & date <= "2013-12-31", 0, 0), 
         fold = ifelse(date >= "2014-01-01" & date <= "2014-03-31", 1, fold), 
         fold = ifelse(date >= "2014-04-01" & date <= "2014-06-30", 2, fold), 
         fold = ifelse(date >= "2014-07-01" & date <= "2014-09-30", 3, fold), 
         fold = ifelse(date >= "2014-10-01" & date <= "2014-12-31", 4, fold), 
         fold = ifelse(date >= "2015-01-01" & date <= "2015-03-31", 5, fold), 
         fold = ifelse(date >= "2015-04-01" & date <= "2015-06-30", 6, fold), 
         fold = ifelse(date >= "2015-07-01" & date <= "2015-09-30", 7, fold), 
         fold = ifelse(date >= "2015-10-01" & date <= "2015-12-31", 8, fold), 
         fold = ifelse(date >= "2016-01-01" & date <= "2016-03-31", 9, fold), 
         fold = ifelse(date >= "2016-04-01" & date <= "2016-06-30", 10, fold), 
         fold = ifelse(date >= "2016-07-01" & date <= "2016-09-30", 11, fold), 
         fold = ifelse(date >= "2016-10-01" & date <= "2016-12-31", 12, fold), 
         fold = ifelse(date >= "2017-01-01" & date <= "2017-03-31", 13, fold), 
         fold = ifelse(date >= "2017-04-01" & date <= "2017-06-30", 14, fold), 
         fold = ifelse(date >= "2017-07-01" & date <= "2017-09-30", 15, fold), 
         fold = ifelse(date >= "2017-10-01" & date <= "2017-12-31", 16, fold))

#' # 3. Set Features
xgb_features <- train %>% 
  select(matches("close_change_..d$"), matches("close_drawdown"), matches("close_positive_..d$")) %>% 
  colnames()

#' # 4. Correlation
bitcoin_corr <- tibble()
for (i in xgb_features) { 
  df <- tibble(feature = i, 
               correlation = cor(train[[i]], train[["future_return_sign"]], use = "complete.obs"))
  bitcoin_corr <- bind_rows(bitcoin_corr, df)
}
print(bitcoin_corr)
rm(i, df)

#' # 5. Set XGB parameters
xgb_params <- list(booster = "gbtree", 
                   eta = 0.01, 
                   gamma = 0, 
                   max_depth = 1, 
                   min_child_weight = 1, 
                   subsample = 0.3, 
                   colsample_bytree = 0.2, 
                   lambda = 0, 
                   alpha = 0, 
                   objective = "binary:logistic", 
                   eval_metric = "logloss")

#' # 6. Create XGB Data Object
xgb_train <- xgb.DMatrix(data = as.matrix(train %>% filter(fold == 0) %>% .[, xgb_features]), 
                         label = as.matrix(train %>% filter(fold == 0) %>% .[, "future_return_sign"]))

#' # 7. Cross Validate For Parameter Tuning
set.seed(5)
xgb_cv <- xgb.cv(data = xgb_train, 
                 params = xgb_params, 
                 showsd = TRUE, 
                 early_stopping_rounds = 50, 
                 print_every_n = 1, 
                 nfold = 10, 
                 nrounds = 10000)
xgb_nrounds <- xgb_cv[["best_iteration"]]

#' # 8. Cross Validate
xgb_prediction <- tibble()
for (i in 1:15) { 
  print(str_c("Starting cross validation on fold ", i, "."))
  xgb_train <- xgb.DMatrix(data = as.matrix(train %>% filter(fold < i) %>% .[, xgb_features]), 
                           label = as.matrix(train %>% filter(fold < i) %>% .[, "future_return_sign"]))
  xgb_test <- xgb.DMatrix(data = as.matrix(train %>% filter(fold == i) %>% .[, xgb_features]), 
                          label = as.matrix(train %>% filter(fold == i) %>% .[, "future_return_sign"]))
  prediction_list <- list()
  for (j in 1:10) { 
    print(str_c("Training model ", j, "."))
    set.seed(j)
    xgb_model <- xgb.train(data = xgb_train, params = xgb_params, nrounds = xgb_nrounds + 30 * i)
    prediction_list[[j]] <- predict(xgb_model, xgb_test)
  }
  xgb_prediction <- bind_rows(xgb_prediction, tibble(prediction = pmap_dbl(prediction_list, mean)))
}
rm(i, j, prediction_list)

#' # 9. Importance
xgb_importance <- xgb.importance(model = xgb_model, feature_names = xgb_features)

#' # 10. Generate Predictions
signal <- train %>% 
  filter(fold > 0) %>% 
  mutate(future_return_pred = xgb_prediction[["prediction"]], 
         future_return_sign_pred = ifelse(future_return_pred > 0.50, 1, 0), 
         signal = ifelse(lag(future_return_pred, n = 1, default = 1) > 0.50, 1, 0), 
         trading_cost = abs(signal - lag(signal, n = 1, default = 0)) * 0.003, 
         return_buyhold = cumprod(1 + close_change_01d), 
         return_model = cumprod(1 + close_change_01d * signal - trading_cost))



