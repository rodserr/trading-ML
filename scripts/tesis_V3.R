# Libraries----
library(tidyverse)
library(lubridate)
library(magrittr)
library(TTR)
library(caret)
library(quantmod)
library(gridExtra) # Plot's Grid
library(pls) # PCR
library(leaps) # Best subset selection
library(glmnet) # Ridge & Lasso regression
library(ROCR) # ROC curve
library(zoo)
library(purrr)
library(QuantTools)
library(plotly)
library(readr)
library(PerformanceAnalytics)
library(factoextra)

# Functions----

# Get dependent variable 
predict_tp <- function(data = data, tp = 0.015, sl = 0.02, h = 10){
  
  class <- c()
  for (d in 1:(nrow(data)-h)){
    
    buy <- data$close[d]
    
    # Goals
    goal_limit <- buy*( 1 + tp )
    goal_stop <- buy*( 1 - sl )
    
    # Higs and lows within the horizon
    highs <- data$high[(d+1):(d+h)]
    lows <- data$low[(d+1):(d+h)]
    
    # Check for limits triggered
    tests_high <- highs >= goal_limit
    tests_low <- lows <= goal_stop
    
    # Check the first trigger
    first_limit <- ifelse(any(tests_high == TRUE), min(which(tests_high == TRUE)), NA)
    first_stop <- ifelse(any(tests_low == TRUE), min(which(tests_low == TRUE)), NA)
    
    # Make Classification
    if(is.na(first_limit)) {
      react <- 'stay'
    } else {
      if (is.na(first_stop)){
        react <- 'buy'
      } else {
        react <- ifelse(first_limit < first_stop, 'buy', 'stay')
      }
    }
    
    class %<>% append(react)
  }
  
  # Attach class to data
  data %<>% cbind(class = c(class, rep(NA, h)))
  
  return(data)
}

# Get best CutOff according w/ ppv
getROC <- function(predict, testsample){
  
  result <- list()
  for(cutoff in seq(0, 1, 0.05)){
    
    .cm <- factor(ifelse(predict[, 'buy'] >= cutoff, 'buy', 'stay')) %>% 
      confusionMatrix(reference = testsample$class_2)
    
    .overall <- .cm$overall %>% data.frame() %>% t()
    .byClass <- .cm$byClass %>% data.frame() %>% t()
    .cases <- .cm$table['buy','buy']
    
    .aux <- cbind(CutOff = cutoff, .overall, .byClass, cases = .cases) 
    
    result %<>% rlist::list.append(.aux)
    
  }
  result_1 <- map_dfr(result, as.data.frame)
  
  cases <- result_1 %>% 
    filter(cases > 10) %>% 
    select(c('CutOff', 'Pos Pred Value', 'cases')) %>% 
    setNames(c('CutOff', 'ppv', 'cases'))
  
  maxcoff <- which.max(cases$ppv)
  
  coff <- slice(cases, maxcoff)
  
  # plotcoff <- plot(x = cases$CutOff, y = cases$'Pos Pred Value', type = 'l')
  
  return(coff)
}

# Get Value of ppv and CutOff
ppvValue <- function(predict, testsample){
  result <- list()
  for(cutoff in seq(0, 1, 0.05)){
    
    .cm <- factor(ifelse(predict[, 'buy'] >= cutoff, 'buy', 'stay')) %>% 
      confusionMatrix(reference = testsample$class_2)
    
    .byClass <- .cm$byClass %>% data.frame() %>% t()
    .cases <- .cm$table['buy','buy']
    
    .aux <- cbind(CutOff = cutoff, .byClass, cases = .cases) 
    
    result %<>% rlist::list.append(.aux)
    
  }
  result_1 <- map_dfr(result, as.data.frame)
  
  df <- result_1 %>% 
    select(c('CutOff', 'Pos Pred Value', 'cases', 'Sensitivity', 'Specificity', 'Precision', 'Recall')) %>% 
    setNames(c('cutoff', 'ppv',  'cases', 'sensitivity', 'specificity', 'precision', 'recall'))
  
  return(df)
}

# Run all process
PCRegression <- function(serie, tp, sl, h, uniqueBUYs = TRUE, model = FALSE){
  
  # Create class
  if(uniqueBUYs){

    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(aux = ifelse(class == lag(class), 1, 0),
             class_2 = factor(ifelse(aux == 0 & class == 'buy', 'buy', 'stay'))) %>% 
      select(-one_of('aux', 'class'))
  } else {
    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(class_2 = factor(class)) %>% 
      select(-one_of('class'))
  }
  
  # Create Indicators
  .adx <- ADX(data[,c('high', 'low', 'close')], n = 14, maType = 'EMA') %>%
    as.data.frame() %>% setNames(c('dip', 'din', 'dx', 'adx'))
  
  .macd = MACD(data$close, maType = 'EMA') %>% as.data.frame() %>% 
    setNames(c('macd', 'macd_signal'))
  
  data %<>% cbind(.adx[,-3], .macd) %>% 
    mutate(lag_1 = close / lag(close) - 1,
           # lag_2 = close / lag(close, 2) - 1,
           lag_3 = close / lag(close, 3) - 1,
           # lag_4 = close / lag(close, 4) - 1,
           lag_5 = close / lag(close, 5) - 1,
           rsi = RSI(close, n = 14),
           ema_50 = SMA(close, n = 50),
           ema_13 = EMA(close, n = 13),
           # macd_hist = macd - macd_signal,
           bb_down = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,1],
           bb_up = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,3],
           atr = ATR(data[,c('high', 'low', 'close')], n = 14)[,2],
           sar = SAR(data[,c('high', 'low')])[,1]
    ) %>% 
    na.omit()
  
  # Split Data
  train <- data %>% filter(year(timestamp) %in% seq(2009, 2014, 1))
  
  validation <- data %>% filter(year(timestamp) %in% c(2015, 2016))
  
  test <- data %>% filter(year(timestamp) %in% c(2017, 2018))
  
  # Create Folds
  
  # Sample's
  .fold_S1 <- which(year(train$timestamp) %in% c(2009, 2010))
  .fold_S2 <- which(year(train$timestamp) %in% c(2010, 2011))
  .fold_S3 <- which(year(train$timestamp) %in% c(2011, 2012))
  .fold_S4 <- which(year(train$timestamp) %in% c(2012, 2013))
  
  # Held-Out's
  .fold_OS1 <- which(year(train$timestamp) == 2011)
  .fold_OS2 <- which(year(train$timestamp) == 2012)
  .fold_OS3 <- which(year(train$timestamp) == 2013)
  .fold_OS4 <- which(year(train$timestamp) == 2014)
  
  # Create list
  sampleFolds <- list(.fold_S1, .fold_S2, .fold_S3, .fold_S4)
  
  OsampleFolds <- list(.fold_OS1, .fold_OS2, .fold_OS3, .fold_OS4)
  
  # PCA Model
  
  .PCA_cntrl <- trainControl(index = sampleFolds,
                             indexOut = OsampleFolds,
                             classProbs = TRUE,
                             savePredictions = TRUE,
                             summaryFunction = twoClassSummary,
                             preProcOptions = list(thresh = 0.85) #thresh = 0.85,pcaComp = 3)
  )
  
  PCA_model <- train(class_2 ~ (.)^2 - close - high - low - open - macd - bb_down - bb_up - dip - din
                     - adx - macd_signal - lag_1 - lag_3 - lag_5 - rsi - ema_50
                     - atr - sar - ema_13,
                     data = train[,-1],
                     method = "glm",
                     family = 'binomial',
                     metric = 'ROC',
                     preProcess = c('pca'),
                     trControl = .PCA_cntrl
  )
  
  
  # Prediction
  PCA_pred <- predict(PCA_model, newdata = validation[,-1], type = 'prob')
  
  cutoff <- getROC(PCA_pred, validation)
  
  if(model){
    
    return(PCA_model) 
    
  } else{
    
    return(cutoff) 
    
  }
  
}

createIndicators <- function(data){
  
  # Create Indicators
  .adx <- ADX(data[,c('high', 'low', 'close')], n = 14, maType = 'EMA') %>%
    as.data.frame() %>% setNames(c('dip', 'din', 'dx', 'adx'))
  
  .macd = MACD(data$close, maType = 'EMA') %>% as.data.frame() %>% 
    setNames(c('macd', 'macd_signal'))
  
  data %<>% cbind(adx = .adx[,-c(1,2,3)], .macd) %>% 
    mutate(lag_1 = close / lag(close) - 1,
           # lag_2 = close / lag(close, 2) - 1,
           lag_3 = close / lag(close, 3) - 1,
           # lag_4 = close / lag(close, 4) - 1,
           lag_5 = close / lag(close, 5) - 1,
           rsi = RSI(close, n = 14),
           # ema_50 = SMA(close, n = 50),
           # ema_13 = EMA(close, n = 13),
           # macd_hist = macd - macd_signal,
           # bb_down = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,1],
           bb_up = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,3],
           atr = ATR(data[,c('high', 'low', 'close')], n = 14)[,2]
           # sar = SAR(data[,c('high', 'low')])[,1]
    ) %>% 
    na.omit()
  
  return(data)
}

WFRegression <- function(serie, tp, sl, h, cut = .5, uniqueBUYs = TRUE){
  
  # Create class
  if(uniqueBUYs){
    
    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(aux = ifelse(class == lag(class), 1, 0),
             class_2 = factor(ifelse(aux == 0 & class == 'buy', 'buy', 'stay'))) %>% 
      select(-one_of('aux', 'class'))
  } else {
    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(class_2 = factor(class)) %>% 
      select(-one_of('class'))
  }
  
  # Create Indicators
 data %<>% createIndicators()
  
  list_cm <- list()
  list_pred <- list()
  list_model <- list()
  for(y in c(2013, 2014, 2015, 2016, 2017, 2018)){
    
  # Split Data
  train <- data %>% filter(year(timestamp) %in% seq(2009, y-1, 1))
  
  validation <- data %>% filter(year(timestamp) == y )
  
  # Create Folds
  
  # Sample's
  n <- floor(nrow(train)*0.25) %>% as.integer()
  
  .fold_S1 <- seq(1, n, 1) %>% as.integer()
  .fold_S2 <- seq(1, 2*n, 1) %>% as.integer()
  .fold_S3 <- seq(1, 3*n, 1) %>% as.integer()
  
  # Held-Out's
  .fold_OS1 <- seq(n+1, 2*n, 1) %>% as.integer()
  .fold_OS2 <- seq((2*n)+1, n*3, 1) %>% as.integer()
  .fold_OS3 <- seq((3*n)+1, n*4, 1) %>% as.integer()
  
  # Create list
  sampleFolds <- list(.fold_S1, .fold_S2, .fold_S3)
  
  OsampleFolds <- list(.fold_OS1, .fold_OS2, .fold_OS3)
  
  # PCA Model
  
  .PCA_cntrl <- trainControl(index = sampleFolds,
                             indexOut = OsampleFolds,
                             classProbs = TRUE,
                             savePredictions = TRUE,
                             summaryFunction = twoClassSummary,
                             preProcOptions = list(thresh = 0.85) #thresh = 0.85,pcaComp = 3)
  )
  
  PCA_model <- train(class_2 ~ (.)^2 - close #- high - low - open 
                     - adx - macd - macd_signal - rsi - lag_1 - lag_5 - lag_3 
                     - atr - bb_up, # - dip - din - sar - ema_50  - ema_13 - bb_down,
                     data = train[,-c(1,3,4,5)],
                     method = "glm",
                     family = 'binomial',
                     metric = 'ROC',
                     preProcess = c('pca'),
                     trControl = .PCA_cntrl
  )
  
  # Prediction
  PCA_pred <- predict(PCA_model, newdata = validation[,-c(1,3,4,5)], type = 'prob')
  
  prediction <- ifelse(predict(PCA_model, newdata = validation[,-c(1,3,4,5)], type = 'prob')[, 'buy'] >= cut, 
                'buy', 'stay') %>% factor()
  
  aux_cm <- confusionMatrix(prediction, reference = validation$class_2)
  
  list_cm %<>% rlist::list.append(aux_cm)
  
  list_pred %<>% rlist::list.append(prediction) 
  
  list_model %<>% rlist::list.append(PCA_model) 
  }
  
  list_final <- list(list_cm, list_pred, list_model) 
  
  return(list_final)
}

getPredictors <- function(data){
  
  df <- glm(class_2 ~ (.)^2 - close - macd - bb_up # - dip - din 
                     - adx - macd_signal - rsi # - ema_50 - bb_down - sar - ema_13
                     - atr - lag_1 - lag_3 - lag_5,  
                     data = data[,-c(1,3,4,5)],
                     family = 'binomial',
                     x = TRUE)
  
  var_matriz <- df$x %>% data.frame() %>% 
    select(-one_of('X.Intercept.')) 
  
  var_center_scale <- var_matriz %>% preProcess(method = c("center", "scale"))
  
  var_pred <- predict(var_center_scale, var_matriz)
  
  return(var_pred)
}

validationSet <- function(serie, tp, sl, h, uniqueBUYs = TRUE){
  # Create class
  if(uniqueBUYs){
    
    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(aux = ifelse(class == lag(class), 1, 0),
             class_2 = factor(ifelse(aux == 0 & class == 'buy', 'buy', 'stay'))) %>% 
      select(-one_of('aux', 'class'))
    
  } else {
    
    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(class_2 = factor(class)) %>% 
      select(-one_of('class'))
  }
  
  # Create Indicators
  .adx <- ADX(data[,c('high', 'low', 'close')], n = 14, maType = 'EMA') %>%
    as.data.frame() %>% setNames(c('dip', 'din', 'dx', 'adx'))
  
  .macd = MACD(data$close, maType = 'EMA') %>% as.data.frame() %>% 
    setNames(c('macd', 'macd_signal'))
  
  data %<>% cbind(.adx[,-3], .macd) %>% 
    mutate(lag_1 = close / lag(close) - 1,
           # lag_2 = close / lag(close, 2) - 1,
           lag_3 = close / lag(close, 3) - 1,
           # lag_4 = close / lag(close, 4) - 1,
           lag_5 = close / lag(close, 5) - 1,
           rsi = RSI(close, n = 14),
           ema_50 = SMA(close, n = 50),
           ema_13 = EMA(close, n = 13),
           # macd_hist = macd - macd_signal,
           bb_down = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,1],
           bb_up = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,3],
           atr = ATR(data[,c('high', 'low', 'close')], n = 14)[,2],
           sar = SAR(data[,c('high', 'low')])[,1]
    ) %>% 
    na.omit()
  
  # Split Data
  train <- data %>% filter(year(timestamp) %in% seq(2009, 2014, 1))
  
  validation <- data %>% filter(year(timestamp) %in% c(2015, 2016))
  
  test <- data %>% filter(year(timestamp) %in% c(2017, 2018))
  
  return(validation)
}

testSet <- function(serie, tp, sl, h, uniqueBUYs = TRUE){
  # Create class
  if(uniqueBUYs){
    
    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(aux = ifelse(class == lag(class), 1, 0),
             class_2 = factor(ifelse(aux == 0 & class == 'buy', 'buy', 'stay'))) %>% 
      select(-one_of('aux', 'class'))
    
  } else {
    
    data <- predict_tp(serie, tp, sl, h) %>% 
      mutate(class_2 = factor(class)) %>% 
      select(-one_of('class'))
  }
  
  # Create Indicators
  .adx <- ADX(data[,c('high', 'low', 'close')], n = 14, maType = 'EMA') %>%
    as.data.frame() %>% setNames(c('dip', 'din', 'dx', 'adx'))
  
  .macd = MACD(data$close, maType = 'EMA') %>% as.data.frame() %>% 
    setNames(c('macd', 'macd_signal'))
  
  data %<>% cbind(.adx[,-3], .macd) %>% 
    mutate(lag_1 = close / lag(close) - 1,
           # lag_2 = close / lag(close, 2) - 1,
           lag_3 = close / lag(close, 3) - 1,
           # lag_4 = close / lag(close, 4) - 1,
           lag_5 = close / lag(close, 5) - 1,
           rsi = RSI(close, n = 14),
           ema_50 = SMA(close, n = 50),
           ema_13 = EMA(close, n = 13),
           # macd_hist = macd - macd_signal,
           bb_down = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,1],
           bb_up = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,3],
           atr = ATR(data[,c('high', 'low', 'close')], n = 14)[,2],
           sar = SAR(data[,c('high', 'low')])[,1]
    ) %>% 
    na.omit()
  
  # Split Data
  train <- data %>% filter(year(timestamp) %in% seq(2009, 2014, 1))
  
  validation <- data %>% filter(year(timestamp) %in% c(2015, 2016))
  
  test <- data %>% filter(year(timestamp) %in% c(2017, 2018))
  
  return(test)
}

createGrid <- function(stock, validation = TRUE){
  result_aux <- list()
  for(tp in c(0.02, 0.04, 0.06)) 
    for(sl in c(0.02, 0.04, 0.06))
      for(h in c(20)){
        
        model <- PCRegression(stock, tp, sl, h, uniqueBUYs = FALSE, model = TRUE)
        if(validation){
          outsample <- validationSet(stock, tp, sl, h, uniqueBUYs = FALSE)
        }else{
          outsample <- testSet(stock, tp, sl, h, uniqueBUYs = FALSE)
        }
       
        val_pred <- predict(model, newdata = outsample[,-1], type = 'prob')
        
        .df <- ppvValue(val_pred, outsample)
        
        par <- paste(tp, sl, h, sep = '-')
        
        .df %<>% cbind(parameters = par)
        
        result_aux %<>% rlist::list.append(.df)
         
      }
  result <- map_dfr(result_aux, as.data.frame)
  
  return(result)
}

longStrat <- function(back_data, cap_inic = 10000, comission = 0, sl = 0.02,
                      tp = 0.02, sli_pag = 0, horizon = 20){
  
  last_balance <- 0
  balance <- rep(0, nrow(back_data))
  last_cap <- cap_inic
  cap <- rep(last_cap, nrow(back_data))
  buy <- rep(0, nrow(back_data))
  sell <- rep(0, nrow(back_data))
  buy_date <- rep(NA, nrow(back_data))
  buy_price <- rep(0, nrow(back_data))
  sell_date <- rep(NA, nrow(back_data))
  sell_price <- rep(0, nrow(back_data))
  last_quantity <- 0
  last_buy_price <- 0
  last_sell_price <- 0
  last_buy_date <- 0
  quantity <-rep(0, nrow(back_data)) 
  last_comision <- 0
  comision <- rep(0, nrow(back_data)) 
  salida <- rep(0, nrow(back_data))
  stop_loss <- 0
  take_profit <- 0
  buy_class <- rep('', nrow(back_data))
  aux_class <- ''

  for(i in 1:nrow(back_data)) {

          # Buy
      if(back_data$predict[i] == 'buy' &
         last_balance == 0 & last_cap >= last_quantity)
      {
        last_balance <- 1
        last_buy_price <- back_data$close[i] * (1+sli_pag)
        last_quantity <- 1000
        
        stop_loss <- last_buy_price*(1 - sl)
        take_profit <- last_buy_price*(1 + tp)
        
        last_buy_date <- back_data$timestamp[i]
        last_comision <- last_quantity * comission
        last_cap <- last_cap - last_quantity
        buy[i] <- 1
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
        aux_class <- back_data$class[i] %>% as.character()
        
      }
      
      #Sell by StopLoss
      if(last_balance == 1 &
         back_data$low[i] <= stop_loss &
         back_data$timestamp[i] != last_buy_date) 
      { 
        last_balance <- 0
        buy_date[i] <- last_buy_date
        buy_price[i] <- last_buy_price
        sell_date[i] <- back_data$timestamp[i]
        sell_price[i] <- stop_loss
        last_quantity <- last_quantity/last_buy_price*stop_loss
        last_comision <- last_comision + (last_quantity * comission)
        last_cap <- last_cap + last_quantity - last_comision
        sell[i] <- 1
        salida[i] <- "Stop Loss"
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
        buy_class[i] <- aux_class
      }
      
      # Sell by takeProfit
      if(last_balance == 1 &&
         (back_data$high[i] >= take_profit &
          back_data$timestamp[i] != last_buy_date))
      { 
        last_balance <- 0
        buy_date[i] <- last_buy_date
        buy_price[i] <- last_buy_price
        sell_date[i] <- back_data$timestamp[i]
        sell_price[i] <-  take_profit * (1 - sli_pag)
        last_quantity <- last_quantity/last_buy_price*take_profit
        last_comision <- last_comision + (last_quantity * comission)
        last_cap <- last_cap + last_quantity - last_comision
        sell[i] <- 1
        salida[i] <- "Strategy"
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
        buy_class[i] <- aux_class
      }
      
      # Sell by horizon
      if(last_balance == 1 &&
         (as.numeric(i - which(back_data$timestamp == last_buy_date)) > horizon &
          back_data$timestamp[i] != last_buy_date))
      { 
        last_balance <- 0
        buy_date[i] <- last_buy_date
        buy_price[i] <- last_buy_price
        sell_date[i] <- back_data$timestamp[i]
        sell_price[i] <- back_data$close[i]* (1 - sli_pag)
        last_quantity <- last_quantity/last_buy_price*back_data$close[i]
        last_comision <- last_comision + (last_quantity * comission)
        last_cap <- last_cap + last_quantity - last_comision
        sell[i] <- 1
        salida[i] <- "Limit Horizon"
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
        buy_class[i] <- aux_class
      }
      
      balance[i] <- last_balance
      cap[i] <- last_cap
      
    
  }
  
  back_data$balance <- balance
  back_data$quantity <- quantity
  back_data$comision <- comision
  back_data$cap <- cap
  back_data$buy_date <- as_date(buy_date)
  back_data$sell_date <- as_date(sell_date)
  back_data$buy_price <- buy_price
  back_data$sell_price <- sell_price
  back_data$buy <- buy
  back_data$sell <- sell
  back_data$salida <- salida
  back_data$buy_class <- buy_class
  
  return(back_data)
}

source('scripts/summStrat.R')
# Inspect data----
# Read
serie <- c('S&P_500', 'NASDAQ', 'NIKKEI_225', 'FTSE_100', 'BOVESPA')
list_serie <- list()
for(s in serie){
  .aux_serie <- paste('data/', s, '.csv', sep = '') %>%
    read_csv(locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
    select(-one_of(c('Vol.', '% var.'))) %>% 
    mutate(Fecha = dmy(Fecha)) %>% 
    setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
    arrange(timestamp)
  
  list_serie %<>% rlist::list.append(.aux_serie) 
}

# Search NA's
# which(is.na(FTSE))

# Plot
# xts(raw_data$close, raw_data$timestamp) %>% plot()

# Create dependent variable----

# data <- WTI %>% predict_tp(tp = 0.02, sl = 0.06, h = 20) %>%
#   mutate(aux = ifelse(class == lag(class), 1, 0),
#          class_2 = factor(ifelse(aux == 0 & class == 'buy', 'buy', 'stay'))) %>% #levels = c('stay', 'buy')))
#   select(-one_of('aux', 'class'))

.tp = 0.02
.sl = 0.025
.h = 20

data <- list_serie[[1]] %>% predict_tp(tp = .tp, sl = .sl, h = .h) %>%
  mutate(class_2 = factor(class)) %>% #levels = c('stay', 'buy')))
  select(-one_of('class'))

# which(data$class_2 == 'buy') %>% length()

# data %>% filter(year(timestamp) %in% seq(2009, 2010, 1)) %>% 
#   mutate(buy_close = if_else(class_2 == 'buy', close, NA_real_)) %>%
#   ggplot(aes(x = timestamp, y = close)) +
#   geom_line(colour = 'grey') +
#   geom_point(aes(y = buy_close), colour = 'darkgreen', size = 0.8) +
#   labs(x = NULL)

# Create factors----

data %<>% createIndicators()

# Description Analysis----

data <- mod$x %>% data.frame() %>% select(-one_of('X.Intercept.')) %>% 
  cbind(class_2 = data_2$class_2)

# Correlation plot
.plot_corr <- data_2 %>%
  select(-one_of('timestamp', 'class_2', 'open', 'high', 'low')) %>%  
  # ,'close', 'high', 'low', 'open', 'ema_13', 'lag_2', 'lag_4', 'macd')) %>%
  na.omit() %>%
  cor() %>%
  corrplot::corrplot(method = 'number', type = 'lower', order = 'hclust', title = '1')

# Box-plot's
.plot_OHLC <- data %>% 
  select(c('class_2', 'close', 'open', 'high', 'low')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class_2)) +
  geom_boxplot()

.plot_lags <- data %>% 
  select(c('class_2', 'lag_1', 'lag_3', 'lag_5')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class_2)) +
  geom_boxplot()

.plot_adx_rsi <- data %>% 
  select(c('class_2', 'adx', 'rsi')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class_2)) +
  geom_boxplot()

.plot_macd <- data %>% 
  select(c('class_2', 'macd', 'macd_signal')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class_2)) +
  geom_boxplot()

.plot_atr <- data %>% 
  select(c('class_2', 'atr')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class_2)) +
  geom_boxplot()

.plot_boxplot <- list(.plot_OHLC, .plot_lags, .plot_adx_rsi, .plot_macd, .plot_atr)

# Histogram
.ind <- names(data)[-46]#[-c(1, 6)]
.plot_hist <- list()
for(i in .ind){
  .histo_plot <- data %>% 
    select(i) %>% 
    reshape2::melt() %>% 
    ggplot(aes(x = value)) +
    geom_histogram(color="darkblue", fill="lightblue") +
    labs(x = i)
  
  .plot_hist %<>% rlist::list.append(.histo_plot)
}

# Density
.plot_dens <- list()
for(i in .ind){
  .dens_plot <- data %>% 
    select(c('class_2', i)) %>% 
    reshape2::melt() %>%
    ggplot(aes(x = value, color = class_2)) +
    geom_density() +
    labs(x = i)
  
  .plot_dens %<>% rlist::list.append(.dens_plot)
}

# list of all plots
descr_plots <- list(.plot_corr, .plot_boxplot, .plot_hist, .plot_dens)


# Grid Plots

# Correlation
descr_plots[[1]] %>% 
  corrplot::corrplot(method = 'number', type = 'lower', order = 'hclust')

# Box-Plot
do.call('grid.arrange', descr_plots[[2]])

# Histograms
do.call('grid.arrange', descr_plots[[3]])

# Density
do.call('grid.arrange', descr_plots[[4]])

# Interaction Analysis----





# Split data----
train <- data %>% filter(year(timestamp) %in% seq(2009, 2012, 1))

# validation <- data %>% filter(year(timestamp) %in% c(2013, 2018))

test <- data %>% filter(year(timestamp) %in% c(2013, 2013))

# Create Folds---- 

# Sample's
# .fold_S1 <- which(year(train$timestamp) %in% c(2009, 2010))
# .fold_S2 <- which(year(train$timestamp) %in% c(2010, 2011))
# .fold_S3 <- which(year(train$timestamp) %in% c(2011, 2012))
# .fold_S4 <- which(year(train$timestamp) %in% c(2012, 2013))

# Held-Out's
# .fold_OS1 <- which(year(train$timestamp) == 2011)
# .fold_OS2 <- which(year(train$timestamp) == 2012)
# .fold_OS3 <- which(year(train$timestamp) == 2013)
# .fold_OS4 <- which(year(train$timestamp) == 2014)

n <- floor(nrow(train)*0.25) %>% as.integer()

.fold_S1 <- seq(1, n, 1) %>% as.integer()
.fold_S2 <- seq(1, 2*n, 1) %>% as.integer()
.fold_S3 <- seq(1, 3*n, 1) %>% as.integer()

# Held-Out's
.fold_OS1 <- seq(n+1, 2*n, 1) %>% as.integer()
.fold_OS2 <- seq((2*n)+1, n*3, 1) %>% as.integer()
.fold_OS3 <- seq((3*n)+1, n*4, 1) %>% as.integer()

# Create list
sampleFolds <- list(.fold_S1, .fold_S2, .fold_S3)

OsampleFolds <- list(.fold_OS1, .fold_OS2, .fold_OS3)

# PCR: ALL interactions----

.PCA_cntrl <- trainControl(index = sampleFolds,
                          indexOut = OsampleFolds,
                          classProbs = TRUE,
                          savePredictions = TRUE,
                          summaryFunction = twoClassSummary, #twoClassSummary,
                          preProcOptions = list(thresh = 0.85) #thresh = 0.85,pcaComp = 3)
)
# - high - low - open
PCA_model <- train(class_2 ~ (.)^2 - close - macd - bb_up # - bb_down - dip - din  high - low - open 
                   - adx - macd_signal - lag_1 - lag_3 - lag_5 - rsi # - ema_50 - sar - ema_13
                   - atr,  
                   data = train[,-c(1,3,4,5)],
                   method = "glm",
                   family = 'binomial',
                   metric = 'ROC', # 'ROC',
                   preProcess = c('pca'), # c('center', 'scale'), 
                   trControl = .PCA_cntrl
)

# Summary c(1,3,4,5)
PCA_model
summary(PCA_model)
PCA_model$preProcess$rotation
PCA_model$resample

PCA_model$preProcess$std

# Prediction
PCA_pred <- predict(PCA_model, newdata = test[,-c(1,3,4,5)], type = 'prob')

factor(ifelse(PCA_pred[, 'buy'] >= .5, 'buy', 'stay')) %>% 
  confusionMatrix(reference = test$class_2)


# PCA Analysis----

prueba <- WFRegression(list_serie[[1]], .tp, .sl, .h, cut = .cut, uniqueBUYs = FALSE)

prueba[[3]][[1]] %>% summary()

data <- list_serie[[1]] %>% predict_tp(tp = .tp, sl = .sl, h = .h) %>%
  mutate(class_2 = factor(class)) %>% #levels = c('stay', 'buy')))
  select(-one_of('class')) %>% 
  createIndicators() %>% 
  filter(year(timestamp) %in% seq(2009, 2012, 1))

predictors <- data %>% getPredictors()

pca <- prcomp(predictors, scale = FALSE)

pca_var <- get_pca_var(pca)
corrplot::corrplot(pca_var$cos2[,1:7], is.corr=FALSE)

get_eigenvalue(pca)

fviz_eig(pca, addlabels = TRUE, choice = 'variance', main = '')
fviz_eig(pca, addlabels = TRUE, choice = 'eigenvalue')

fviz_cos2(pca, choice = "var", axes = 1:2, top = 20)

fviz_pca_var(pca, 
             col.var = "contrib",
             select.var = list(contrib = 20),
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             # alpha.var = "contrib",
             repel = TRUE
)

fviz_pca_ind(pca,
             geom = 'point',
             col.ind = 'contrib', #data$class_2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             # repel = TRUE # Avoid text overlapping (slow if many points)
             )

# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 30)
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 30)

fviz_contrib(pca, choice = "var", axes = 1:2, top = 30)

# Grid parameters: best ppv----

result_aux <- list()
for(sl in c(0.02, 0.03, 0.04, 0.05, 0.06)) 
  for(tp in c(0.02, 0.03, 0.04, 0.05, 0.06))
    for(h in c(10, 20)){
      
      parameters <- c(tp, sl, h) %>% 
        matrix() %>%
        t() %>%
        as.data.frame() %>% 
        setNames(c('tp', 'sl', 'h'))
      
      ppv <- PCRegression(BOVESPA, tp, sl, h, uniqueBUYs = FALSE)
      if(sum(ppv$CutOff) == 0){next}
      ppv %<>% cbind(parameters)
      result_aux %<>% rlist::list.append(ppv)
      
      
      
    }

result_BOVESPA <- map_dfr(result_aux, as.data.frame)

result_BOVESPA %>% filter(ppv > 0.75) %>% apply(2, mean)

final_model <- PCRegression(BOVESPA, 0.02, 0.06, 10, uniqueBUYs = FALSE, model = TRUE)

# Result in Validation

validation <- validationSet(BOVESPA, 0.02, 0.06, 10, uniqueBUYs = FALSE)
factor(ifelse(predict(final_model, newdata = validation[,-1], type = 'prob')[, 'buy'] >= .80, 
              'buy', 'stay')) %>% 
  confusionMatrix(reference = validation$class_2)

# Result in Test
test <- testSet(BOVESPA, 0.02, 0.06, 10, uniqueBUYs = FALSE)

test_pred <- predict(final_model, newdata = test[,-1], type = 'prob')

factor(ifelse(test_pred[, 'buy'] >= .60, 'buy', 'stay')) %>% 
  confusionMatrix(reference = test$class_2)


prueba <- ppvValue(test_pred, test)

# Grid parameters: best ppv----

stock <- list_serie[[4]]

result <- createGrid(stock, validation = TRUE)

result_T <- createGrid(stock, validation = FALSE)

# Plot
.grid_plot <- list(result %>% ggplot(aes(x = cutoff, y = ppv, color = parameters)) +
                    geom_line(),
                  
                  result %>% ggplot(aes(x = 1-specificity, y = sensitivity, color = parameters)) +
                      geom_line(),
                  
                  result %>% ggplot(aes(x = cutoff, y = cases, color = parameters)) +
                    geom_line(),

                  result %>% ggplot(aes(x = recall, y = precision, color = parameters)) +
                    geom_line()
                  )

.grid_plot_T <- list(result_T %>% ggplot(aes(x = cutoff, y = ppv, color = parameters)) +
                     geom_line(),
                   
                     result_T %>% ggplot(aes(x = 1-specificity, y = sensitivity, color = parameters)) +
                     geom_line(),
                   
                     result_T %>% ggplot(aes(x = cutoff, y = cases, color = parameters)) +
                     geom_line(),
                   
                     result_T %>% ggplot(aes(x = recall, y = precision, color = parameters)) +
                     geom_line()
)

do.call('grid.arrange', .grid_plot)

do.call('grid.arrange', .grid_plot_T)

result_val <- result

# result %>% filter(ppv > 0.75) %>% apply(2, mean)

.tp <- 0.02
.sl <- 0.02
.h <- 20
.cut <- 0.5
stock <- list_serie[[5]]

final_model <- PCRegression(stock, .tp, .sl, .h, uniqueBUYs = FALSE, model = TRUE)

# Result in Validation

validation <- validationSet(stock, .tp, .sl, .h, uniqueBUYs = FALSE)
factor(ifelse(predict(final_model, newdata = validation[,-1], type = 'prob')[, 'buy'] >= .cut, 
              'buy', 'stay')) %>% 
  confusionMatrix(reference = validation$class_2)


# Result in Test
test <- testSet(stock, .tp, .sl, .h, uniqueBUYs = FALSE)

test_pred <- predict(final_model, newdata = test[,-1], type = 'prob')

factor(ifelse(test_pred[, 'buy'] >= .cut, 'buy', 'stay')) %>% 
  confusionMatrix(reference = test$class_2)





# WalkForward----

.tp <- 0.02
.sl <- 0.025
.h <- 20
.cut <- 0.5

list_cm <- list()
list_fr <- list()
list_m <- list()
for(i in 1:length(serie)){
  
  stock <- list_serie[[i]]
  
  # cm <- WFRegression(stock, .tp, .sl, .h, cut = .cut, uniqueBUYs = FALSE)
  cm <- WFRegression(stock, .tp, .sl, .h, cut = .cut, uniqueBUYs = FALSE)
  
  list_cm %<>% rlist::list.append(cm[[1]])
  
  list_m %<>% rlist::list.append(cm[[3]])
  
  prediction <- map_dfr(cm[[2]], as.data.frame) %>% setNames('predict')
  
  data <- stock %>% 
    predict_tp(.tp, .sl, .h) %>% 
    filter(year(timestamp) %in% seq(2013, 2019, 1)) %>% 
    na.omit()
  
  data %<>% cbind(prediction)
  
  long_all <- data %>% longStrat(tp = .tp, sl = .sl, horizon = .h) 
  long_result <- summStrat(long_all)
  
  list_fr %<>% rlist::list.append(long_result)
}

list_m[[1]][[1]] %>% summary()

list_runtest <- list_fr %>% map(function(x){
  
  x[[2]] %>% 
    select(c('sell_date', 'profits_ind')) %>% 
    filter(profits_ind %in% c(.tp*1000, -.sl*1000)) %>% 
    pull(profits_ind) %>%  
    factor() %>% 
    tseries::runs.test()
})
  
