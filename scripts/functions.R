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

createIndicators <- function(data, full = FALSE){

  .adx <- ADX(data[,c('high', 'low', 'close')], n = 14, maType = 'EMA') %>%
    as.data.frame() %>% setNames(c('dip', 'din', 'dx', 'adx'))
  
  .macd = MACD(data$close, maType = 'EMA') %>% as.data.frame() %>% 
    setNames(c('macd', 'macd_signal'))
 
  if(full){
    
    data %<>% cbind(.adx[,-3], .macd) %>% 
      mutate(lag_1 = close / lag(close) - 1,
             lag_2 = close / lag(close, 2) - 1,
             lag_3 = close / lag(close, 3) - 1,
             lag_4 = close / lag(close, 4) - 1,
             lag_5 = close / lag(close, 5) - 1,
             rsi = RSI(close, n = 14),
             # ema_50 = SMA(close, n = 50),
             # ema_13 = EMA(close, n = 13),
             bb_down = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,1],
             bb_up = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,3],
             # sar = SAR(data[,c('high', 'low')])[,1],
             atr = ATR(data[,c('high', 'low', 'close')], n = 14)[,2]
      ) %>% 
      na.omit()
    
  }else{
    
    data %<>% cbind(adx = .adx[,-c(1,2,3)], .macd) %>% 
      mutate(lag_1 = close / lag(close) - 1,
             lag_3 = close / lag(close, 3) - 1,
             lag_5 = close / lag(close, 5) - 1,
             rsi = RSI(close, n = 14),
             bb_up = BBands(data[,c('high', 'low', 'close')], n = 14, sd = 2.5)[,3],
             atr = ATR(data[,c('high', 'low', 'close')], n = 14)[,2]
      ) %>% 
      na.omit()
  }
  
  return(data)
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
  data %<>% createIndicators() %>% select(-one_of('high', 'open', 'low'))
  
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
                       data = train[,-1],
                       method = "glm",
                       family = 'binomial',
                       metric = 'ROC',
                       preProcess = c('pca'),
                       trControl = .PCA_cntrl
    )
    
    # Prediction
    PCA_pred <- predict(PCA_model, newdata = validation[,-1], type = 'prob')
    
    prediction <- ifelse(predict(PCA_model, newdata = validation[,-1], type = 'prob')[, 'buy'] >= cut, 
                         'buy', 'stay') %>% factor()
    
    aux_cm <- confusionMatrix(prediction, reference = validation$class_2)
    
    list_cm %<>% rlist::list.append(aux_cm)
    
    list_pred %<>% rlist::list.append(prediction) 
    
    list_model %<>% rlist::list.append(PCA_model) 
  }
  
  list_final <- list(list_cm, list_pred, list_model) 
  
  return(list_final)
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