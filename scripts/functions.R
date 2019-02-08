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

summStrat <- function(back_data){
  
  #Info from all data (necesary for some indicators)----
  temp <- difftime(back_data$timestamp[2], back_data$timestamp[1], units = 'mins') %>% as.numeric()
  longevity <- difftime(back_data$timestamp[nrow(back_data)], back_data$timestamp[1], units = 'mins') %>% as.numeric()
  
  long_in_month <- seq.POSIXt(as.POSIXct(back_data$timestamp[1]),
                              as.POSIXct(back_data$timestamp[nrow(back_data)]), 
                              by = 'month') %>% length()
  
  long_in_day <- seq.POSIXt(as.POSIXct(back_data$timestamp[1]),
                            as.POSIXct(back_data$timestamp[nrow(back_data)]), 
                            by = 'day') %>% length()
  
  transactions <- back_data %>% 
    filter(sell == 1) %>% 
    select(buy_date, sell_date, buy_price, sell_price, comision, quantity, cap, salida, buy_class) %>% 
    mutate(quantity_buy = quantity/sell_price,
           prof_loss = sell_price - buy_price,
           profits_ind = round(prof_loss*quantity_buy - comision, digits = 4),
           return_trade = round(profits_ind/(quantity_buy*buy_price), digits = 4),
           risk_perc = round(profits_ind/(cap-profits_ind), digits = 4))
  
  #Basic Indicators----
  cap_init <- transactions$cap[1] - transactions$profits_ind[1]
  total_net_profit <- sum(transactions$profits_ind)
  return_accum <- total_net_profit/cap_init
  annual_return <- - 1 + (return_accum + 1) ^ (365/long_in_day)
  total_nro_trades <- length(transactions$buy_date)
  percent_profitable <- sum(transactions$profits_ind >= 0) / total_nro_trades
  percent_loss <- sum(transactions$profits_ind < 0) / total_nro_trades
  avg_profits <- sum(if_else(transactions$profits_ind >= 0, transactions$profits_ind, 0) ) / sum(transactions$profits_ind >= 0)
  avg_loss <- sum(if_else(transactions$profits_ind < 0, transactions$profits_ind, 0) )*-1 / sum(transactions$profits_ind < 0)
  profit_factor <-  (avg_profits*percent_profitable)/(avg_loss*percent_loss)
  
  perc_stra_exit <- (transactions$salida %>% .[. == 'Strategy'] %>% length())/total_nro_trades
  perc_stoploss_exit <- 1-perc_stra_exit
  
  interval_trans <- difftime(transactions$sell_date, transactions$buy_date, units = 'hours') %>% abs()
  mean_duration_trans <- mean(interval_trans) %>% round(1)
  trans_over_24h <- sum(interval_trans > 24) / total_nro_trades
  
  largest_win_trade <- max(transactions$profits_ind)
  largest_loss_trade <- min(transactions$profits_ind)
  aux_consec <- if_else(transactions$profits_ind < 0, -1, 1) %>% rle()
  max_consectv_wins <- max(aux_consec$lengths * aux_consec$values)
  max_consectv_loss <- min(aux_consec$lengths * aux_consec$values)*-1
  
  aux_pprof <- filter(transactions, profits_ind > 0)
  avg_bars_win <- mean(abs(difftime(aux_pprof$sell_date, aux_pprof$buy_date, units = 'mins')) / temp) %>% as.numeric()
  
  aux_nprof <- filter(transactions, profits_ind < 0)
  avg_bars_loss <- mean(abs(difftime(aux_nprof$sell_date, aux_nprof$buy_date, units = 'mins')) / temp) %>% as.numeric()
  
  trading_months <- long_in_month
  percent_time_in_market <- abs( as.numeric(sum( difftime(transactions$sell_date, transactions$buy_date, units = 'mins') ) ) ) / longevity
  
  perc_predict <- sum(transactions$buy_class == 'buy')/total_nro_trades
  
  
  #Maximum Drawdown & other indicators----
  cap_aux <- c(cap_init, transactions$cap)
  
  transactions <- transactions %>% mutate(returns = (log(cap_aux) - log(lag(cap_aux)))[-1])
  
  transactions_cap <- as.data.frame(transactions[, c('returns')]) %>%
    setNames('returns')
  
  row.names(transactions_cap) <- transactions$buy_date
  max_drawdown <- PerformanceAnalytics::maxDrawdown(transactions_cap)
  max_drawdown %<>%  ifelse(. == 0, 0.01, .) # to make sure risk return can be calculated (back rolling window)
  
  
  sharpe_ratio <- mean(transactions$profits_ind)/sqrt(var(transactions$profits_ind))
  calmar_ratio <- mean(transactions$profits_ind)/max_drawdown
  risk_return <- return_accum/max_drawdown
  
  #Maximum Adverse Excursion----
  mae <- rep(0, nrow(transactions))
  
  for(i in 1:nrow(transactions)){
    
    aux_buy <- which(back_data$timestamp == transactions$buy_date[i]) + 1
    aux_sell <- which(back_data$timestamp == transactions$sell_date[i])
    
    mae[i] <- min(back_data$low[aux_buy:aux_sell])/transactions$buy_price[i] - 1
  }
  
  transactions %<>% cbind(mae)
  
  #min(negative) = worst
  mae_max <- transactions %>% filter(salida == 'Strategy') %>% pull(mae) %>% min()
  
  #max(negative) = best
  mae_min <- transactions %>% filter(salida == 'Strategy') %>% pull(mae) %>% max()
  
  mae_mean <- transactions %>% filter(salida == 'Strategy') %>% pull(mae) %>% mean()
  
  
  #Make resume data frame----
  resume <- data.frame(total_net_profit, return_accum, annual_return, total_nro_trades, perc_predict,
                       percent_profitable,
                       percent_loss, avg_profits, avg_loss, perc_stra_exit, perc_stoploss_exit,
                       profit_factor, max_drawdown, mae_min, mae_mean, mae_max, risk_return, sharpe_ratio,
                       calmar_ratio, mean_duration_trans, trans_over_24h, largest_win_trade, largest_loss_trade,
                       max_consectv_wins, max_consectv_loss, avg_bars_win, avg_bars_loss, trading_months,
                       percent_time_in_market)
  
  #Plots----
  
  # Register initials values
  aux_init <- data.frame(sell_date = back_data$timestamp[1],
                         cap = cap_init,
                         returns = 0)
  
  tranc_plot <- rbind(aux_init, transactions[, c('sell_date', 'cap', 'returns')]) %>% 
    mutate(sell_date = as.POSIXct(sell_date))
  
  capCurve <- tranc_plot %>%
    ggplot(aes(x = sell_date)) +
    geom_line(aes(y = cap, colour = 'red')) +
    theme(legend.position="none") +
    labs(x = 'timestamp') +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%b-%y")
  
  return_plot <- tranc_plot %>%
    ggplot(aes(x = sell_date)) +
    geom_line(aes(y = returns), col = 'dark blue') +
    theme(legend.position="none") +
    labs(x = 'timestamp') +
    scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::percent) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%b-%y")
  
  #DrawDown Plot
  returns <- transactions_cap$returns
  cumret <- cumprod(returns + 1)
  maxret <- cummax(cumret)
  
  dd <- data.frame(timestamp = c(back_data$timestamp[1], as_date(row.names(transactions_cap))),
                   drdw = c(0, round(cumret/maxret - 1, 5)),
                   cap = c(0, cumsum(returns))) %>% 
    mutate(timestamp = as.POSIXct(timestamp))
  
  
  dd_plot <- dd %>% ggplot(aes(x = timestamp)) +
    geom_ribbon(aes(ymin = drdw, ymax = 0), color = 'dark green', fill = 'dark green') +
    geom_line(aes(y = cap), color = 'red') +
    theme(legend.position = "none") +
    labs(y = 'cummulative returns') +
    scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::percent) +
    scale_x_datetime(date_breaks = "4 month", date_labels = "%b-%y")
  
  # Return----
  return(list(resume, transactions, capCurve, return_plot, dd_plot))
  # return(transactions)
  
}