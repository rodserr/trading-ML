#------Libraries------
library(lubridate)
library(dplyr)
library(TTR)
library(ggplot2)
library(magrittr)
library(zoo)
library(purrr)
library(QuantTools)
library(plotly)
library(readr)
library(PerformanceAnalytics)


#------FUNCTIONS-----------

result_FTSE %>% filter(ppv > 0.75) %>% apply(2, mean)

final_model <- PCRegression(IBEX, 0.02, 0.06, 10, uniqueBUYs = FALSE, model = TRUE)

validation <- validationSet(IBEX, 0.02, 0.06, 10, uniqueBUYs = FALSE)

test <- testSet(IBEX, 0.025, 0.05, 15, uniqueBUYs = FALSE)

val_pred <- predict(final_model, newdata = validation[,-1], type = 'prob')

factor(ifelse(val_pred[, 'buy'] >= .65, 'buy', 'stay')) %>% 
  confusionMatrix(reference = validation$class_2)

factor(ifelse(predict(final_model, newdata = test[,-1], type = 'prob')[, 'buy'] >= .6, 
              'buy', 'stay')) %>% 
  confusionMatrix(reference = test$class_2)

validation$predict <- ifelse(val_pred$buy >= .65, 'buy', 'stay')

longStrat <- function(back_data, cap_inic = 100000, comission = 0, sl = 0.06,
                      tp = 0.02, sli_pag = 0, horizon = 10){
  
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
  atr_in <- 0
  stop_loss <- 0
  take_profit <- 0
  
  na_count <- sapply(back_data, function(.x) sum((is.na(.x)))) %>% which.max() %>% names()
  
  for(i in 2:nrow(back_data)) {
    if(!is.na(back_data[i-1, na_count])){
      
      # Buy
      if(back_data$predict[i] == 'buy' &
         last_balance == 0 & last_cap >= last_quantity)
      {
        last_balance <- 1
        last_buy_price <- back_data$close[i] * (1+sli_pag)
        last_quantity <- 1 * last_buy_price
        
        stop_loss <- last_buy_price*(1 - sl)
        take_profit <- last_buy_price*(1 + tp)
        
        last_buy_date <- back_data$timestamp[i]
        last_comision <- last_quantity * comission
        last_cap <- last_cap - last_quantity
        buy[i] <- 1
        atr_in <- back_data$atr[i]
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
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
        last_quantity <- (last_quantity/last_buy_price) * sell_price[i]
        last_comision <- last_comision + (last_quantity * comission)
        last_cap <- last_cap + last_quantity - last_comision
        sell[i] <- 1
        salida[i] <- "Stop Loss"
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
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
        last_quantity <- (last_quantity/last_buy_price) * sell_price[i]
        last_comision <- last_comision + (last_quantity * comission)
        last_cap <- last_cap + last_quantity - last_comision
        sell[i] <- 1
        salida[i] <- "Strategy"
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
      }
      
      # Sell by horizon
      if(last_balance == 1 &&
         (as.numeric(back_data$timestamp[i] - last_buy_date) > horizon &
          back_data$timestamp[i] != last_buy_date))
      { 
        last_balance <- 0
        buy_date[i] <- last_buy_date
        buy_price[i] <- last_buy_price
        sell_date[i] <- back_data$timestamp[i]
        sell_price[i] <- back_data$close[i]* (1 - sli_pag)
        last_quantity <- (last_quantity/last_buy_price) * sell_price[i]
        last_comision <- last_comision + (last_quantity * comission)
        last_cap <- last_cap + last_quantity - last_comision
        sell[i] <- 1
        salida[i] <- "Stop Loss"
        
        quantity[i] <- last_quantity
        comision[i] <- last_comision
      }
      
      balance[i] <- last_balance
      cap[i] <- last_cap
      
    }
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
  
  return(back_data)
}

source('scripts/summStrat.R')
#------QUICK TRYs-----------

long_all <- validation %>% longStrat() 
long_result <- summStrat(long_all, .position = 'long')
long_tranc <- long_result[[2]]

long_result[[5]]

.plot_curve <- gridExtra::arrangeGrob(long_result[[5]] + labs(title = '4hr - ema 13 - 3 atr', x = NULL),
                               long_result[[4]], heights = c(5,2))
grid::grid.draw(.plot_curve)

#-----WalkForward-----

# Create list with walk-windows
source('functions/backRollWin.R')
source('functions/summStratinSample.R')
# source('functions/summStrat_WF.R')

temp <- '1 hour'
.back_data <- transPeriod(raw_data, by = temp) %>% 
  backRollWin(.sample_periods = 3, .out_sample_periods = 1)

sample_wind <- .back_data[seq(1,22,2)]

osample_wind <- .back_data[seq(2,22,2)]

# 1st LOOP: Choose best fits in every sample periods

params_ema <- c(10, 13, 16, 19, 22, 26, 29, 32)
params_atr <- c(1, 2, 3)

aux_best_fit <- data.frame()
best_fit <- data.frame()
sample_result <- data.frame()

system.time(
  for (j in 1:length(sample_wind)){
    for (i in params_ema) for (at in params_atr){
      
      # Auxiliar to iterate the parameters variations
      .aux_samplefit <- sample_wind[[j]] %>% 
        indicators(.ema = i) %>% 
        longStrat(.atr_coef = at) %>% 
        summStratinSample()
      
      # fits Results of month j
      aux_best_fit %<>% rbind(cbind(param_ema = i, 
                                    param_atr = at,
                                    .aux_samplefit[[1]]
      )
      )
    }
    
    # Best fit in month j
    opti <- aux_best_fit %>% 
      filter(max_drawdown < 0.30) %>%   
      filter(risk_return > quantile(risk_return, probs = 0.95)) %>%  # Keep best 5% risk_return  
      slice(which.max(return_accum))    # Best Fit = max(return_accum)
    
    # Save parameters
    best_fit %<>% rbind(opti[, 1:2])
    
    # Save results of In-Sample's
    sample_result %<>% rbind(opti[, -c(1, 2)])
    
    # best_fit %<>% rbind(cbind(aux_best_fit$param_ema[which.max(aux_best_fit$risk_return)],
    #                           aux_best_fit$param_atr[which.max(aux_best_fit$risk_return)]))
    
    # Clean storage of result to month j+1
    aux_best_fit <- data.frame()
    
  })

beepr::beep()
best_fit %<>% setNames(c('ema', 'atr'))


# 2nd LOOP: Apply strategy with best fit in corresponding out-sample periods
osample_result <- data.frame()
osample_tranc <- data.frame()
capital <- 50000

system.time(
  for (j in 1:length(osample_wind)){
    
    parameters <- best_fit %>% slice(j)
    
    # Get number of periods necesary to create indicators in corresponding out-sample
    .sample_history <- sample_wind[[j]] %>% tail(max(parameters))
    
    # Run strategy in out-sample j
    aux_osamplefit <- rbind(.sample_history, osample_wind[[j]])  %>% 
      indicators(.ema = parameters$ema) %>% 
      longStrat(.ci = capital, 
                .atr_coef = parameters$atr) %>% 
      summStratinSample()
    
    # Storage Transactions & general results
    osample_tranc %<>% rbind(aux_osamplefit[[2]])
    osample_result %<>% rbind(aux_osamplefit[[1]])
    
    # Update capital according to last closed trancs, to start every month with real-time capital
    capital <- osample_tranc$cap %>% last()
  })

# # Get final results
# resume_strategy <- osample_tranc %>% summStrat_WF(temp_in_mins = (4*60),
#                                                   from = raw_data$timestamp[1], 
#                                                   to = raw_data$timestamp[nrow(raw_data)])

params <- apply(best_fit, 1, paste, collapse = '-')

summ_sample <- cbind(sample_result, 
                     temp = rep(temp, nrow(sample_result)),
                     strategy = 'impulso',
                     sample = 'in',
                     window = seq(1, nrow(sample_result), 1),
                     params)

summ_osample <- cbind(osample_result, 
                      temp = rep(temp, nrow(osample_result)),
                      strategy = 'impulso',
                      sample = 'out',
                      window = seq(1, nrow(sample_result), 1),
                      params)

trades_2 <- cbind(osample_tranc, 
                  temp = rep(temp, nrow(osample_tranc)),
                  strategy = 'impulso')

summ_2 <- rbind(summ_sample, summ_osample) %>% arrange(window, sample)

trades_total <- rbind(trades_1, trades_2)

summ_total <- rbind(summ_1, summ_2)

# Export Results to Dashboard

summ_total %>% 
  write_csv('/home/leonardo/Desktop/Rodrigo/Dashboard Backtesting/results/impulso/summary_V3.csv')

trades_total %>% 
  write_csv('/home/leonardo/Desktop/Rodrigo/Dashboard Backtesting/results/impulso/trades_V3.csv')



