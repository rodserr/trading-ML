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

test$predict <- ifelse(test_pred$buy >= .5, 'buy', 'stay')

longStrat <- function(back_data, cap_inic = 100000, comission = 0, sl = 0.02,
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

long_all <- data %>% longStrat(tp = .tp, sl = .sl, horizon = .h) 
long_result <- summStrat(long_all)
long_tranc <- long_result[[2]]

long_result[[5]]

.plot_curve <- gridExtra::arrangeGrob(long_result[[5]] + labs(title = '4hr - ema 13 - 3 atr', x = NULL),
                               long_result[[4]], heights = c(5,2))
grid::grid.draw(.plot_curve)
