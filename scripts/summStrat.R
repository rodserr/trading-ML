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