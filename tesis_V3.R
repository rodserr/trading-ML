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

# Inspect data----
# Read
EUR_USD <- read_csv('data/EUR_USD.csv', 
                     locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(-one_of('% var.')) %>% 
  mutate(Fecha = dmy(Fecha)) %>% 
  setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
  arrange(timestamp)

NIKKEI <- read_csv('data/Nikkei 225.csv', 
                     locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(-one_of(c('Vol.', '% var.'))) %>% 
  mutate(Fecha = dmy(Fecha)) %>% 
  setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
  arrange(timestamp)

WTI <- read_csv('data/WTI.csv', 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(-one_of(c('Vol.', '% var.'))) %>% 
  mutate(Fecha = dmy(Fecha)) %>% 
  setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
  arrange(timestamp)

FTSE <- read_csv('data/FTSE 100.csv', 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(-one_of(c('Vol.', '% var.'))) %>% 
  mutate(Fecha = dmy(Fecha)) %>% 
  setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
  arrange(timestamp)

IBEX <- read_csv('data/IBEX 35.csv', 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(-one_of(c('Vol.', '% var.'))) %>% 
  mutate(Fecha = dmy(Fecha)) %>% 
  setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
  arrange(timestamp)

BOVESPA <- read_csv('data/Bovespa.csv', 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(-one_of(c('Vol.', '% var.'))) %>% 
  mutate(Fecha = dmy(Fecha)) %>% 
  setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
  arrange(timestamp)

NASDAQ <- read_csv('data/NASDAQ.csv', 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(-one_of(c('Vol.', '% var.'))) %>% 
  mutate(Fecha = dmy(Fecha)) %>% 
  setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>% 
  arrange(timestamp)

# Search NA's
# which(is.na(raw_data))

# Plot
# xts(raw_data$close, raw_data$timestamp) %>% plot()

# Create dependent variable----

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

data <- BOVESPA %>% predict_tp(tp = 0.03, sl = 0.03, h = 20) %>%
  mutate(class = factor(class), 
         aux = ifelse(class == lag(class), 1, 0),
         class_2 = factor(ifelse(aux == 0 & class == 'buy', 'buy', 'stay'))) %>% #levels = c('stay', 'buy')))
  select(-one_of('aux', 'class'))

# which(data$class_2 == 'buy') %>% length()
# 
data %>%
  mutate(buy_close = if_else(class_2 == 'buy', close, NA_real_)) %>%
  ggplot(aes(x = timestamp, y = close)) +
  geom_line(colour = 'grey') +
  geom_point(aes(y = buy_close), colour = 'darkgreen', size = 0.5)

# Create factors----

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

# Create Interactions btwn factors(in case of 'MY interactions')----
data %<>% mutate(dip_action = adx - dip,
                 din_action = adx - din,
                 ema_50_action = close - ema_50,
                 ema_13_action = close - ema_13,
                 sar_action = sar - close,
                 bb_down_action = close - bb_down,
                 bb_up_action = close - bb_up,
                 macd_action = macd - macd_signal,
                 ema_macd_action = ema_13 - macd_action
)

data <- data %>% select(c('timestamp', 'lag_1', 'lag_3', 'lag_5', 'rsi', 'atr', 'dip_action',
                          'din_action', 'ema_50_action', 'ema_13_action', 'sar_action', 'bb_down_action',
                          'bb_up_action', 'macd_action', 'ema_macd_action', 'class_2'))

# Description Analysis----

# data %<>%
#   select(-one_of(
#                 'close', 'high', 'low', 'open', 'ema_13', 'lag_2', 'lag_4', 'macd'))

# Correlation plot
.plot_corr <- data %>%
  select(-one_of('timestamp', 'class')) %>%  
  # ,'close', 'high', 'low', 'open', 'ema_13', 'lag_2', 'lag_4', 'macd')) %>%
  na.omit() %>%
  cor() %>%
  corrplot::corrplot(method = 'number', type = 'lower', order = 'hclust')

# Box-plot's
.plot_OHLC <- data %>% 
  select(c('class', 'close', 'open', 'high', 'low', 'ma', 'ema', 'sar')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class)) +
  geom_boxplot()

.plot_lags <- data %>% 
  select(c('class', 'lag_1', 'lag_2', 'lag_3', 'lag_4', 'lag_5')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class)) +
  geom_boxplot()

.plot_adx_rsi <- data %>% 
  select(c('class', 'din', 'dip', 'adx', 'rsi')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class)) +
  geom_boxplot()

.plot_macd <- data %>% 
  select(c('class', 'macd', 'macd_signal', 'macd_hist')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class)) +
  geom_boxplot()

.plot_atr <- data %>% 
  select(c('class', 'atr')) %>% 
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = value, fill = class)) +
  geom_boxplot()

.plot_boxplot <- list(.plot_OHLC, .plot_lags, .plot_adx_rsi, .plot_macd, .plot_atr)

# Histogram
.ind <- names(data)[-c(1, 6)]
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
    select(c('class', i)) %>% 
    reshape2::melt() %>%
    ggplot(aes(x = value, color = class)) +
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

# Split data----
train <- data %>% filter(year(timestamp) %in% seq(2009, 2014, 1))

test <- data %>% filter(year(timestamp) %in% c(2015, 2016))

validation <- data %>% filter(year(timestamp) %in% c(2017, 2018))

# Create Folds---- 

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

# PCR: ALL interactions----

.PCA_cntrl <- trainControl(index = sampleFolds,
                          indexOut = OsampleFolds,
                          classProbs = TRUE,
                          savePredictions = TRUE,
                          summaryFunction = twoClassSummary,
                          preProcOptions = list(thresh = 0.85) #thresh = 0.85,pcaComp = 3)
)
# - high - low - open
PCA_model <- train(class_2 ~ (.)^2 - close  - macd - bb_down - bb_up - dip - din 
                   - adx - macd_signal - lag_1 - lag_3 - lag_5 - rsi - ema_50 
                   - atr - sar - ema_13,  
                   data = train[,-c(1,3,4,5)],
                   method = "glm",
                   family = 'binomial',
                   metric = 'ROC',
                   preProcess = c('pca'),
                   trControl = .PCA_cntrl
)

# Summary c(1,3,4,5)
PCA_model
summary(PCA_model)
PCA_model$preProcess$rotation
PCA_model$resample

# Prediction
PCA_pred <- predict(PCA_model, newdata = test[, -1], type = 'prob')

factor(ifelse(PCA_pred[, 'buy'] >= .7, 'buy', 'stay')) %>% 
  confusionMatrix(reference = test$class_2)

# PCR: MY interactions----

PCA_cntrl <- trainControl(index = sampleFolds,
                          indexOut = OsampleFolds,
                          classProbs = TRUE,
                          savePredictions = TRUE,
                          summaryFunction = twoClassSummary,
                          preProcOptions = list(thresh = 0.85) #thresh = 0.85,pcaComp = 3)
)

PCA_model <- train(class_2 ~ .,  
                   data = train[,-1],
                   method = "glm",
                   family = 'binomial',
                   metric = 'ROC',
                   preProcess = c('pca'),
                   trControl = PCA_cntrl
)

# Summary
PCA_model
summary(PCA_model)
PCA_model$preProcess$rotation
PCA_model$resample

# Prediction
PCA_pred <- predict(PCA_model, newdata = test[, -1], type = 'prob')

cm <- confusionMatrix(data = factor(ifelse(PCA_pred[, 'buy'] >= .5, 'buy', 'stay')),
                reference = test$class_2)

# Summary----
  pROC::roc(test$class,
      PCA_pred[,1],
      levels = rev(levels(test$class))) %>% 
  plot(print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)

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
  select(c('CutOff', 'Pos Pred Value', 'cases'))

maxcoff <- which.max(cases$'Pos Pred Value')

coff <- slice(cases, maxcoff)

# plotcoff <- plot(x = cases$CutOff, y = cases$'Pos Pred Value', type = 'l')

return(coff)
}

getROC(prueba, test)
# plot(x = result_1$CutOff, y = result_1$'Pos Pred Value', type = 'l')
# plot(x = result_1$CutOff, y = result_1$'Accuracy', type = 'l')

# Function----

PCRegression <- function(serie, tp, sl, h){
  
  # Create class
  data <- predict_tp(serie, tp, sl, h) %>% 
    mutate(aux = ifelse(class == lag(class), 1, 0),
           class_2 = factor(ifelse(aux == 0 & class == 'buy', 'buy', 'stay'))) %>% 
    select(-one_of('aux', 'class'))
  
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

  test <- data %>% filter(year(timestamp) %in% c(2015, 2016))

  validation <- data %>% filter(year(timestamp) %in% c(2017, 2018))

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
  PCA_pred <- predict(PCA_model, newdata = test[,-1], type = 'prob')

  cutoff <- getROC(PCA_pred, test)
  
return(cutoff) 
  
}

result <- list()
for(sl in c(0.02, 0.03, 0.04, 0.05, 0.06)) 
  for(tp in c(0.02, 0.03, 0.04, 0.05, 0.06))
    for(h in c(5, 10, 20)){
      
      parameters <- c(sl, tp, h) %>% 
        matrix() %>%
        t() %>%
        as.data.frame() %>% 
        setNames(c('sl', 'tp', 'h'))
      
      ppv <- PCRegression(FTSE, sl, tp, h)
      ppv %<>% cbind(parameters)
      result %<>% rlist::list.append(ppv)
      
    }

FTSE_result <- map_dfr(result, as.data.frame)



