setwd("C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터")
library(tidyverse)
library(forecast)
library(MLmetrics)
library(data.table)
library("timetk")
library("data.table")
library("gridExtra")
library("caret")
library("MLmetrics")
library("lubridate")
library("factoextra")
library("corrplot")
library("Hmisc")


economic_data <- "last_data_set.csv" %>% fread(encoding = "UTF-8")
economic_data <- economic_data[, -"consumer_index"]
economic_data <- economic_data[, -'V1']
economic_data %>% head # 날짜 미래부터 과거순임.
economic_data <- economic_data[economic_data[, "Date"] %>% order]
rownames(economic_data) <- NULL
economic_data

##################################################################################
#  [ 시계열 교차검증 ]
# - 비 누적 교차검증 사용.
###############################################################################



window_size <- c(90, 95, 100, 105, 110)

window_tuning <- expand.grid(window_size, NA)
names(window_tuning) <- c("#_of_window", "RMSE")

window_tuning


t = 1
for (k in window_size) {
  n = dim(economic_data)[1] # 끝행 값
  m = n-k-61+1 # 자르는 처음 값
  
  data_cut = economic_data[m:n,]
  
  x=1:k # 윈도우 사이즈 
  
  folds <-  list() 
  for(i in 1:61){ # 여기서 61은 며칠 전까지 CV돌릴것인가를 의미 
    folds[[i]]= x +i- 1
  } # 각 리스트 번호는 몇번쨰 CV인지를 나타냄. 리스트번호가 60까지 있다면 60일 전까지 CV한다는거.
  # folds의 각 list가 CV에 사용할 x들임
  
  folds_test=list()
  for (i in 1:61){
    folds_test[[i]]=folds[[i]][k]+1
  } # folds_test가 각 CV마다 테스트할 시점들을 모아놓은 거.
  
  eco_y <- ts(data_cut$BTC_price) # 예측하고자 하는 y값
  eco_x <- data_cut[, c("KOSPI", "VKOSPI", "MA", "PYPL", "SQ", "vidente")] # 그레인져 인과검정을 기각한 변수들만 넣음.
  
  eco_x_test <- list()
  eco_y_test <- list()
  
  for (i in (1:length(folds_test))){
    eco_y_test[[i]] <- ts(eco_y[folds_test[[i]]])
    eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
  } # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값
  
  eco_yfit_1 <- c()
  for (i in (1:length(folds_test))) {
    print(paste0(i, "th"))
    y_train <- ts(eco_y[folds[[i]]])   # y_train은 train하는 시점들의 y값(과거 y값으로 미래 값 예측하니까)
    xreg_train <- as.matrix(eco_x[folds[[i]],]) # xreg_train은 train하는 시점들의 외생변수값
    eco_arima <- auto.arima(y_train, xreg = as.matrix(xreg_train), approximation = FALSE)
    y_fit1 <- forecast::forecast(eco_arima, xreg = eco_x_test[[i]], h=1)$mean
    eco_yfit_1 <- c(eco_yfit_1, y_fit1)
  }
  
  
  
  eco_test_final_y <- unlist(eco_y_test) # eco_test_final_y는 eco_test_y값에서 하나의 벡터로 넣은것.
  eco_rmse1 <- RMSE(eco_yfit_1, eco_test_final_y)
  window_tuning[t,"RMSE"] <- eco_rmse1
  t = t+1
}


##########다시 적합############
economic_data <- fread("C:/Users/User/Desktop/주제분석_데이터파일/final_data_frame.csv")
economic_data <- economic_data[, -'V1']
economic_data %>% head # 날짜 미래부터 과거순임.
economic_data <- economic_data[economic_data[, "Date"] %>% order]
#economic_data %>% tail
#economic_data[dim(economic_data)[1]]

k = 60
n = dim(economic_data)[1] # 끝행 값
m = n-k-61+1 # 자르는 처음 값

data_cut = economic_data[m:n,]

x=1:k # 윈도우 사이즈 

folds <-  list() 
for(i in 1:61){ # 여기서 61은 며칠 전까지 CV돌릴것인가를 의미 
  folds[[i]]= x +i- 1
} # 각 리스트 번호는 몇번쨰 CV인지를 나타냄. 리스트번호가 60까지 있다면 60일 전까지 CV한다는거.
# folds의 각 list가 CV에 사용할 x들임

folds_test=list()
for (i in 1:61){
  folds_test[[i]]=folds[[i]][k]+1
} # folds_test가 각 CV마다 테스트할 시점들을 모아놓은 거.

eco_y <- ts(data_cut$BTC_price) # 예측하고자 하는 y값
eco_x <- data_cut[, c("KOSPI", "VKOSPI", "MA", "PYPL", "SQ", "vidente")] # 그레인져 인과검정을 기각한 변수들만 넣음.

eco_x_test <- list()
eco_y_test <- list()

for (i in (1:length(folds_test))){
  eco_y_test[[i]] <- ts(eco_y[folds_test[[i]]])
  eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
} # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값

eco_yfit_1 <- c()
for (i in (1:length(folds_test))) {
  print(paste0(i, "th"))
  y_train <- ts(eco_y[folds[[i]]])   # y_train은 train하는 시점들의 y값(과거 y값으로 미래 값 예측하니까)
  xreg_train <- as.matrix(eco_x[folds[[i]],]) # xreg_train은 train하는 시점들의 외생변수값
  eco_arima <- auto.arima(y_train, xreg = as.matrix(xreg_train), approximation = FALSE)
  y_fit1 <- forecast::forecast(eco_arima, xreg = eco_x_test[[i]], h=1)$mean
  eco_yfit_1 <- c(eco_yfit_1, y_fit1)
}

eco_test_final_y %>% length
eco_test_final_y

eco_test_final_y <- unlist(eco_y_test) # eco_test_final_y는 eco_test_y값에서 하나의 벡터로 넣은것.
eco_rmse1 <- RMSE(eco_yfit_1, eco_test_final_y)
eco_rmse1

BTC_price_true = eco_test_final_y
BTC_price_fitted =eco_yfit_1
Date = data_cut$Date[(dim(data_cut)[1]-60): dim(data_cut)[1]]


fit_frame <- cbind( Date, BTC_price_true, BTC_price_fitted) %>% data.frame()

fit_frame %>% str()

ggplot(data = fit_frame, aes(x = Date))+
  geom_line(aes(y = BTC_price_true, colour = 'blue'))+
  geom_line(aes(y = BTC_price_fitted, colour = 'red'))

fit_frame %>% str()





