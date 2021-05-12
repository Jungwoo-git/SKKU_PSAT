
##### 랜덤포레스트 factor 변수만 넣음 

## [ library ]
library("tidyverse")
library("data.table")
library("gridExtra")
library("caret")
library("MLmetrics")
library("lubridate")
library("factoextra")
library("corrplot")
library("Hmisc")
library("forecast")
library("randomForest")
library(urca)
library(tseries)
library(vars)

RNGkind(sample.kind = "Rounding") # setseed해도 자꾸 바뀌길래 구글링해서 넣은 코드


## [ data loading ] 
setwd("C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터")
rf_data1 <- "factor_data.csv" %>% fread(data.table = F,encoding = "UTF-8") # 데이터: 2018-01-01~ 2021-03-31


window_size <- c(50, 70, 90, 110)
mtry = c(3, 4, 5)
ntree = c(50, 100, 150, 200)
window_rf_RMSE <- expand.grid(mtry = mtry,
                              ntree = ntree,
                              window_size = window_size,
                              RMSE = NA)
window_rf_RMSE


### 단위근 검정 --> 기각하면 정상성 만족. 정상성 만족되지 않은 경우엔 차분해서 랜포 돌릴것.
# BTC_price 의 단위근 검정
rf_data1[, "volume"] <- (rf_data1[, "volume"] %>% as.numeric) # volume의 class를 numeric으로 바꿈

for(i in 2:ncol(rf_data1)){
  print(paste0(colnames(rf_data1)[i],"   ADFtest"))
  print(adf.test(rf_data1[,i])) 
}
# [ 결과 ]
# 정상 데이터: volume, sentiment, F2, 
# 비정상 데이터: 그 외 


### 차분 후 단위근 검정
rf_diff_data <- rf_data1[-1,]

for(i in 2:ncol(rf_diff_data)){
  rf_diff_data[,i] <- rf_data1[,i] %>% diff()
}

for(i in 2:ncol(rf_diff_data)){
  print(paste0(colnames(rf_diff_data)[i],"   ADFtest"))
  print(adf.test(rf_diff_data[,i])) 
}
# [ 결과 ]
# 정상 데이터: 모두 
rf_diff_data %>% names

### 정상화된 변수로 데이터 교체
# VIX, VKOSPI, net_point_by_word 제외하고 전부 차분
rf_diff_data <- rf_data1[-1,]
rf_diff_data[, "BTC_price"] <- rf_data1[, "BTC_price"] %>% diff
rf_diff_data[, "volume"] <- rf_data1[-1, "volume"]
rf_diff_data[, "sentiment"] <- rf_data1[-1, "sentiment"]
rf_diff_data[, "F1"] <- rf_data1[, "F1"] %>% diff
rf_diff_data[, "F2"] <- rf_data1[-1, "F2"]
rf_diff_data[, "F3"] <- rf_data1[, "F3"] %>% diff
rf_diff_data[, "F4"] <- rf_data1[, "F4"] %>% diff

rf_data <- rf_diff_data

######################## 랜포 튜닝


set.seed(1234)

t = 1 ## 랜포 돌려서 window_rf_RMSE 각 mtry, ntree, windowsize 당 RMSE 평균 넣을때 row 번호  
for (k in window_size) {
  n = dim(rf_data)[1] # 끝행 값
  m = n-k-61+1 # 자르는 처음 값
  
  data_cut = rf_data[m:n,]
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
  eco_x <- data_cut # 모든 변수를 다 넣음.
  
  eco_x_test <- list()
  eco_y_test <- list()
  
  for (i in (1:length(folds_test))){
    eco_y_test[[i]] <- ts(eco_y[folds_test[[i]]])
    eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
  } # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값
  
  RMSE_save <- NULL
  for (mt in mtry) {
    for (j in ntree) {
      for ( i in (1:length(folds_test))) {
        rf_model <- randomForest(BTC_price~ volume + sentiment + F1 + F2 + F3 + F4,
                                 data = rf_data[(nrow(rf_data)-60-k+(i-1)) : (nrow(rf_data)-60-1+(i-1)), ], 
                                 mtry = mt, ntree = j)
        print(paste0("mtry :", mt, ",  ntree :", j ,", ",i, "th"))
        eco_test_final_y <- unlist(eco_y_test)
        rf_fit <- predict(rf_model, newdata = eco_x_test[[i]])
        RMSE_save <- c(RMSE_save, RMSE(rf_fit, eco_test_final_y[i]))
      }
      window_rf_RMSE[t, "RMSE"] <- mean(RMSE_save)
      t = t + 1
      RMSE_save <- NULL # RMSE_save 초기화
    }
  }
}

window_rf_RMSE[which(window_rf_RMSE[,"RMSE"] == min(window_rf_RMSE[,"RMSE"])), ] # 최적의 mtry, ntree, window_size, RMSE 출력
rf_fit_model <- randomForest(BTC_price~ volume + sentiment + F1 + F2 + F3 + F4,
                             data = data_cut, mtry = 3, ntree = 50)

##### 차분된 시계열 그리기

n1 <- nrow(rf_data) -60
n2 <- nrow(rf_data)
n3 <- which(names(rf_data) %in% c("Date", "BTC_price"))
rf_test_x <- rf_data[n1:n2, -n3]
BTC_price_fitted <- predict(rf_fit_model, rf_test_x)

date_ <- rf_data[n1:n2, "Date"]
BTC_price_true <- rf_data[n1:n2, "BTC_price"]

fit_frame <- cbind( date_, BTC_price_true, BTC_price_fitted)
fit_frame <- fit_frame %>% as.data.frame

ggplot(data = fit_frame, aes(x = date_))+
  geom_line(aes(y = BTC_price_true))+
  geom_line(aes(y = BTC_price_fitted, colour = 'red'))




##### 차분 풀어준 시계열 

ori_price_true <- rf_data1[(dim(rf_data1)[1] - dim(fit_frame)[1]): dim(rf_data1)[1], "BTC_price"]

ori_price_fitted_k <- c(rf_data1[dim(rf_data1)[1] - dim(fit_frame)[1], "BTC_price"], BTC_price_fitted) %>% diffinv
ori_price_fitted <- ori_price_fitted_k[-1]


fit_frame[,"date_"] <- fit_frame[, "date_"] %>% as.Date
date1 <- rbind("18656", fit_frame[, "date_"]%>% as.matrix) %>% as.numeric %>% as.Date
fit_frame[, "BTC_price_true"]

original_fit_frame <- cbind(date1, ori_price_true, ori_price_fitted)
original_fit_frame <- original_fit_frame %>% as.data.frame 
original_fit_frame[, "date1"] <- original_fit_frame[, "date1"] %>% as.matrix %>% as.Date
names(original_fit_frame) <- c("Date", "original_BTC_price", "BTC_price_fit")


original_fit_frame[, "ori_price_true"] %>% length
original_fit_frame[, "ori_price_fitted"] %>% length

ggplot(data = original_fit_frame, aes(x = Date))+
  geom_line(aes(y = original_BTC_price))+
  geom_line(aes(y = BTC_price_fit, colour = 'red'))

RMSE(original_fit_frame[,"original_BTC_price"], original_fit_frame[,"BTC_price_fit"])

#### importance plot
rf_best <- randomForest(BTC_price~ volume + sentiment + F1 + F2 + F3 + F4, 
                        data = rf_data,  ## cross validation 하는게 아니라 주어진 기간에서 중요변 ㅂ수를 택하는거니까 전체데이터
                        mtry = 3, ntree = 50)
rf_impo <- cbind(rf_best$importance %>% as.data.frame,
                 rownames = rf_best$importance %>% rownames)


varImpPlot(rf_best)

