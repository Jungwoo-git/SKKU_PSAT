
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



## [ data loading ] 
set.seed(1234)

setwd("C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터")
rf_data1 <- "last_data_set.csv" %>% fread(data.table = F,encoding = "UTF-8") # 데이터: 2018-01-01~ 2021-03-31
rf_data1 <- rf_data1[,-1] # 데이터프레임에서 쓰잘데기 없는 V1 column 삭제 
rf_data1 <- rf_data1[, -which(rf_data1 %>% names == 'consumer_index')] # 데이터프레임에서 소비자 물가지수 삭제 
rf_data1 <- rf_data1[rf_data1[, 'Date'] %>% order, ]
rownames(rf_data1) <- NULL

remove_index <- which(names(rf_data1) %in% c("Gold_price", "Bond_yield", "USD_exchange_rate", "BTC_circulation"))
rf_data1 <- rf_data1[, -remove_index] # 그레인져 인과성 검정으로 무의미한 변수들 제거

window_size <- c(50, 100, 150, 200, 250, 300, 350, 400)
mtry = c(1, 3, 5, 7)
ntree = c(5, 10, 15, 30, 50, 70)
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
# 정상 데이터: VIX, VKOSPI, net_point_by_word, volume
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
# 비정상 데이터: BTC_circulation
# 정상 데이터: 그 외

# BTC_circulation 2차 차분 후 단위근 검정
rf_diff_data[, "BTC_circulation"] %>% diff %>% adf.test

# [ 결과 ]
# 정상화됨. 

### 정상화된 변수로 데이터 교체
# VIX, VKOSPI, net_point_by_word 제외하고 전부 차분
rf_diff_data <- rf_data1[c(-1, -2),]
rf_diff_data[, "BTC_price"] <- rf_data1[-1, "BTC_price"] %>% diff
rf_diff_data[, "KOSPI"] <- rf_data1[-1 , "KOSPI"] %>% diff
rf_diff_data[, "NASDAQ"] <- rf_data1[-1 , "NASDAQ"] %>% diff # VIX는 이미 정상시계열이므로 차분안함
rf_diff_data[, "S.P500"] <- rf_data1[-1 , "S.P500"] %>% diff
rf_diff_data[, "Nvidia"] <- rf_data1[-1 , "Nvidia"] %>% diff
rf_diff_data[, "AMD"] <- rf_data1[-1 , "AMD"] %>% diff
rf_diff_data[, "MA"] <- rf_data1[-1 , "MA"] %>% diff
rf_diff_data[, "PYPL"] <- rf_data1[-1 , "PYPL"] %>% diff
rf_diff_data[, "SQ"] <- rf_data1[-1 , "SQ"] %>% diff
rf_diff_data[, "vidente"] <- rf_data1[-1, "vidente"] %>% diff
rf_diff_data[, "volume"] <- rf_data1[c(-1,-2), "volume"]
rf_diff_data[, "Atinum"] <- rf_data1[-1, "Atinum"] %>% diff
rf_diff_data[, "Woori"] <- rf_data1[-1, "Woori"] %>% diff

rf_data <- rf_diff_data
rownames(rf_data) <- NULL # row index 초기화 
rf_data_price_index <- which(colnames(rf_data) == "BTC_price")



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
        set.seed(1234)
        rf_model <- randomForest(BTC_price~ KOSPI + VIX + NASDAQ + VKOSPI + S.P500 + Nvidia + AMD + MA + PYPL + SQ + 
                                   vidente + net_point_by_word +volume + Atinum + Woori, 
                                 data = rf_data[(nrow(rf_data)-60-k+(i-1)) : (nrow(rf_data)-60-1+(i-1)),],
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



fit_params <- window_rf_RMSE[which(window_rf_RMSE[,"RMSE"] == min(window_rf_RMSE[,"RMSE"])), ] # 최적의 mtry, ntree, window_size, RMSE 출력
fit_windows <- fit_params[["window_size"]]
fit_mtry <- fit_params[["mtry"]]
fit_ntree <- fit_params[["ntree"]]
fit_value <- NULL


rf_data_price_index <- which(colnames(rf_data) == "BTC_price")

for ( i in (1:61)) {
  train_data <- rf_data[(nrow(rf_data)-60-fit_windows+(i-1)) : (nrow(rf_data)-60-1+(i-1)), ]
  set.seed(1234)
  rf_model <- randomForest(BTC_price~  KOSPI + VIX + NASDAQ + VKOSPI + S.P500 + Nvidia + AMD + MA + PYPL + SQ + 
                             vidente + net_point_by_word +volume + Atinum + Woori, 
                           data = train_data, 
                           mtry = fit_mtry, ntree = fit_ntree)
  
  fit <- predict(rf_model, rf_data[(nrow(rf_data)-60-1+(i)),-rf_data_price_index ])
  fit_value <- c(fit_value, fit)
}

RMSE(rf_data[(nrow(rf_data)-60):nrow(rf_data),"BTC_price"], fit_value) 


##### 차분된 시계열 그리기


n1 <- nrow(rf_data) -60
n2 <- nrow(rf_data)

pred_date <- rf_data[n1:n2, "Date"]
true_value <- rf_data[n1:n2, "BTC_price"]
rf_data %>% head

fit_frame <- cbind(pred_date, true_value, fit_value)
fit_frame <- fit_frame %>% as.data.frame
fit_value %>% length

ggplot(data = fit_frame, aes(x = pred_date))+
  geom_line(aes(y = true_value))+
  geom_line(aes(y = fit_value, colour = 'orange')) +
  theme_classic()



##### 차분 풀어준 시계열 


rf_data1 # 차분 전 데이터프레임

inv_fit_value <- 1:61

for (i in 1:61 ) {
  inv_fit_value[i] <- fit_value[i] + rf_data1[nrow(rf_data1)-61+(i-1), "BTC_price"]
}


n1 <- nrow(rf_data1) -60
n2 <- nrow(rf_data1)


pred_date <- rf_data1[n1:n2, "Date"]
inv_true_value <- rf_data1[n1:n2, "BTC_price"]



fit_frame2 <- cbind(pred_date, inv_true_value, inv_fit_value)
fit_frame2 <- fit_frame2 %>% as.data.frame
fit_frame2 %>% head

fit_frame2[,"pred_date"] <- fit_frame2[, "pred_date"] %>% as.Date
fit_frame2


ggplot(data = fit_frame2, aes(x = pred_date)) +
  geom_line(aes( y = inv_true_value)) +
  geom_line(aes( y = inv_fit_value, colour = 'orange')) +
  theme_classic()


RMSE(fit_frame2[, "inv_true_value"], fit_frame2[, "inv_fit_value"])

#### importance plot

fit_window <- fit_params[['window_size']]
save_frame <- NULL

for ( i in 1:61) {
  train_data2 <- rf_data[(nrow(rf_data)-(i-1)-fit_window):nrow(rf_data)-(i-1), ]
  
  
  set.seed(1234)
  rf_best <- randomForest(BTC_price~ net_point_by_word + vidente + AMD + volume + SQ + Atinum +
                            Woori + KOSPI + VIX + PYPL + MA + Nvidia + NASDAQ + S.P500 + VKOSPI, 
                          data = train_data2,  # 롤링 윈도우 시작했던 날짜부터 잘라서 데이터로 넣음.
                          mtry = fit_params[["mtry"]], ntree = fit_params[["ntree"]])
  save_frame <- cbind(save_frame, rf_best$importance)
  
}


net_point <- t(save_frame)[, "net_point_by_word"] %>% mean
vidente <- t(save_frame)[, "vidente"] %>% mean
AMD <- t(save_frame)[, "AMD"] %>% mean
volume <- t(save_frame)[, "volume"] %>% mean
SQ <- t(save_frame)[, "SQ"] %>% mean
Atinum <- t(save_frame)[, "Atinum"] %>% mean
Woori <- t(save_frame)[, "Woori"] %>% mean
KOSPI <- t(save_frame)[, "KOSPI"] %>% mean
VIX <- t(save_frame)[, "VIX"] %>% mean
PYPL <- t(save_frame)[, "PYPL"] %>% mean
MA <- t(save_frame)[, "MA"] %>% mean
Nvidia <- t(save_frame)[, "Nvidia"] %>% mean
NASDAQ <- t(save_frame)[, "NASDAQ"] %>% mean
S.P500 <- t(save_frame)[, "S.P500"] %>% mean
VKOSPI <- t(save_frame)[, "VKOSPI"] %>% mean

impo_mean <- rbind(net_point, vidente, AMD, volume, SQ,
                   Atinum, Woori, KOSPI, VIX, PYPL,
                   MA, Nvidia, NASDAQ, S.P500, VKOSPI)
colnames(impo_mean) <- "IncNodePurity"
impo_mean <- impo_mean %>% as.data.frame

ggplot(data = impo_mean,
       aes(x = reorder(rownames(impo_mean), -IncNodePurity ),
           y = IncNodePurity)) +
  theme_classic()+
  geom_bar(stat = 'identity',
           colour = 'orange',
           fill = 'orange') +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14))+
  scale_fill_brewer(palette = "Oranges")+
  theme(legend.position = "none")+
  xlab("")


################## 위에가 변수 중요도 평균낸것. 아래는 연습장 ###########3

fit_window <- fit_params[['window_size']]
train_data2 <- rf_data[(nrow(rf_data)-50-fit_window):nrow(rf_data)-50, ]

set.seed(1234)
rf_best <- randomForest(BTC_price~ KOSPI + VIX + NASDAQ + VKOSPI + S.P500 + Nvidia + AMD + MA + PYPL + SQ + 
                           vidente + net_point_by_word +volume + Atinum + Woori, 
                         data = train_data2,  # 롤링 윈도우 시작했던 날짜부터 잘라서 데이터로 넣음.
                        mtry = fit_params[["mtry"]], ntree = fit_params[["ntree"]])

rf_impo <- cbind(rf_best$importance %>% as.data.frame,
                 rownames = rf_best$importance %>% rownames)


varImpPlot(rf_best, bg = 'orange')

rf_data1 %>% tail

## 위 importance plot은 마지막 sliding window로 한 plot임. 

## fit value 보내주기
rf_data1[,"BTC_price"] %>% tail
rf_data1[, "Date"] %>% tail
