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
library(RColorBrewer)


## [ data loading ] 
setwd("C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터")
rf_data1 <- "factor_data.csv" %>% fread(data.table = F,encoding = "UTF-8") # 데이터: 2018-01-01~ 2021-03-31
rf_data1 %>% tail

window_size <- c(50, 100, 150, 200, 250, 300, 350, 400 )
mtry = c(2, 3, 4,5)
ntree = c(5, 10, 15, 30, 50, 70)
window_rf_RMSE <- expand.grid(mtry = mtry,
                              ntree = ntree,
                              window_size = window_size,
                              RMSE = NA)


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
rownames(rf_data) <- NULL

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

f_params <- window_rf_RMSE[which(window_rf_RMSE[,"RMSE"] == min(window_rf_RMSE[,"RMSE"])), ] # 최적의 mtry, ntree, window_size, RMSE 출력
f_windows <- f_params[["window_size"]]
f_mtry <- f_params[["mtry"]]
f_ntree <- f_params[["ntree"]]
fit_value <- NULL
f_params

#####

rf_data_price_index <- which(colnames(rf_data) == "BTC_price")

for ( i in (1:61)) {
  train_data <- rf_data[(nrow(rf_data)-60-f_windows+(i-1)) : (nrow(rf_data)-60-1+(i-1)), ]
  set.seed(1234)
  rf_model <- randomForest(BTC_price~ volume + sentiment + F1 + F2 + F3 + F4,
                           data = train_data, 
                           mtry = f_mtry, ntree = f_ntree)
  fit <- predict(rf_model, rf_data[(nrow(rf_data)-60-1+(i)), -rf_data_price_index ])
  
  fit_value <- c(fit_value, fit)
}




##### 차분된 시계열 그리기

n1 <- nrow(rf_data) -60
n2 <- nrow(rf_data)


pred_date <- rf_data[n1:n2, "Date"]
true_value <- rf_data[n1:n2, "BTC_price"]

fit_frame <- cbind(pred_date, true_value, fit_value)
fit_frame <- fit_frame %>% as.data.frame
fit_frame[, "pred_date"] <- fit_frame[, "pred_date"] %>% as.Date


ggplot(data = fit_frame, aes(x = pred_date))+
  geom_line(aes(y = true_value))+
  geom_line(aes(y = fit_value, colour = 'red')) +
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
inv_true_value


fit_frame2 <- cbind(pred_date, inv_true_value, inv_fit_value)
fit_frame2 <- fit_frame2 %>% as.data.frame
fit_frame2 %>% head

fit_frame2[,"pred_date"] <- fit_frame2[, "pred_date"] %>% as.Date

ggplot(data = fit_frame2, aes(x = pred_date)) +
  geom_line(aes( y = inv_true_value)) +
  geom_line(aes( y = inv_fit_value, colour = 'red')) +
  theme_classic()

RMSE(fit_frame2[, "inv_true_value"], fit_frame2[, "inv_fit_value"])
fit_frame2 %>% head
f_params


#### importance plot
fit_window <- f_params[['window_size']]
save_frame <- NULL

for ( i in 1:61) {
  train_data2 <- rf_data[(nrow(rf_data)-(i-1)-fit_window):nrow(rf_data)-(i-1), ]
  
  
  set.seed(1234)
  rf_best <- randomForest(BTC_price~ volume + sentiment + F1 + F2 + F3 + F4, 
                          data = train_data2,  # 롤링 윈도우 시작했던 날짜부터 잘라서 데이터로 넣음.
                          mtry = f_params[["mtry"]], ntree = f_params[["ntree"]])
  save_frame <- cbind(save_frame, rf_best$importance)
  
}
sentiment_mean <- t(save_frame)[, "sentiment"] %>% mean
volume_mean <- t(save_frame)[, 'volume'] %>% mean
F1_mean <- t(save_frame)[, 'F1'] %>% mean
F2_mean <- t(save_frame)[, 'F2'] %>% mean
F3_mean <- t(save_frame)[, 'F3'] %>% mean
F4_mean <- t(save_frame)[, 'F4'] %>% mean

impo_mean <- rbind(sentiment_mean, volume_mean, F1_mean, F2_mean, F3_mean, F4_mean)
colnames(impo_mean) <- "IncNodePurity"
impo_mean <- impo_mean %>% as.data.frame

colourCount = length(rownames(impo_mean))
getPalette = colorRampPalette(brewer.pal(9, "YlOrBr"))

ggplot(data = impo_mean,
       aes(x = reorder(rownames(impo_mean), -IncNodePurity ),
           y = IncNodePurity,
           fill = reorder(rownames(impo_mean), IncNodePurity ))) +
  theme_classic()+
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 14))+
  scale_fill_manual(values = getPalette(colourCount))+
  theme(legend.position = "none")+
  xlab("")


  
################################### 위에가 중요도 평균 낸 것. 아래는 연습용 코드  ############### 
train_data3 <- rf_data[(nrow(rf_data)-30-fit_window):nrow(rf_data)-30, ]
rownames(train_data3) <- NULL
train_data3 %>% head


set.seed(1234)
rf_best <- randomForest(BTC_price~ volume + sentiment + F1 + F2 + F3 + F4, 
                        data = train_data3,  # 롤링 윈도우 시작했던 날짜부터 잘라서 데이터로 넣음.
                        mtry = f_params[["mtry"]], ntree = f_params[["ntree"]])
rf_impo <- cbind(rf_best$importance %>% as.data.frame,
                 rownames = rf_best$importance %>% rownames)
rf_best$importance

varImpPlot(rf_best, bg = 'orange',
           cex =1.5)
