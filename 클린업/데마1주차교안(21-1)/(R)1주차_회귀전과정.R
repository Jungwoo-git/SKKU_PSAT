#우선, 경로부터 지정해주고
getwd()
setwd("C:/Users/samsung/Documents/DATAMINING_PSAT")

#관련 패키지 호출
library(data.table)
library(tidyverse)

#데이터 불러오기
data=fread('kc_house_data.csv')
data %>% dim
data %>% glimpse

#결측치 제거해주고, 필요없는 칼럼 drop 취해주자
data=data[complete.cases(data),]
data=subset(data,select=-c(id,date))
data %>% dim #19개 컬럼으로 줄었음을 확인

#모델링에 앞서, train-test split 해주기
library(caret)
#매 시도시마다 일정한 값 산출하도록 seed 고정
set.seed(2021)
#createDataPartition() 이용. train: val = 8:2
train_idx=createDataPartition(data$price, p=.7, times=1, list=FALSE)
train=data[train_idx,]
test=data[-train_idx,]

train_x=train %>% select(-price) %>% as.matrix()
train_y=train %>% select(price) %>% unlist %>% as.vector
test_x=test %>% select(-price) %>% as.matrix()
test_y=test %>% select(price) %>% unlist %>% as.vector

#1. 모든 컬럼을 이용해 선형 회귀식 적합, RMSE 확인해보기
all_lm = lm(price ~ ., data=train)
all_lm %>% summary
#F-test는 통과했고, sqft_basement, sqft_lot 변수 제외 t-test도 유의하다는 결론
#(1 not defined because of singularities ~ 지우자 걍)
train=subset(train,select=-c(sqft_basement,sqft_lot))
train_x=subset(train_x,select=-c(sqft_basement,sqft_lot))
test_x=subset(test_x,select=-c(sqft_basement,sqft_lot))

#이 상태에서 다시 적합하여 test data에 대한 RMSE 구하기
all_new_lm = lm(price ~ ., data=train)
all_new_lm %>% summary

#validation data에 대해 예측
#관련 library 먼저 불러오고
library(MLmetrics)
pred_all_lm=predict(all_new_lm, test)
all_lm_rmse=RMSE(pred_all_lm, test_y)
all_lm_rmse

#2. Backward selection 방법으로 변수 선택, RMSE 확인해보기
#관련 library 호출!
library(leaps)
set.seed(2021)
#cross validation 5번 해보자
train.control = trainControl(method='cv',number=5)
#parameter로서 backward selection 정의. 변수를 5~10개 중 선택
backward.model<-train(price ~., data=train,
                     method='leapBackward',
                     tuneGrid=data.frame(nvmax=5:10),
                     trControl=train.control
                     )
backward.model$results 
backward.model$bestTune #변수를 10개로 설정했을 때 RMSE가 가장 낮더라!
all_lm_rmse #오 적은 변수로 해보니 기존보다 대폭 줄었더라!

#스스로 복습해보기~ MLmetrics 라이브러리의 다른 performance metric 시도해보기!
#https://www.rdocumentation.org/packages/MLmetrics/versions/1.1.1