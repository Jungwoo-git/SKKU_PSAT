setwd('C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터')
library(tidyverse)
library(forecast)
library(MLmetrics)
library(data.table)
####################
# SM, YG, JYP 분류 #
####################
test_yg <- fread('test_yg.csv', header = T, stringsAsFactors = F, data.table = F, encoding = 'UTF-8')
train_yg <- fread('train_yg.csv', header = T, stringsAsFactors = F, data.table = F, encoding = 'UTF-8')
data_yg <- rbind(train_yg[2162:2251,],test_yg)
data_yg<-data_yg %>% select(-date)

test_sm <- fread('test_sm.csv', header = T, stringsAsFactors = F, data.table = F, encoding = 'UTF-8')
train_sm <- fread('train_sm.csv', header = T, stringsAsFactors = F, data.table = F, encoding = 'UTF-8')
data_sm <- rbind(train_sm[2162:2251,],test_sm)
data_sm<-data_sm %>% select(-date)

test_jyp <- fread('test_jyp.csv', header = T, stringsAsFactors = F, data.table = F, encoding = 'UTF-8')
train_jyp <- fread('train_jyp.csv', header = T, stringsAsFactors = F, data.table = F, encoding = 'UTF-8')
data_jyp <- rbind(train_jyp[2162:2251,],test_jyp)
data_jyp <- data_jyp %>% select(-date)


## rolling window 활용한train-validation index ##
##window size=90
##100일 초과일 경우 arima 에서 ar이 0나오는 경우가 많았음.
##이 경우, 미래의 변동이 과거 시점 데이터에 영향을 받지 않는다는 뜻이므로 
##좋은 시계열 모델이라고 보기 어려움.
#또한, test 기간인 3-4월은 코로나 있었던 시기, 코로나바이러스 출현 이후로 train 을 설정하기 위해
#train을 2020년 3월 1일로부터 90일 전인 12월 초부터 잡음
x=1:90

FOLDS <-  list()
for(i in 1:61){
  FOLDS[[i]]=x+(i-1)
}

FOLDS_TEST=list()
for (i in 1:61){
  FOLDS_TEST[[i]]=FOLDS[[i]][90]+1
}

FOLDS_TEST[[61]]

#1.YG entertainment

yg_y<-ts(data_yg$YG_Price,frequency=365.25)
yg_x<-data_yg[,-1] %>% mutate_if(is.character,as.numeric) %>% as.matrix()
plot(yg_y, type='l')


#(1)with only macroscoping variables

yg_x_test=list()
yg_y_test=list()
for (i in (1:length(FOLDS_TEST))){
  yg_y_test[[i]]<- ts(yg_y[FOLDS_TEST[[i]]],frequency = 365.25)
  yg_x_test[[i]] <- yg_x[FOLDS_TEST[[i]],] %>% as.matrix()
}
yg_yhat_1=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(yg_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- yg_x[FOLDS[[i]],] %>% as.matrix()
  yg_arima <- auto.arima(y_train,xreg=xreg_train[,1:9] ,approximation=FALSE)
  yhat_1 <- forecast::forecast(yg_arima,xreg=t(yg_x_test[[i]][1:9]))$mean
  yg_yhat_1=c(yg_yhat_1,yhat_1)
}

yg_test_final_y=unlist(yg_y_test)
yg_test_final_y %>% head()
length(yg_test_final_y)
length(yg_yhat_1)
yg_rmse1=RMSE(yg_yhat_1, yg_test_final_y);yg_rmse1

plot(yg_test_final_y,type = 'l',col="red")
lines(yg_yhat_1)

#(2) macroscoping variables+emotional

yg_x_test2=list()
yg_y_test2=list()
for (i in (1:length(FOLDS_TEST))){
  yg_y_test2[[i]]<- ts(yg_y[FOLDS_TEST[[i]]],frequency = 365.25)
  yg_x_test2[[i]] <- yg_x[FOLDS_TEST[[i]],] %>% as.matrix()
}
yg_yhat_2=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(yg_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- yg_x[FOLDS[[i]],] %>% as.matrix()
  yg_arima <- auto.arima(y_train,xreg=xreg_train[,1:10] ,approximation=FALSE)
  yhat_1 <- forecast::forecast(yg_arima,xreg=t(yg_x_test2[[i]][1:10]))$mean
  yg_yhat_2=c(yg_yhat_2,yhat_1)
}

yg_test_final_y2=unlist(yg_y_test2)
yg_test_final_y2 %>% head()
length(yg_test_final_y2)
length(yg_yhat_2)
yg_rmse2=RMSE(yg_yhat_2, yg_test_final_y2);yg_rmse2


lines(yg_yhat_2,col="blue")

#(3) macroscoping variables+emotional+singer
##We have to remove sparse features&solve rank deficient problem
yg_yhat_3=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(yg_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- yg_x[FOLDS[[i]],] %>% as.matrix()
  yg_arima <- auto.arima(y_train,xreg=xreg_train,approximation=FALSE)
  yhat_1 <- forecast::forecast(yg_arima,xreg=t(yg_x_test2[[i]]))$mean
  yg_yhat_3=c(yg_yhat_3,yhat_1)
}

length(yg_yhat_3)
yg_rmse3=RMSE(yg_yhat_3, yg_test_final_y)
lines(yg_yhat_3,col="green")



#2.SM entertainment

sm_y<-ts(data_sm$SM_Price,frequency=365.25)
sm_x<-data_sm[,-1] %>% mutate_if(is.character,as.numeric) %>% as.matrix()
plot(sm_y, type='l')


#(1)with only macroscoping variables

sm_x_test=list()
sm_y_test=list()
for (i in (1:length(FOLDS_TEST))){
  sm_y_test[[i]]<- ts(sm_y[FOLDS_TEST[[i]]],frequency = 365.25)
  sm_x_test[[i]] <- sm_x[FOLDS_TEST[[i]],] %>% as.matrix()
}
sm_yhat_1=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(sm_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- sm_x[FOLDS[[i]],] %>% as.matrix()
  sm_arima <- auto.arima(y_train,xreg=xreg_train[,1:9] ,approximation=FALSE)
  yhat_1 <- forecast::forecast(sm_arima,xreg=t(sm_x_test[[i]][1:9]))$mean
  sm_yhat_1=c(sm_yhat_1,yhat_1)
}

sm_test_final_y=unlist(sm_y_test)
length(sm_test_final_y)
length(sm_yhat_1)
sm_rmse1=RMSE(sm_yhat_1, sm_test_final_y);sm_rmse1

plot(sm_test_final_y,type = 'l',col="red")
lines(sm_yhat_1)

#(2) macroscoping variables+emotional


sm_yhat_2=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(sm_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- sm_x[FOLDS[[i]],] %>% as.matrix()
  sm_arima <- auto.arima(y_train,xreg=xreg_train[,1:10] ,approximation=FALSE)
  yhat_1 <- forecast::forecast(sm_arima,xreg=t(sm_x_test[[i]][1:10]))$mean
  sm_yhat_2=c(sm_yhat_2,yhat_1)
}


length(sm_test_final_y)
length(sm_yhat_2)
sm_rmse2=RMSE(sm_yhat_2, sm_test_final_y);sm_rmse2


lines(sm_yhat_2,col="blue")

#(3) macroscoping variables+emotional+singer
##We have to remove sparse features&solve rank deficient problem
sm_yhat_3=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(sm_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- sm_x[FOLDS[[i]],] %>% as.matrix()
  sm_arima <- auto.arima(y_train,xreg=xreg_train,approximation=FALSE)
  yhat_1 <- forecast::forecast(sm_arima,xreg=t(sm_x_test[[i]]))$mean
  sm_yhat_3=c(sm_yhat_3,yhat_1)
}

length(sm_yhat_3)
sm_rmse3=RMSE(sm_yhat_3, sm_test_final_y)
lines(sm_yhat_3,col="green")



#3. JYP Entertainment

jyp_y<-ts(data_jyp$JYP_Price,frequency=365.25)
jyp_x<-data_jyp[,-1] %>% mutate_if(is.character,as.numeric) %>% as.matrix()
plot(jyp_y, type='l')


#(1)with only macroscoping variables

jyp_x_test=list()
jyp_y_test=list()
for (i in (1:length(FOLDS_TEST))){
  jyp_y_test[[i]]<- ts(jyp_y[FOLDS_TEST[[i]]],frequency = 365.25)
  jyp_x_test[[i]] <- jyp_x[FOLDS_TEST[[i]],] %>% as.matrix()
}
jyp_yhat_1=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(jyp_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- jyp_x[FOLDS[[i]],] %>% as.matrix()
  jyp_arima <- auto.arima(y_train,xreg=xreg_train[,1:9] ,approximation=FALSE)
  yhat_1 <- forecast::forecast(jyp_arima,xreg=t(jyp_x_test[[i]][1:9]))$mean
  jyp_yhat_1=c(jyp_yhat_1,yhat_1)
}

jyp_test_final_y=unlist(jyp_y_test)
length(jyp_test_final_y)
length(jyp_yhat_1)
jyp_rmse1=RMSE(jyp_yhat_1, jyp_test_final_y);jyp_rmse1

plot(ts(jyp_test_final_y,frequency = 365.25),type = 'l',col="red")
lines(ts(jyp_yhat_1,frequency = 365.25))

#(2) macroscoping variables+emotional

jyp_yhat_2=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(jyp_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- jyp_x[FOLDS[[i]],] %>% as.matrix()
  jyp_arima <- auto.arima(y_train,xreg=xreg_train[,1:10] ,approximation=FALSE)
  yhat_1 <- forecast::forecast(jyp_arima,xreg=t(sm_x_test[[i]][1:10]))$mean
  jyp_yhat_2=c(jyp_yhat_2,yhat_1)
}


length(jyp_test_final_y)
length(jyp_yhat_2)
jyp_rmse2=RMSE(jyp_yhat_2, jyp_test_final_y);jyp_rmse2


lines(ts(jyp_yhat_2,frequency = 365.25),col="blue")

#(3) macroscoping variables+emotional+singer
##We have to remove sparse features&solve rank deficient problem
jyp_yhat_3=c()
for (i in (1:length(FOLDS))){
  print(paste0(i,"th"))
  y_train<-ts(jyp_y[FOLDS[[i]]],frequency = 365.25)
  xreg_train <- jyp_x[FOLDS[[i]],] %>% as.matrix()
  jyp_arima <- auto.arima(y_train,xreg=xreg_train,approximation=FALSE)
  yhat_1 <- forecast::forecast(jyp_arima,xreg=t(jyp_x_test[[i]]))$mean
  jyp_yhat_3=c(jyp_yhat_3,yhat_1)
}

length(jyp_yhat_3)
jyp_rmse3=RMSE(jyp_yhat_3, jyp_test_final_y)
lines(jyp_yhat_3,col="green")


##RMSE

yg_rmse=c(yg_rmse1,yg_rmse2,yg_rmse3)
sm_rmse=c(sm_rmse1,sm_rmse2,sm_rmse3)
jyp_rmse=c(jyp_rmse1,jyp_rmse2,jyp_rmse3)
rmse=list(yg_rmse,sm_rmse,jyp_rmse)
names(rmse)<-c('yg_rmse','sm_rmse','jyp_rmse')
rmse
