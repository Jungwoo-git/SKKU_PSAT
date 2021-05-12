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


## [ data loading ] 
setwd("C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터")

raw_BTC_price <- "BTC_KRW coinmarketcap.csv" %>% fread   # 단위: 원
raw_exchange_rate <- "USD_KRW_exchange_rate.csv" %>% fread # 단위: 달러vs원
raw_gold_price <- "Gold_price.csv" %>% fread # 단위: 달러/oz
raw_USA_bond <- "USA_10year_bond_.csv" %>% fread # 단위: %
raw_kospi <- "KOSPI.csv" %>% fread 
raw_consumer_price <- "consumer_price_index.csv" %>% fread(head = FALSE) 
raw_bitcoin_circulation <- "bitcoin_circulation.csv" %>% fread # 단위: 달러
raw_VIX <- "변동성지수_VIX.csv" %>% fread
raw_nasdaq <- "nasdaq.csv" %>% fread # 단위 : 달러
raw_VKOSPI <- "VKOSPI.csv" %>% fread

raw_bitcoin_circulation %>% tail(20)

raw_SP500 <- "S&P500.csv" %>% fread(encoding = 'UTF-8')
raw_nvidia <- "nvidia_stock.csv" %>% fread(encoding = 'UTF-8')
raw_AMD_stock <- "AMD_stock.csv" %>% fread(encoding = 'UTF-8')

raw_AMD_stock

# MA, PYPL, SQ는 암호화폐로 결제할 수 있는 온라인 플랫폼 주가
raw_MA <- "MA.csv" %>% fread(encoding = "UTF-8")
raw_PYPL <- "PYPL.csv" %>% fread(encoding= "UTF-8")
raw_SQ <- "SQ.csv" %>% fread(encoding= "UTF-8")
raw_consumer_price

# vidente
raw_vidente <- "vidente.csv" %>% fread(encoding = "UTF-8")
raw_vidente


# Atinum
raw_atinum <- "Atinum.csv" %>% fread(encoding = "UTF-8")


# Woori
raw_woori <- "Woori.csv" %>% fread(encoding = "UTF-8")


## [ data 가공 ]
# 1. 날짜 데이터 형식 맞추기
raw_BTC_price[, "날짜"] <- raw_BTC_price[, "날짜"] %>% as.matrix %>% dmy
raw_BTC_price[, "날짜"]

raw_exchange_rate[, 'Date'] <- raw_exchange_rate[,'Date'] %>% as.matrix %>% dmy
raw_exchange_rate[, 'Date']

raw_gold_price[, "Date"] <- raw_gold_price[, "Date"] %>% as.matrix %>% mdy() 
raw_gold_price[, "Date"]

raw_USA_bond[, "날짜"] <- raw_USA_bond[, "날짜"] %>% as.matrix %>% ymd()
raw_USA_bond[,'날짜']

raw_kospi[, "날짜"] <- raw_kospi[, "날짜"] %>% as.matrix %>% ymd()
raw_kospi[, "날짜"]

consumer_price <- (raw_consumer_price[c(1,2),] %>% t() %>% as.data.frame)[-c(1,2), ]
names(consumer_price) <- c("연-월 날짜", "총_지수")
consumer_price[, '연-월 날짜'] %>% as.matrix %>% ym()
consumer_price[, '연-월 날짜'] <- consumer_price[, '연-월 날짜'] %>% as.matrix %>% ym()
consumer_price


raw_bitcoin_circulation[,'DateTime'] <- raw_bitcoin_circulation[, "DateTime"] %>% as.matrix %>% as.Date

raw_VIX[, "날짜"] <- raw_VIX[, "날짜"] %>% as.matrix %>% ymd() 

raw_nasdaq[, 'Date'] <- raw_nasdaq[, "Date"] %>% as.matrix %>% ymd()

raw_VKOSPI[, '일자'] <- raw_VKOSPI[, "일자"] %>% as.matrix %>% ymd

raw_SP500[, '날짜'] <- raw_SP500[, "날짜"] %>% as.matrix %>% ymd()
raw_nvidia[, '날짜'] <- raw_nvidia[, '날짜'] %>% as.matrix %>% ymd()
raw_AMD_stock[, '날짜'] <- raw_AMD_stock[, "날짜"] %>% as.matrix %>% ymd()


raw_MA[, "Date"] <- raw_MA[, 'Date'] %>% as.matrix %>% ymd
raw_PYPL[, "Date"] <- raw_PYPL[, "Date"] %>% as.matrix %>% ymd
raw_SQ[, "Date"] <- raw_SQ[, "Date"] %>% as.matrix %>% ymd

raw_vidente[, "날짜"] <- raw_vidente[, "날짜"] %>% as.matrix %>% ymd
raw_vidente[,'날짜'] <- raw_vidente[, '날짜'] %>% as.matrix %>% as.Date


raw_atinum[, "Date"] <- raw_atinum[, "Date"] %>% as.matrix %>% mdy


raw_woori[, "Date"] <- raw_woori[, "Date"] %>% as.matrix %>% mdy



## [ 필요한 데이터만 추출하기]
# 종속변수: 비트코인 일일 수익률
a <- gsub("\\\\",'', raw_BTC_price[, '종가**'] %>% as.matrix) # 종가 안의 \\ 삭제.
b <- gsub(",", "", a) # 종가 안의 ',' 삭제.
raw_BTC_price[, "종가**"] <- b

BTC_price <- raw_BTC_price[, c("날짜", "종가**")] 
BTC_price[, '종가**'] <- BTC_price[,'종가**'] %>% as.matrix %>% as.numeric
names(BTC_price) <- c("날짜", "BTC_가격")
BTC_price[, 'BTC_가격'] %>% str

# 변수 (1) USD 환율
exchange_rate <- raw_exchange_rate[, c("Date", "Price")]
names(exchange_rate) <- c("날짜", "환율")
exchange_rate[,"환율"] <- gsub(",", "",exchange_rate[, "환율"] %>% as.matrix)
exchange_rate[, "환율"] <- exchange_rate[, "환율"] %>% as.matrix %>% as.numeric
exchange_rate


# 변수 (2) gold 가격
gold_price <- raw_gold_price[, c('Date', 'Close/Last')]
names(gold_price) <- c("날짜", "금_가격")



# 변수 (3) 미국 채권 수익률
USA_bond <- raw_USA_bond[, c("날짜", "종가")]
names(USA_bond) <- c("날짜", "채권_수익률")



# 변수 (4) KOSPI 수익률
kospi <- raw_kospi[, c("날짜", "종가")]
kospi[,"종가"] <- gsub(",", "",kospi[, "종가"] %>% as.matrix)
kospi[, "종가"] <- kospi[, "종가"] %>% as.matrix %>% as.numeric
names(kospi) <- c("날짜", "KOSPI")



# 변수 (5) 소비자물가지수
names(consumer_price) <- c("연-월 날짜", "소비자물가지수")
consumer_price[,"소비자물가지수"] <- consumer_price[, "소비자물가지수"] %>% as.matrix %>% as.numeric
consumer_price


# 변수 (6) 비트코인 공급량 (단위: 1개)
bitcoin_circulation <- raw_bitcoin_circulation[, c("DateTime", "Total Supply")]
names(bitcoin_circulation) <- c("날짜", "BTC_공급량")


# 변수 (7) 변동성지수 VIX
VIX <- raw_VIX[, c("날짜", "종가")]
names(VIX) <- c("날짜", "VIX_value")
VIX %>% head(20)


# 변수 (8) NASDAQ
nasdaq <- raw_nasdaq[, c("Date", "Close")]
names(nasdaq) <- c("날짜", "Nasdaq_종가")
nasdaq %>% tail(20)


# 변수 (9) VKOSPI
VKOSPI <- raw_VKOSPI[, c("일자", "종가")]
names(VKOSPI) <- c("날짜", "VKOSPI_종가")
VKOSPI %>% head(20)


# 변수 (10) S&P500
SP500 <- raw_SP500[, c("날짜", "종가")]
SP500[, '종가'] <- gsub(",", "", SP500[, "종가"] %>% as.matrix)
names(SP500) <- c("날짜", "S&P500_종가")

# 변수 (11) Nvidia
nvidia <- raw_nvidia[, c("날짜", "종가")]
names(nvidia) <- c('날짜', 'nvidia_종가')


# 변수 (12) AMD
AMD <- raw_AMD_stock[, c("날짜", "종가")]
names(AMD) <- c("날짜", "AMD_종가")


# 변수 (13) MA
MA <- raw_MA[, c("Date", "Close")]
names(MA) <- c("날짜", "MA_종가")


# 변수 (14) PYPL
PYPL <- raw_PYPL[, c("Date", "Close")]
names(PYPL) <- c("날짜", "PYPL_종가")


# 변수 (15) SQ
SQ <- raw_SQ[, c("Date", "Close")]
names(SQ) <- c("날짜", "SQ_종가")
PYPL

# 변수 (16) vidente
vidente <- raw_vidente[, c("날짜", "종가")]
vidente[,"종가"] <- gsub(",", "",vidente[, "종가"] %>% as.matrix)
vidente[,"종가"] <- vidente[,"종가"] %>% as.matrix %>% as.numeric
names(vidente) <- c("날짜", "종가")
vidente

# 변수 17. Atinum
atinum <- raw_atinum[, c("Date", "Price")]
atinum[,"Price"] <- gsub(",", "", atinum[, "Price"]%>% as.matrix)
names(atinum) <- c("날짜", "atinum")


# 변수 18. woori
woori <- raw_woori[, c("Date", "Price")]
woori[, "Price"] <- gsub(",", "", woori[, "Price"] %>% as.matrix)
names(woori) <- c("날짜", "woori")



## [ 모든 변수 데이터 합치기. ]
# 종속변수;BTC_profit_rate: (13.04.29 ~ 21.04.13)
# exchange_rate: (11.01.03 ~ 21.04.01)
# gold_price: (11.04.14 ~ 21.04.13)
# USA_bond: (11.01.03 ~ 21.04.15)
# kospi: (11.01.03 ~ 21.04.14)
# consumer_price: (10.01.01 ~ 21.02.01) -> 월별 데이터.
# BTC circulation
# VIX
# NASDAQ
# VKOSPI


temperary_frame <- left_join(BTC_price, gold_price, by = "날짜") %>%
  left_join(USA_bond, by = "날짜") %>%
  left_join(kospi, by = "날짜") %>%
  left_join(exchange_rate, by = '날짜') %>%
  left_join(bitcoin_circulation, by = '날짜') %>%
  left_join(VIX, by = '날짜') %>% 
  left_join(nasdaq, by = "날짜") %>% 
  left_join(VKOSPI, by = "날짜") %>% 
  left_join(SP500, by = "날짜") %>%
  left_join(nvidia, by = "날짜") %>%
  left_join(AMD, by = "날짜") %>% 
  left_join(MA, by = '날짜') %>%
  left_join(PYPL, by = "날짜") %>%
  left_join(SQ, by = "날짜") %>%
  left_join(vidente, by = "날짜") %>% 
  left_join(atinum, by = "날짜") %>%
  left_join(woori, by = "날짜")
temperary_frame %>% head(20)

temperary_frame[-(1:which(temperary_frame[,"날짜"] == "2021-04-01")),] ## 2013년 4월 29일 ~ 2021년 3월 31일 데이터


temp <- as.matrix(temperary_frame)
SQ_min_date = raw_SQ[1,1]
which(temperary_frame == '2015-11-19') # SQ데이터 중 가장 오래된 날짜날 전날
temperary_frame[which(temperary_frame == '2015-11-19'):dim(temperary_frame)[1],"SQ_종가"] <- 0


PYPL_min_date = raw_PYPL[1,1]
which(temperary_frame == "2015-07-06") # PYPL 데이터 중 가장 오래된 날짜날 전날
temperary_frame[which(temperary_frame == '2015-07-06'):dim(temperary_frame)[1],"PYPL_종가"] <- 0 



vidente_min_date = raw_vidente[dim(raw_vidente)[1],1]
which(temperary_frame == "2014-03-16") # vidente 데이터 중 가장 오래된 날짜 전날
temperary_frame[which(temperary_frame == '2014-03-16'):dim(temperary_frame)[1],"종가"] <- 0 
vidente_min_date
temperary_frame

temperary_frame2 <- temperary_frame %>% mutate(temperary_frame[, "날짜"] %>% format("%Y-%m")) # 일별 데이터에 월별 데이터를 합치기 위해 작업.
names(temperary_frame2)[(temperary_frame2 %>% names %>% length)] <- "연-월 날짜"
temperary_frame2[, "연-월 날짜"] <- temperary_frame2[, "연-월 날짜"] %>% as.matrix %>% ym


temperary_frame3 <- left_join(temperary_frame2, consumer_price, by = "연-월 날짜") # 일별 데이터의 각 월마다 월별 데이터 값 inner_join.



which(temperary_frame3[, 1] == '2021-03-31')
temperary_frame4 <- temperary_frame3[14:dim(temperary_frame3)[1],]



final_frame<- temperary_frame4[, -"연-월 날짜"]
final_frame[, "S&P500_종가"] <- final_frame[, "S&P500_종가"] %>% as.matrix %>% as.numeric # S&P500 종가 numeric화
final_frame # 최종 data frame




final_frame[, 2] <- na.interp(final_frame[, 2])
final_frame[, 3] <- na.interp(final_frame[, 3])
final_frame[, 4] <- na.interp(final_frame[, 4])
final_frame[, 5] <- na.interp(final_frame[, 5])
final_frame[, 6] <- na.interp(final_frame[, 6])
final_frame[, 7] <- na.interp(final_frame[, 7])
final_frame[, 8] <- na.interp(final_frame[, 8])
final_frame[, 9] <- na.interp(final_frame[, 9])
final_frame[, 10] <- na.interp(final_frame[, 10])
final_frame[, 11] <- na.interp(final_frame[, 11])
final_frame[, 12] <- na.interp(final_frame[, 12])
final_frame[, 13] <- na.interp(final_frame[, 13])
final_frame[, 14] <- na.interp(final_frame[, 14])
final_frame[, 15] <- na.interp(final_frame[, 15])
final_frame[, 16] <- na.interp(final_frame[, 16])
final_frame[, 17] <- na.interp(final_frame[, 17])
final_frame[, 18] <- na.interp(final_frame[, 18])

names(final_frame) <- c("Date", "BTC_price", "Gold_price", "Bond_yield", "KOSPI", "USD_exchange_rate",
                        "BTC_circulation", "VIX", "NASDAQ", "VKOSPI", "S&P500", "Nvidia",
                        "AMD", "MA", "PYPL", "SQ", "vidente","consumer_index")

final_frame

raw_BTC_price %>% head(20)
final_frame %>% names
file = file('Economic_frame.csv', encoding= "UTF-8")
write.csv(final_frame, file = file)

## [ EDA ]
# 1. 모든 설명변수와 종속변수의 그래프를 한 공간에 그리기 - 추세 확인.

USD_plot <- ggplot(data = final_frame,
                   aes(x = 날짜)) +
  geom_line(aes(y = 환율), color = "lightblue") +
  geom_line(aes(y = BTC_가격/6000), color = 'orange') +
  scale_y_continuous(
    name = "USD axis",
    sec.axis = sec_axis(~.*6000 , name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs USD/KRW 환율",
       x = "날짜")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
USD_plot


Gold_plot <- ggplot(data = final_frame,
                    aes(x = 날짜)) +
  geom_line(aes(y = 금_가격), color = "#CC3399") +
  geom_line(aes(y = BTC_가격/6000), color = 'orange') +
  scale_y_continuous(
    name = "Gold axis",
    sec.axis = sec_axis(~. *6000, name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs 금 가격",
       x = "날짜")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
Gold_plot



Bond_plot <- ggplot(data = final_frame,
                    aes(x = 날짜)) +
  geom_line(aes(y = 채권_수익률), color = "green") +
  geom_line(aes(y = BTC_가격/4000000), color = 'orange') +
  scale_y_continuous(
    name = "Bond axis",
    sec.axis = sec_axis(~. *4000000, name="BTC  axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs 미국 10년 채권 수익률",
       x = "날짜") +
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
Bond_plot




Kospi_plot <- ggplot(data = final_frame,
                     aes(x = 날짜)) +
  geom_line(aes(y = KOSPI), color = "pink") +
  geom_line(aes(y = BTC_가격/4000), color = 'orange') +
  scale_y_continuous(
    name = "KOSPI axis",
    sec.axis = sec_axis(~.*4000, name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs KOSPI",
       x = "날짜") +
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
Kospi_plot



Price_plot <- ggplot(data = final_frame,
                     aes(x = 날짜)) +
  geom_line(aes(y = 소비자물가지수), 
            color = "black",
            size = 1.3) +
  geom_line(aes(y = BTC_가격/80000), 
            color = 'orange') +
  scale_y_continuous(
    name = "Price index axis",
    sec.axis = sec_axis(~. *80000, name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs 소비자물가지수",
       x = "날짜") +
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
Price_plot



circulation_plot <- ggplot(data = final_frame,
                     aes(x = 날짜)) +
  geom_line(aes(y = BTC_공급량), 
            color = "gray") +
  geom_line(aes(y = BTC_가격), 
            color = 'orange') +
  scale_y_continuous(
    name = "BTC circulation axis",
    sec.axis = sec_axis(~., name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs BTC 공급량",
       x = "날짜") +
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
circulation_plot


VIX_plot <- ggplot(data = final_frame,
                           aes(x = 날짜)) +
  geom_line(aes(y = VIX_value), 
            color = "gray") +
  geom_line(aes(y = BTC_가격/500000), 
            color = 'orange') +
  scale_y_continuous(
    name = "VIX axis",
    sec.axis = sec_axis(~.*500000, name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs VIX 값",
       x = "날짜") +
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
VIX_plot


Nasdaq_plot <- ggplot(data = final_frame,
                   aes(x = 날짜)) +
  geom_line(aes(y = Nasdaq_종가), 
            color = "blue") +
  geom_line(aes(y = BTC_가격/2000), 
            color = 'orange') +
  scale_y_continuous(
    name = "NASDAQ axis",
    sec.axis = sec_axis(~.*2000, name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs NASDAQ",
       x = "날짜") +
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
Nasdaq_plot



Vkospi_plot <- ggplot(data = final_frame,
                      aes(x = 날짜)) +
  geom_line(aes(y = VKOSPI_종가), 
            color = "red") +
  geom_line(aes(y = BTC_가격/800000), 
            color = 'orange') +
  scale_y_continuous(
    name = "VKOSPI axis",
    sec.axis = sec_axis(~.*800000, name="BTC axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 가격 vs VKOSPI",
       x = "날짜") +
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
Vkospi_plot



grid.arrange(USD_plot, Gold_plot, Bond_plot, 
             Kospi_plot, Price_plot, circulation_plot, 
             VIX_plot, Nasdaq_plot, Vkospi_plot, nrow= 3)

grid.arrange(USD_plot, Gold_plot, nrow= 1)

grid.arrange( Bond_plot, Kospi_plot,  nrow= 1)

grid.arrange(Price_plot, circulation_plot, nrow= 1)

grid.arrange(VIX_plot, Nasdaq_plot, nrow= 3)




## 2. 설명변수간 상관관계 확인 --> 상관관계가 크다면 다중회귀분석을 사용하기 곤란하다. 만약 작다면 사용해도 무방하므로 사용해보겠다는 flow
# 피어슨 상관분석 --> 설명변수 간 선형상관관계도 작지만, 설명변수와 종속변수 간 선형상관관계도 너무 작음. 이거 한 후에 비선형 상관관계도 고려해보자. 
data_cor <- cor(final_frame[,-1])
data_cor[,-1]
corrplot(data_cor, method = 'number', type = "upper")

# 스피어만 상관분석 --> 설명변수와 종속변수 간 비선형상관관계도 너무 작음. 설명변수 추가해야하나.
spearman_cor <- cor(final_frame[, -1], method = 'spearman')
corrplot(spearman_cor, method = 'number', type = 'upper')

# 상관계수 검정. 영가설: 상관계수가 없다.
cor_pairwise_test <- rcorr(data_cor, 
                           type = 'spearman')
cor_pairwise_test[[3]]
corrplot(cor_pairwise_test[[3]], method = 'number', type = 'upper')


