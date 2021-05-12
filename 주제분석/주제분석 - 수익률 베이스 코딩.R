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

## [ data loading ] 
setwd("C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터")

raw_BTC_price <- "BTC_KRW coinmarketcap.csv" %>% fread
raw_exchange_rate <- "USD_KRW_exchange_rate.csv" %>% fread
raw_gold_price <- "Gold_price.csv" %>% fread
raw_USA_bond <- "USA_10year_bond_.csv" %>% fread
raw_kospi <- "KOSPI.csv" %>% fread
raw_consumer_price <- "consumer_price_index.csv" %>% fread(head = FALSE) 


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


## [ 필요한 데이터만 추출하기 - scale-free를 위해 가격이 아닌 '변화율' 선택.]
# 종속변수: 비트코인 일일 수익률
a <- gsub("\\\\",'', raw_BTC_price[, '종가**'] %>% as.matrix) # 종ㄱ 안의 \\ 삭제.
b <- gsub(",", "", a) # 종가 안의 ',' 삭제.
raw_BTC_price[, "종가**"] <- b

BTC_price <- raw_BTC_price[, c("날짜", "종가**")] 
BTC_price[, '종가**'] <- BTC_price[,'종가**'] %>% as.matrix %>% as.numeric

store_vector <- rep(NA, dim(BTC_price)[1])
for (i in 1:dim(BTC_price)[1]) {
  store_vector[i] <- ((BTC_price[i, "종가**" ] - BTC_price[i+1, "종가**"])/BTC_price[i+1, "종가**"] )
} # 일일 수익률 계산

BTC_merged_data <- BTC_price %>% mutate('BTC_일일_수익률_Percent' = as.matrix(store_vector))
BTC_profit_rate <- BTC_merged_data[, c("날짜", "BTC_일일_수익률_Percent")] # BTC 날짜와 일일 수익률 데이터
BTC_profit_rate[,"BTC_일일_수익률_Percent"] <- gsub("%", "",BTC_profit_rate[, "BTC_일일_수익률_Percent"] %>% as.matrix)
BTC_profit_rate[,"BTC_일일_수익률_Percent"] <- BTC_profit_rate[, "BTC_일일_수익률_Percent"] %>% as.matrix %>% as.numeric
BTC_profit_rate[,2] <- BTC_profit_rate[,2] *100
BTC_profit_rate

# 변수 (1) USD 환율
exchange_rate <- raw_exchange_rate[, c("Date", "Change %")]
names(exchange_rate) <- c("날짜", "환율_변화율_Percent")
exchange_rate[,"환율_변화율_Percent"] <- gsub("%", "", exchange_rate[, "환율_변화율_Percent"] %>% as.matrix)
exchange_rate[,"환율_변화율_Percent"] <- exchange_rate[, "환율_변화율_Percent"] %>% as.matrix %>% as.numeric
exchange_rate



# 변수 (2) gold 가격 변화율
store_vector <- rep(NA, dim(raw_gold_price)[1])
for (i in 1:dim(raw_gold_price)[1]) {
  store_vector[i] <- ((raw_gold_price[i,"Close/Last" ] - raw_gold_price[i+1, "Close/Last"])/raw_gold_price[i+1, "Close/Last"] )
} # gold 가격 변화율 계산

raw_gold_data <- raw_gold_price %>% mutate(change_rate = as.matrix(100*(store_vector %>% unlist))) # gold 가격 변화율을 기존의 데이터프레임에 추가하기. %이므로 100을 곱함.
gold_price_rate <- raw_gold_data[, c("Date", "change_rate")] # 날짜변수와 gold 가격 변화율을 추출하기 --> 변수 (2) gold 가격 변화율
names(gold_price_rate) <- c("날짜", "금_가격_변화율_Percent")
gold_price_rate


# 변수 (3) 미국 채권 수익률
USA_bond <- raw_USA_bond[, c("날짜", "변동 %")]
names(USA_bond) <- c("날짜", "미국_채권_수익률_Percent")
USA_bond[,"미국_채권_수익률_Percent"] <- gsub("%", "", USA_bond[, "미국_채권_수익률_Percent"] %>% as.matrix)
USA_bond[,"미국_채권_수익률_Percent"] <- USA_bond[, "미국_채권_수익률_Percent"] %>% as.matrix %>% as.numeric
USA_bond


# 변수 (4) KOSPI 수익률
kospi <- raw_kospi[, c("날짜", "변동 %")]
names(kospi) <- c("날짜", "KOSPI_수익률_Percent")
kospi[,"KOSPI_수익률_Percent"] <- gsub("%", "", kospi[, "KOSPI_수익률_Percent"] %>% as.matrix)
kospi[,"KOSPI_수익률_Percent"] <- kospi[, "KOSPI_수익률_Percent"] %>% as.matrix %>% as.numeric
kospi


# 변수 (5) 소비자물가지수
names(consumer_price) <- c("연-월 날짜", "소비자물가지수")
consumer_price[,"소비자물가지수"] <- consumer_price[, "소비자물가지수"] %>% as.matrix %>% as.numeric
consumer_price %>% str

## [ 모든 변수 데이터 합치기. ]
# 종속변수;BTC_profit_rate: (13.04.29 ~ 21.04.13)
# exchange_rate: (11.01.03 ~ 21.04.01)
# gold_price_rate: (11.04.14 ~ 21.04.13)
# USA_bond: (11.01.03 ~ 21.04.15)
# kospi: (11.01.03 ~ 21.04.14)
# consumer_price: (10.01.01 ~ 21.02.01) -> 월별 데이터.

temperary_frame <- inner_join(exchange_rate, gold_price_rate, by = "날짜") %>%
  inner_join(USA_bond) %>%
  inner_join(kospi) %>%
  inner_join(BTC_profit_rate)  # 일별 데이터는 모두 합쳤음.
temperary_frame

temperary_frame2 <- temperary_frame %>% mutate(temperary_frame[, "날짜"] %>% format("%Y-%m")) # 일별 데이터에 월별 데이터를 합치기 위해 작업.
names(temperary_frame2)[7] <- "연-월 날짜"
temperary_frame2[, "연-월 날짜"] <- temperary_frame2[, "연-월 날짜"] %>% as.matrix %>% ym

temperary_frame3 <- inner_join(temperary_frame2, consumer_price, by = "연-월 날짜") # 일별 데이터의 각 월마다 월별 데이터 값 inner_join.

temperary_frame4 <- temperary_frame3[, -"연-월 날짜"]

final_frame <- temperary_frame4[-dim(temperary_frame4)[1], ] # BTC 일일 수익률을 변수로 쓰므로, 마지막 데이터는 일일 수익률을 구할 수 없음.
# 그래서 NA로 남게 되므로, 그 row 삭제. 
final_frame # 최종 data frame


## [ EDA ]
# 1. 모든 설명변수와 종속변수의 그래프를 한 공간에 그리기 - 추세 확인.


BTC_plot <- ggplot(data = final_frame,
                   aes(x = 날짜)) +
  geom_line(aes(y = BTC_일일_수익률_Percent), color = 'orange')+
  theme_classic() +
  labs(title = "BTC 일일 수익률(%)",
       x = "날짜",
       y = "Percent")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=20, face='bold'))
BTC_plot



USD_plot <- ggplot(data = final_frame,
                   aes(x = 날짜)) +
  geom_line(aes(y = 환율_변화율_Percent), color = "lightblue") +
  geom_line(aes(y = BTC_일일_수익률_Percent*10), color = 'orange') +
  scale_y_continuous(
    name = "USD Percent axis",
    sec.axis = sec_axis(~. /10, name="BTC Percent axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 일일 수익률(%) vs USD 환율 변화율(%)",
       x = "날짜",
       y = "Percent")+
  theme(plot.title = element_text(hjust = 0.5))



USD_plot <- ggplot(data = final_frame,
                     aes(x = 날짜)) +
  geom_line(aes(y = 환율_변화율_Percent), color = "lightblue") +
  geom_line(aes(y = BTC_일일_수익률_Percent*10), color = 'orange') +
  scale_y_continuous(
    name = "USD Percent axis",
    sec.axis = sec_axis(~. /10, name="BTC Percent axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 일일 수익률(%) vs USD 환율 변화율(%)",
       x = "날짜",
       y = "Percent")+
  theme(plot.title = element_text(hjust = 0.5))

Gold_plot <- ggplot(data = final_frame,
                   aes(x = 날짜)) +
  geom_line(aes(y = 금_가격_변화율_Percent), color = "#CC3399") +
  geom_line(aes(y = BTC_일일_수익률_Percent*10), color = 'orange') +
  scale_y_continuous(
    name = "Gold Percent axis",
    sec.axis = sec_axis(~. /10, name="BTC Percent axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 일일 수익률(%) vs 금 가격 변화율(%)",
       x = "날짜",
       y = "Percent")+
  theme(plot.title = element_text(hjust = 0.5))

Bond_plot <- ggplot(data = final_frame,
                    aes(x = 날짜)) +
  geom_line(aes(y = 미국_채권_수익률_Percent), color = "green") +
  geom_line(aes(y = BTC_일일_수익률_Percent*50), color = 'orange') +
  scale_y_continuous(
    name = "Bond Percent axis",
    sec.axis = sec_axis(~. /50, name="BTC Percent axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 일일 수익률(%) vs 미국 채권 수익률(%)",
       x = "날짜",
       y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))


Kospi_plot <- ggplot(data = final_frame,
                    aes(x = 날짜)) +
  geom_line(aes(y = KOSPI_수익률_Percent), color = "pink") +
  geom_line(aes(y = BTC_일일_수익률_Percent*15), color = 'orange') +
  scale_y_continuous(
    name = "KOSPI Percent axis",
    sec.axis = sec_axis(~. /15, name="BTC Percent axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 일일 수익률(%) vs KOSPI 수익률(%)",
       x = "날짜",
       y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

Price_plot <- ggplot(data = final_frame,
                     aes(x = 날짜)) +
  geom_line(aes(y = 소비자물가지수-100), 
            color = "black",
            size = 1.3) +
  geom_line(aes(y = BTC_일일_수익률_Percent*100), 
            color = 'orange') +
  scale_y_continuous(
    name = "Price index axis",
    sec.axis = sec_axis(~. /100, name="BTC Percent axis")
  ) + 
  theme_classic() +
  labs(title = "BTC 일일 수익률(%) vs 소비자물가지수",
       x = "날짜",
       y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(USD_plot, Gold_plot, Bond_plot, Kospi_plot, Price_plot, nrow= 2)


## 2. 설명변수간 상관관계 확인 --> 상관관계가 크다면 다중회귀분석을 사용하기 곤란하다. 만약 작다면 사용해도 무방하므로 사용해보겠다는 flow
# 피어슨 상관분석 --> 설명변수 간 선형상관관계도 작지만, 설명변수와 종속변수 간 선형상관관계도 너무 작음. 이거 한 후에 비선형 상관관계도 고려해보자. 
data_cor <- cor(final_frame[,-1])
corrplot(data_cor, method = 'number', type = "upper")
cor.test(data_cor,
         alternative = c("two.sided"),
         method = "pearson")

# 스피어만 상관분석 --> 설명변수와 종속변수 간 비선형상관관계도 너무 작음. 설명변수 추가해야하나.
spearman_cor <- cor(final_frame[, -1], method = 'spearman')
corrplot(spearman_cor, method = 'number', type = 'upper')

# 상관계수 검정. 영가설: 상관계수가 없다.
cor_pairwise_test <- rcorr(data_cor[,-c(1)], 
                           type = 'spearman')
cor_pairwise_test[[3]]

corrplot(cor_pairwise_test[[3]], method = 'number', type = 'upper') # 상관검정. 모든 설명변수들 상관관계가 유의하지 않다?????



## 3. BTC 가격이 급격히 상승하거나 하락했을 때 설명변수의 경제지표와 관련해서 무슨 사건이 있었는지.
# BTC 가격이 크게 변동했던 상위 10개의 날짜 출력.
sup <- final_frame[, "BTC_일일_수익률_Percent"] %>% as.matrix %>% sort %>% tail(10)
sup
inf <- final_frame[, "BTC_일일_수익률_Percent"] %>% as.matrix %>% sort %>% head(10)
inf
sup_index <- which( final_frame[, "BTC_일일_수익률_Percent"] %>% as.matrix %in% sup)
sup_index
inf_index <-which( final_frame[, "BTC_일일_수익률_Percent"] %>% as.matrix %in% inf)
inf_index
final_frame[1767,"BTC_일일_수익률_Percent"]

x <- final_frame[sup_index, "날짜"]
y <- final_frame[inf_index, "날짜"]
cbind(x,y)
x
y

final_frame[which(final_frame[,1] == "2017-12-14"),c(1,6)]
final_frame[which(final_frame[,1] == "2018-01-12"),c(1,6)]
final_frame[which(final_frame[,1] == "2017-12-12"),c(1,6)]
final_frame[which(final_frame[,1] == "2021-02-24"),c(1,6)]
final_frame[which(final_frame[,1] == "2021-02-23"),c(1,6)]
final_frame[which(final_frame[,1] == "2021-02-25"),c(1,6)]
final_frame[which(final_frame[,1] == "2021-02-10"),c(1,6)]









