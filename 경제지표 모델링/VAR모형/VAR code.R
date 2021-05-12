setwd("C:/Users/10sop/Desktop/°æÁ¦ÁöÇ¥ ¸ğµ¨¸µ/VAR¸ğÇü")

library(tidyverse)
library(data.table)
library(vars)
library(urca)
library(tseries)
library(gridExtra)
library(ggplot)


dat=fread("Economic_frame1.csv",data.table = F)
dat<-dat[,-c(1,19)]
dat %>% class
dat <- dat[order(dat[,"Date"]), ]
rownames(dat) <- NULL


###ADFtest(?‹¨?œ„ê·¼ê?€? •)###
###rawdata###
for(i in 2:ncol(dat)){
  print(paste0(colnames(dat)[i],"   ADFtest"))
  print(adf.test(dat[,i])) 
}
library(ggcorrplot)
cor<-cor(dat[,2:17])

cor<-ggcorrplot(cor,hc.order = TRUE, 
                type = "lower",
                lab = TRUE)


###diff###
dat1<-dat[-1,]
for(i in 2:ncol(dat1)){
  dat1[,i]<-dat[,i] %>% diff()
}
for(i in 2:ncol(dat1)){
  print(paste0(colnames(dat1)[i],"   ADFtest"))
  print(adf.test(dat1[,i])) 
}


cor1<-cor(dat1[,2:17])
cor1<-ggcorrplot(cor1, 
                 type = "lower",
                 lab = TRUE)
gri


####Granger Causality Test ### °¨¼º, °Å·¡·® ±×´ë·Î ³Ö°í ±İÀ¶À¸·Î FAVAR
# H0: A does not Granger cause B

y<-dat1[,-1]
VARselect(y,lag.max=8,type="const")
#p=8

for(i in 2:ncol(y)){
  print(paste0(colnames(y)[i],"   Granger causality test"))
  print(grangertest(y[,i], y[,1], order = 8))
}
for(i in 2:ncol(y)){
  print(paste0(colnames(y)[i],"   Granger causality test"))
  print(grangertest( y[,1], y[,i], order = 8))
}
#Gold_price,Bond_yield,BTC_circulation,VKOSPI
datv<-y[,-c(2,3,6,9)]


###VAR###
VAR <- VAR(datv, p = 8, type = "const")

serl <- serial.test(VAR, lags.pt = 16, type = "PT.asymptotic")
serl$serial
#p-value < 2.2e-16

###Impulse Response Analysis ###

var.irf<-irf(VAR, impulse = 'BTC_price', response = 'BTC_price', boot =TRUE)
plot(var.irf)

var.irf2<-irf(VAR, impulse = "KOSPI", response = 'BTC_price', boot =TRUE)
plot(var.irf2)

var.irf3<-irf(VAR, impulse = "USD_exchange_rate", response = 'BTC_price', boot =TRUE)
plot(var.irf3)

var.irf4<-irf(VAR, impulse = "VIX", response = 'BTC_price', boot =TRUE)
plot(var.irf4)

var.irf5<-irf(VAR, impulse = "NASDAQ", response = 'BTC_price', boot =TRUE)
plot(var.irf5)

var.irf6<-irf(VAR, impulse = colnames(datv[,6]), response = 'BTC_price', boot =TRUE)
plot(var.irf6)

var.irf7<-irf(VAR, impulse = "Nvidia", response = 'BTC_price', boot =TRUE)
plot(var.irf7)

var.irf8<-irf(VAR, impulse = "AMD", response = 'BTC_price', boot =TRUE)
plot(var.irf8)

var.irf9<-irf(VAR, impulse = "MA", response = 'BTC_price', boot =TRUE)
plot(var.irf9)

var.irf10<-irf(VAR, impulse = "PYPL", response = 'BTC_price', boot =TRUE)
plot(var.irf10)

var.irf11<-irf(VAR, impulse = "SQ", response = 'BTC_price', boot =TRUE)
plot(var.irf11)

var.irf12<-irf(VAR, impulse = "vidente", response = 'BTC_price', boot =TRUE)
plot(var.irf12)


