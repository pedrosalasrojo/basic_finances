
#         Author: Pedro Salas Rojo
#         Date: 09/03/2022
#         Dataset: Tesla (yahoo finances)
#         Name of project: Basic plots 

rm(list = ls(all.names = TRUE)) 

library(quantmod)
library(tidyverse)
library(TTR)
library(lubridate)
library(ggplot2)
library(PerformanceAnalytics)

# Get date (today) and other terms ----
now  <- as.Date(now())
long_t <- as.Date(seq(now, length = 2, by = "-15 months")[2])
med_t  <- as.Date(seq(now, length = 2, by = "-8 months")[2])
short_t  <- as.Date(seq(now, length = 2, by = "-4 months")[2])
vshort_t <- as.Date(seq(now, length = 2, by = "-2 months")[2])

# Get data (use symbol, for instance, from yahoo.finances) ----

symbolBasket <- c('TSLA')
getSymbols(symbolBasket , src='yahoo', from=long_t)
VAL=TSLA
names(VAL) <- c("Open"   ,  "High"   ,  "Low"   ,   "Close"  ,  "Volume",  "Adjusted")

# Get subsets

long <- VAL[paste0(long_t,"/",now)]
med <- VAL[paste0(med_t,"/",now)]
short <- VAL[paste0(short_t,"/",now)]
vshort <- VAL[paste0(vshort_t,"/",now)]

# Plots / Charts ----

# Basic values

lineChart(med$Adjusted, line.type='l', up.col='red',
          theme='black', name='Adjusted Price',
          TA=NULL)

#High/Low/Close

barChart(short, bar.type='ohlc', theme='black', 
         up.col='green', dn.col='red', TA=NULL)

#Candle Chart
candleChart(short, up.col='green', dn.col='red')

#Candle Chart with SMA
chartSeries(med, 
            type = c("auto", "matchsticks"), 
            subset = short,
            show.grid = TRUE,
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE, up.col='green', dn.col='red',
            TA=c(addSMA(n=100,col = 'blue'),addSMA(n=50,col = 'red'),addSMA(n=10,col = 'green'),
                 addROC(n=100,col = 'blue'),addROC(n=50,col = 'red'),addROC(n=10,col = 'green')))

# Bellinger Bands
chartSeries(med, theme="black",
            TA="addBBands(n=20,sd=1.5);addCCI()", 
            up.col='green', dn.col='red')

#Multiple means
VAL <- na.omit(VAL)
VAL.10 <- SMA(VAL$Close, n=10 ) 
VAL.50 <- SMA(VAL$Close, n=50 ) 
VAL.200 <- SMA(VAL$Close, n=200 ) 
Fast.Diff <- VAL.10 - VAL.50
Slow.Diff <- VAL.50 - VAL.200
chartSeries(VAL$Close, theme="black", TA=c(addEMA(50, col='red'), addEMA(200, col='blue'),
                                           addTA(Fast.Diff, col='green', type='h',legend="10-50 MA used for in-out of market"),
                                           addTA(Slow.Diff, col='blue', type='h',legend="50-200 MA give trending sense")))


#Candle plot (MACD)
candleChart(med, TA=c(addMACD(), addVo()), 
            theme='black', up.col='green', 
            dn.col='red')

#Candle plot (ADX)
candleChart(short, TA=c(addADX(wilder=TRUE), addVo()), 
            theme='black', up.col='green', dn.col='red')

#Candle plot (ATR)
candleChart(med, TA=c(addATR(), addVo()), 
            theme='black', up.col='green',
            dn.col='red')

#Two returns: arithmetic and simple
arit <- coredata(VAL$Adjusted)
n <- nrow(arit)
arit_1 <- (arit[2:n, 1] - arit[1:(n - 1), 1]) / arit[1:(n - 1), 1]
head(arit_1)

arit_2 = Return.calculate(VAL)
head(arit_2)
lineChart(arit_2$Adjusted,   line.type='l', 
          up.col='red', theme='black', 
          name='Low', TA=NULL)

