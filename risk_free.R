library(xts)
library(zoo)
library(quantmod)
library(tidyverse)

t10 <- quantmod::getSymbols("DGS10",src = "FRED",auto.assign = FALSE) 
t10 <- t10['2019/']


t10 <- t10/100 # acording to FRED , the returns are in percentage


t10 <- na.locf(t10,fromLast = TRUE) # fill with previous day


t10_daily <- ((1+ t10) ^ (1/252)) - 1


t10_daily_2020 <- t10_daily["2020"] %>%  na.omit()
t10_daily_2021 <- t10_daily["2021"] %>%  na.omit()
t10_daily_2022 <- t10_daily["2022/2022-10"] %>%  na.omit()





