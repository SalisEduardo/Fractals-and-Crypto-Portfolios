
#Packages

library(xts)
library(zoo)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(RiskPortfolios)
library(covRobust)
library(quantmod)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(tibble)
library(corrplot)
library(GGally)
library(RColorBrewer)

source("functions.R") # set of functions
source("risk_free.R") # obtaining risk free data


# Data ------------------------------------

excel_origin_date = "1899-12-30"


BTC <- read_excel("dados.xlsx",sheet =1)
names(BTC)[2:dim(BTC)[2]] <- paste("BTC",names(BTC)[2:dim(BTC)[2]],sep=".")
BTC$Date  <- BTC$Date %>% as.Date(origin = "1899-12-30")

ETH <- read_excel("dados.xlsx",sheet =2)
names(ETH)[2:dim(ETH)[2]] <- paste("ETH",names(ETH)[2:dim(ETH)[2]],sep=".")
ETH$Date  <- ETH$Date %>% as.Date(origin = excel_origin_date)

BNB <- read_excel("dados.xlsx",sheet =3)
names(BNB)[2:dim(BNB)[2]] <- paste("BNB",names(BNB)[2:dim(BNB)[2]],sep=".")
BNB$Date  <- BNB$Date %>% as.Date(origin = excel_origin_date)

XRP <- read_excel("dados.xlsx",sheet =4)
names(XRP)[2:dim(XRP)[2]] <- paste("XRP",names(XRP)[2:dim(XRP)[2]],sep=".")
XRP$Date  <- XRP$Date %>% as.Date(origin = excel_origin_date)

ADA <- read_excel("dados.xlsx",sheet =5)
names(ADA)[2:dim(ADA)[2]] <- paste("ADA",names(ADA)[2:dim(ADA)[2]],sep=".")
ADA$Date  <- ADA$Date %>% as.Date(origin = excel_origin_date)

DOGE <- read_excel("dados.xlsx",sheet =6)
names(DOGE)[2:dim(DOGE)[2]] <- paste("DOGE",names(DOGE)[2:dim(DOGE)[2]],sep=".")
DOGE$Date  <- DOGE$Date %>% as.Date(origin = excel_origin_date)

TRX <- read_excel("dados.xlsx",sheet =7)
names(TRX)[2:dim(TRX)[2]] <- paste("TRX",names(TRX)[2:dim(TRX)[2]],sep=".")
TRX$Date  <- TRX$Date %>% as.Date(origin = excel_origin_date)

LTC <- read_excel("dados.xlsx",sheet =8)
names(LTC)[2:dim(LTC)[2]] <- paste("LTC",names(LTC)[2:dim(LTC)[2]],sep=".")
LTC$Date  <- LTC$Date %>% as.Date(origin = excel_origin_date)

BTCH <- read_excel("dados.xlsx",sheet =9)
names(BTCH)[2:dim(BTCH)[2]] <- paste("BTCH",names(BTCH)[2:dim(BTCH)[2]],sep=".")
BTCH$Date  <- BTCH$Date %>% as.Date(origin = excel_origin_date)

LINK <- read_excel("dados.xlsx",sheet =10)
names(LINK)[2:dim(LINK)[2]] <- paste("LINK",names(LINK)[2:dim(LINK)[2]],sep=".")
LINK$Date  <- LINK$Date %>% as.Date(origin = excel_origin_date)

XLM <- read_excel("dados.xlsx",sheet =11)
names(XLM)[2:dim(XLM)[2]] <- paste("XLM",names(XLM)[2:dim(XLM)[2]],sep=".")
XLM$Date  <- XLM$Date %>% as.Date(origin = excel_origin_date)

ETC <- read_excel("dados.xlsx",sheet =12)
names(ETC)[2:dim(ETC)[2]] <- paste("ETC",names(ETC)[2:dim(ETC)[2]],sep=".")
ETC$Date  <- ETC$Date %>% as.Date(origin = excel_origin_date)

XRM <- read_excel("dados.xlsx",sheet =13)
names(XRM)[2:dim(XRM)[2]] <- paste("XRM",names(XRM)[2:dim(XRM)[2]],sep=".")
XRM$Date  <- XRM$Date %>% as.Date(origin = excel_origin_date)

FIL <- read_excel("dados.xlsx",sheet =14)
names(FIL)[2:dim(FIL)[2]] <- paste("FIL",names(FIL)[2:dim(FIL)[2]],sep=".")
FIL$Date  <- FIL$Date %>% as.Date(origin = excel_origin_date)

MANA <- read_excel("dados.xlsx",sheet =15)
names(MANA)[2:dim(MANA)[2]] <- paste("MANA",names(MANA)[2:dim(MANA)[2]],sep=".")
MANA$Date  <- MANA$Date %>% as.Date(origin = excel_origin_date)

EOS <- read_excel("dados.xlsx",sheet =16)
names(EOS)[2:dim(EOS)[2]] <- paste("EOS",names(EOS)[2:dim(EOS)[2]],sep=".")
EOS$Date  <- EOS$Date %>% as.Date(origin = excel_origin_date)

KCS <- read_excel("dados.xlsx",sheet =17)
names(KCS)[2:dim(KCS)[2]] <- paste("KCS",names(KCS)[2:dim(KCS)[2]],sep=".")
KCS$Date  <- KCS$Date %>% as.Date(origin = excel_origin_date)

ZEC <- read_excel("dados.xlsx",sheet =18)
names(ZEC)[2:dim(ZEC)[2]] <- paste("ZEC",names(ZEC)[2:dim(ZEC)[2]],sep=".")
ZEC$Date  <- ZEC$Date %>% as.Date(origin = excel_origin_date)

WAVES <- read_excel("dados.xlsx",sheet =19)
names(WAVES)[2:dim(WAVES)[2]] <- paste("WAVES",names(WAVES)[2:dim(WAVES)[2]],sep=".")
WAVES$Date  <- WAVES$Date %>% as.Date(origin = excel_origin_date)

crypto_list <- list(BTC,ETH,BNB,XRP,ADA,DOGE,TRX,LTC,BTCH,LINK,XLM,ETC,XRM,FIL,MANA,EOS,KCS,ZEC,WAVES)

crypto_dataframe <- crypto_list %>% reduce(full_join,by='Date')

crypto_returns <- crypto_dataframe %>% 
  dplyr::select(Date,ends_with("Return")) %>% 
  arrange(Date) %>% 
  rename_with(~str_remove(., '.Return'))

crypto_returns_xts <- xts(as.data.frame(crypto_returns[,-1]),order.by=crypto_returns$Date)

crypto_mktCap <- crypto_dataframe %>% 
  dplyr::select(Date,ends_with("MarketCap")) %>% 
  arrange(Date) %>% 
  rename_with(~str_remove(., '.MarketCap'))


crypto_mktCap_xts <- xts(as.data.frame(crypto_mktCap[,-1]),order.by=crypto_mktCap$Date)


# Selecting by efficiency----------------------------------------------------------------------------------------------------------------



first_train_period <- "2018/2019"
first_test_period <- "2020"

second_train_period <- "2019/2020"
second_test_period <- "2021"


third_train_period <- "2020/2021"
third_test_period <- "2022"


first_deltaH <- table.assets.fractality(crypto_returns_xts[first_train_period],calcDeltaH,"DeltaH_2018_2019")
second_deltaH <- table.assets.fractality(crypto_returns_xts[second_train_period],calcDeltaH,"DeltaH_2019_2020")
third_deltaH <- table.assets.fractality(crypto_returns_xts[third_train_period],calcDeltaH,"DeltaH_2020_2021")

all_deltaH <- list(first_deltaH,second_deltaH,third_deltaH) %>% reduce(left_join, by = "Ticker")

#write.csv(all_deltaH,"Results/Fractality/deltaH.csv")

effic_ranks <- sapply(all_deltaH[-1], rank) %>%  as.data.frame()
colnames(effic_ranks) <- c("RankEffic_2018_2019", "RankEffic_2019_2020", "RankEffic_2020_2021")
effic_ranks$Ticker <- all_deltaH$Ticker
effic_ranks <- effic_ranks[c('Ticker',"RankEffic_2018_2019", "RankEffic_2019_2020", "RankEffic_2020_2021")]

#write.csv(effic_ranks,"Results/Fractality/ranks_deltaH.csv")

## Portfolios build with 4 crypto currencies
top4_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = 4) #top ranks gets a positive sign
bottom4_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = -4) #bottom ranks gets a positive sign

top4_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = 4)
bottom4_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = -4)

top4_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = 4)
bottom4_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = -4)

## Portfolios build with 8 crypto currencies

top8_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = 8)
bottom8_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = -8)

top8_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = 8)
bottom8_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = -8)

top8_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = 8)
bottom8_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = -8)


crypto_names <- colnames(crypto_returns_xts) 


# Tests -----------------------------------------------

backtest_max40 <- execute_backtest_box_constraints(pspec.box.fullmax40)

max40pct_KPIS <- get.RoubtsTest.KPIs(backtest_max40,name_pattern = '_')%>%  
  mutate(Strategy_name = paste(Strategy_name,"0.4",sep="_"))


backtest_max50 <- execute_backtest_box_constraints(pspec.box.fullmax50)

max50pct_KPIS <- get.RoubtsTest.KPIs(backtest_max50,name_pattern = '_')%>%  
  mutate(Strategy_name = paste(Strategy_name,"0.5",sep="_"))


backtest_max60 <- execute_backtest_box_constraints(pspec.box.fullmax60)

max60pct_KPIS <- get.RoubtsTest.KPIs(backtest_max60,name_pattern = '_')%>%  
  mutate(Strategy_name = paste(Strategy_name,"0.6",sep="_"))




all_box_backtest_MAX <- rbind(max40pct_KPIS,max50pct_KPIS,max60pct_KPIS)

box_backtest2020_MAX  <- all_box_backtest_MAX %>%  
  filter(Test_period == 2020) %>% 
  pivot_wider(id_cols = Strategy_name,values_from = value,names_from = variable)

box_backtest2021_MAX <- all_box_backtest_MAX %>%  
  filter(Test_period == 2021) %>% 
  pivot_wider(id_cols = Strategy_name,values_from = value,names_from = variable)

box_backtest2022_MAX <- all_box_backtest_MAX %>%  
  filter(Test_period == 2022) %>% 
  pivot_wider(id_cols = Strategy_name,values_from = value,names_from = variable)


box_backtest2020_MAX %>%  write_csv("Results/RobustTest_minaloc/Max_aloc/box_MAX_backtest2020.csv")
box_backtest2021_MAX %>%  write_csv("Results/RobustTest_minaloc/Max_aloc/box_MAX_backtest2021.csv")
box_backtest2022_MAX %>%  write_csv("Results/RobustTest_minaloc/Max_aloc/box_MAX_backtest2022.csv")





####################################################################
backtest_1pct <- execute_backtest_box_constraints(pspec.box.full.1pct)
min1pct_KPIS <- get.RoubtsTest.KPIs(backtest_1pct,name_pattern = '_')%>%  
  mutate(Strategy_name = paste(Strategy_name,"0.01",sep="_"))

backtest_2pct <- execute_backtest_box_constraints(pspec.box.full.2pct)
min2pct_KPIS <- get.RoubtsTest.KPIs(backtest_2pct,name_pattern = '_') %>% 
  mutate(Strategy_name = paste(Strategy_name,"0.02",sep="_"))


backtest_5pct <- execute_backtest_box_constraints(pspec.box.full.5pct)
min5pct_KPIS <- get.RoubtsTest.KPIs(backtest_5pct,name_pattern = '_')%>% 
  mutate(Strategy_name = paste(Strategy_name,"0.05",sep="_"))

all_box_backtest <- rbind(min1pct_KPIS,min2pct_KPIS,min5pct_KPIS)

box_backtest2020 <- all_box_backtest %>%  
  filter(Test_period == 2020) %>% 
  pivot_wider(id_cols = Strategy_name,values_from = value,names_from = variable)

box_backtest2021 <- all_box_backtest %>%  
  filter(Test_period == 2021) %>% 
  pivot_wider(id_cols = Strategy_name,values_from = value,names_from = variable)

box_backtest2022 <- all_box_backtest %>%  
  filter(Test_period == 2022) %>% 
  pivot_wider(id_cols = Strategy_name,values_from = value,names_from = variable)


box_backtest2020 %>%  write_csv("Results/RobustTest_minaloc/box_backtest2020.csv")
box_backtest2021 %>%  write_csv("Results/RobustTest_minaloc/box_backtest2021.csv")
box_backtest2022 %>%  write_csv("Results/RobustTest_minaloc/box_backtest2022.csv")



