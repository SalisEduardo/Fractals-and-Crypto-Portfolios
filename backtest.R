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



PERIODS <- list(list(train=first_train_period,test=first_test_period),
                list(train=second_train_period,test=second_test_period),
                list(train=third_train_period,test=third_test_period))

first_deltaH <- table.assets.fractality(crypto_returns_xts[first_train_period],calcDeltaH,"DeltaH_2018_2019")
second_deltaH <- table.assets.fractality(crypto_returns_xts[second_train_period],calcDeltaH,"DeltaH_2019_2020")
third_deltaH <- table.assets.fractality(crypto_returns_xts[third_train_period],calcDeltaH,"DeltaH_2020_2021")

all_deltaH <- list(first_deltaH,second_deltaH,third_deltaH) %>% reduce(left_join, by = "Ticker")

effic_ranks <- sapply(all_deltaH[-1], rank) %>%  as.data.frame()
colnames(effic_ranks) <- c("RankEffic_2018_2019", "RankEffic_2019_2020", "RankEffic_2020_2021")
effic_ranks$Ticker <- all_deltaH$Ticker
effic_ranks <- effic_ranks[c('Ticker',"RankEffic_2018_2019", "RankEffic_2019_2020", "RankEffic_2020_2021")]

get_deltaH <- function(period){
  if(period == first_train_period){
    deltaH <- first_deltaH
  }else if(period == second_train_period){
    deltaH <- second_deltaH
  }else{
    deltaH <- third_deltaH
  }
  return(deltaH)
}

get_strats_weights <- function(strategies_list,group_name,folder_path="Results/Complete_Backtest/Weights/",export=FALSE){
  # ONLY FOR STRATS WITH THE SAME ASSETS
  
  DFweights <- NULL
  rnames <- c()
  for(s in strategies_list){
    
    rnames <- append(rnames,s$name)
    
    DFweights <- rbind(DFweights,s$w)
    
  }
  rownames(DFweights) <- rnames
  DFweights <- DFweights %>%  round(4)
  
  if(export){
    fname <-paste(folder_path,group_name,".csv",sep="")
    DFweights %>% write.csv(fname)
    
  }
  
  return(DFweights)
}


execute_defaultpolicies <- function(opt,asset_names,train,test,status_efficiency ="most"){
  
  periodDeltaH <- get_deltaH(train)
  
  n_assets <- length(asset_names)
  
  MVP <-  build.portfolio.strats( paste("MVP",status_efficiency,as.character(n_assets),test,sep='_'),
                                  asset_names,
                                  crypto_returns_xts,
                                  train,
                                  test,
                                  pspec.lo.full,
                                  mvp.spec, 
                                  neg_to_zero = TRUE,
                                  optimizor = opt)
  
  
  
  
  
  maxSR <-  build.portfolio.strats(paste("maxSR",status_efficiency,as.character(n_assets),test,sep='_'),
                                   asset_names,
                                   crypto_returns_xts,
                                   train,
                                   test,
                                   pspec.lo.full,
                                   tp.sepc,
                                   maxSharp = TRUE,
                                   neg_to_zero = TRUE,
                                   optimizor = opt)
  
  
  EW <- build.EW.portfolio( paste("EW",status_efficiency,as.character(n_assets),test,sep='_'),
                            asset_names,
                            crypto_returns_xts,
                            train,
                            test)
  
  
  weights_InvInef <- format.InvInef.weights.df(periodDeltaH,colnames(periodDeltaH)[2],asset_names)
  
  InvInef <- build.inverse.inefficency.strategy(paste("InvInef",status_efficiency,as.character(n_assets),test,sep='_'),
                                                weights_InvInef,
                                                crypto_returns_xts,
                                                train,
                                                test)
  
  
  strats <- list(MVP,maxSR,EW,InvInef)
  
  weights_Tab_name <- paste(status_efficiency,as.character(n_assets),test,sep='_')
  
  weights <- get_strats_weights(strategies_list = strats,group_name  = weights_Tab_name,export=TRUE)
  
  return(strats)
  
}




compleate_backtest <- function(periods_keyval,optm='DEoptim',portfolio_sizes=c(4,6,8)){
  
  strategies_backtest <- list()
  
  
  for(psize in portfolio_sizes){
    
    
    ## Portfolios build with 4 crypto currencies
    mostN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = psize) #top ranks gets a positive sign
    lessN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = - psize) #bottom ranks gets a positive sign
    
    mostN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = psize)
    lessN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = - psize)
    
    mostN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = psize)
    lessN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = - psize)
    
    all_mostN <- list(mostN_first,mostN_second,mostN_third)
    all_lessN <- list(lessN_first,lessN_second,lessN_third)
    
    
    mostN_first_strats <-execute_defaultpolicies(opt=optm,
                                                 asset_names=mostN_first,
                                                 train=first_train_period,
                                                 test=first_test_period,
                                                 status_efficiency ="most")
    
    mostN_second_strats <-execute_defaultpolicies(opt=optm,
                                                  asset_names=mostN_second,train=second_train_period,test=second_test_period,status_efficiency ="most")
    mostN_third_strats <-execute_defaultpolicies(opt=optm,asset_names=mostN_third,train=third_train_period,test=third_test_period,status_efficiency ="most")
    
    lessN_first_strats <-execute_defaultpolicies(opt=optm,asset_names=lessN_first,train=first_train_period,test=first_test_period,status_efficiency ="less")
    lessN_second_strats <-execute_defaultpolicies(opt=optm,asset_names=lessN_second,train=second_train_period,test=second_test_period,status_efficiency ="less")
    lessN_third_strats <-execute_defaultpolicies(opt=optm,asset_names=lessN_third,train=third_train_period,test=third_test_period,status_efficiency ="less")
    
    
    for(s in 1:length(mostN_first_strats)){
      strategies_backtest <- append(strategies_backtest,mostN_first_strats[s])
    }
    
    for(s in 1:length(mostN_second_strats)){
      strategies_backtest <- append(strategies_backtest,mostN_second_strats[s])
    }
    
    for(s in 1:length(mostN_third_strats)){
      strategies_backtest <- append(strategies_backtest,mostN_third_strats[s])
    }
    for(s in 1:length(lessN_first_strats)){
      strategies_backtest <- append(strategies_backtest,lessN_first_strats[s])
    }
    
    for(s in 1:length(lessN_second_strats)){
      strategies_backtest <- append(strategies_backtest,lessN_second_strats[s])
    }
    
    for(s in 1:length(lessN_third_strats)){
      strategies_backtest <- append(strategies_backtest,lessN_third_strats[s])
    }
    
  }
  return(strategies_backtest)

}



get.Backtest.KPIs <- function(strategies_list,RF=0){
  df_KPIS <- data.frame()
  
  for (s in strategies_list){
    
    
    
    df_cumrets <- totalReturn(s$R)  
    
    df_returns <- table.AnnualizedReturns(s$R,Rf = RF) 
    
    df_DR <- table.DownsideRisk(s$R,MAR = 0, p=0.95)

    df_KPIS_strat <- rbind(
      totalReturn(s$R),
      table.AnnualizedReturns(s$R,Rf = RF),
      table.DownsideRisk(s$R,MAR = 0, p=0.95)
      )
    colnames(df_KPIS_strat) <- c(s$name)
    df_KPIS_strat <- df_KPIS_strat %>% t() %>% as.data.frame() 
    
    df_KPIS <- rbind(df_KPIS , df_KPIS_strat)
  }
 
   
   return(df_KPIS)
  
}

# get.Backtest.KPIs(STRATS_DEOPTM_468)


STRATS_DEOPTM_468 <- compleate_backtest(PERIODS,optm = 'DEoptim')

STRATS_DEOPTM_468[[29]]$name
STRATS_DEOPTM_468[[29]]$w

STRATS_DEOPTM_468[[30]]$name
STRATS_DEOPTM_468[[30]]$w

STRATS_DEOPTM_468[[31]]$name
STRATS_DEOPTM_468[[31]]$w

STRATS_DEOPTM_468[[32]]$name
STRATS_DEOPTM_468[[32]]$w


STRATS_DEOPTM_468[[33]]$name
STRATS_DEOPTM_468[[33]]$w

STRATS_DEOPTM_468[[34]]$name
STRATS_DEOPTM_468[[34]]$w

STRATS_DEOPTM_468[[35]]$name
STRATS_DEOPTM_468[[35]]$w

STRATS_DEOPTM_468[[36]]$name
STRATS_DEOPTM_468[[36]]$w


STRATS_DEOPTM_468[[37]]$name
STRATS_DEOPTM_468[[37]]$w

STRATS_DEOPTM_468[[38]]$name
STRATS_DEOPTM_468[[38]]$w


STRATS_DEOPTM_468[[39]]$name
STRATS_DEOPTM_468[[39]]$w


STRATS_DEOPTM_468[[40]]$name
STRATS_DEOPTM_468[[40]]$w






KPIS_backtest <- get.Backtest.KPIs(STRATS_DEOPTM_468)


KPIS_backtest <- tibble::rownames_to_column(KPIS_backtest, "Strats_names")

KPIS_backtest[c("Policy","Efficency_group","N_assets","Test_period")]<- str_split_fixed(KPIS_backtest$Strats_names, '_', 4)


selected_kpis_cols <- c("Strats_names","Policy","Efficency_group","N_assets","Test_period","Total Return","Annualized Return","Annualized Std Dev","Annualized Sharpe (Rf=0%)","Historical VaR (95%)")

KPIS_backtest <- KPIS_backtest[selected_kpis_cols]

KPIS_backtest  %>%  writexl::write_xlsx("Results/Complete_Backtest/BacktestKPIs.xlsx")


