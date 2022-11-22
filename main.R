
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

source("functions.R")


#Data

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

crypto_returns_xts['2018/'] 


#----------------------------------------------------------------------------------------------------------------

# Selecting by efficiency

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
top4_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = 4)
bottom4_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = -4)

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

#----------------------------------------------------------------------------------------------------------

# Individual Performance of each crypto


crypto.Performance_18_22 <- performance_summary_v1(crypto_returns_xts['2018/'])

plot_V1_crypto_correlation <- GGally::ggcorr(crypto_returns_xts['2018/'],method = c("everything", "pearson"),label = TRUE)
#assets_pairs <- GGally::ggpairs(as.data.frame(coredata(crypto_returns_xts['2018/'])), title="Correlation pairs") 

plot_V2_crypto_correlation <- corrplot.mixed(cor(crypto_returns_xts['2018/']), order = 'AOE')


#--------------------------------------------------------------------------------------------------------------------
# Strategies - (short sell restriction) - 4 assets within portfolios

## Minimum Variance Portfolios

### More Efficient cryptos
first_MVP_top4 <-  build.portfolio.strats("first_MVP_top4" ,
                                          top4_first,
                                          crypto_returns_xts,
                                          first_train_period,
                                          first_test_period,
                                          pspec.lo.full,
                                          mvp.spec,
                                          neg_to_zero = TRUE)

second_MVP_top4 <-  build.portfolio.strats("second_MVP_top4" ,
                                           top4_second,
                                           crypto_returns_xts,
                                           second_train_period,
                                           second_test_period,
                                           pspec.lo.full,
                                           mvp.spec,
                                           neg_to_zero = TRUE)

third_MVP_top4 <-  build.portfolio.strats("third_MVP_top4" ,
                                          top4_third,
                                          crypto_returns_xts,
                                          third_train_period,
                                          third_test_period,
                                          pspec.lo.full,
                                          mvp.spec,
                                          neg_to_zero = TRUE)



top4_MVP_returns <- rbind(first_MVP_top4$R,second_MVP_top4$R,third_MVP_top4$R)


### More Inefficient cryptos


first_MVP_bottom4 <-  build.portfolio.strats("first_MVP_bottom4" ,
                                          bottom4_first,
                                          crypto_returns_xts,
                                          first_train_period,
                                          first_test_period,
                                          pspec.lo.full,
                                          mvp.spec,
                                          neg_to_zero = TRUE)


second_MVP_bottom4 <-  build.portfolio.strats("second_MVP_bottom4" ,
                                             bottom4_second,
                                             crypto_returns_xts,
                                             second_train_period,
                                             second_test_period,
                                             pspec.lo.full,
                                             mvp.spec,
                                             neg_to_zero = TRUE)


third_MVP_bottom4 <-  build.portfolio.strats("third_MVP_bottom4" ,
                                             bottom4_third,
                                             crypto_returns_xts,
                                             third_train_period,
                                             third_test_period,
                                             pspec.lo.full,
                                             mvp.spec,
                                             neg_to_zero = TRUE)


bottom4_MVP_returns <- rbind(first_MVP_bottom4$R,second_MVP_bottom4$R,third_MVP_bottom4$R)

## Max Sharpe ---->  not diversifying

### More Efficient cryptos
first_maxSR_top4 <-  build.portfolio.strats("first_maxSR_top4" ,
                                              top4_first,
                                              crypto_returns_xts,
                                              first_train_period,
                                              first_test_period,
                                              pspec.lo.full,
                                              tp.sepc,
                                              maxSharp = TRUE,
                                              neg_to_zero = TRUE)


second_maxSR_top4 <-  build.portfolio.strats("second_maxSR_top4" ,
                                            top4_second,
                                            crypto_returns_xts,
                                            second_train_period,
                                            second_test_period,
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


third_maxSR_top4 <-  build.portfolio.strats("third_maxSR_top4" ,
                                            top4_third,
                                            crypto_returns_xts,
                                            third_train_period,
                                            third_test_period,
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


top4_maxSR_returns <- rbind(first_maxSR_top4$R,second_maxSR_top4$R,third_maxSR_top4$R)

### More Inefficient cryptos

first_maxSR_bottom4 <-  build.portfolio.strats("first_maxSR_bottom4" ,
                                            bottom4_first,
                                            crypto_returns_xts,
                                            first_train_period,
                                            first_test_period,
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


second_maxSR_bottom4 <-  build.portfolio.strats("second_maxSR_bottom4" ,
                                             bottom4_second,
                                             crypto_returns_xts,
                                             second_train_period,
                                             second_test_period,
                                             pspec.lo.full,
                                             tp.sepc,
                                             maxSharp = TRUE,
                                             neg_to_zero = TRUE)


third_maxSR_bottom4 <-  build.portfolio.strats("third_maxSR_bottom4" ,
                                            bottom4_third,
                                            crypto_returns_xts,
                                            third_train_period,
                                            third_test_period,
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)



top4_maxSR_returns <- rbind(first_maxSR_top4$R,second_maxSR_top4$R,third_maxSR_top4$R)


# EW 

### More Efficient cryptos

first_EW_top4 <- build.EW.portfolio("first_EW_top4" ,
                                    top4_first,
                                    crypto_returns_xts,
                                    first_train_period,
                                    first_test_period)

second_EW_top4 <- build.EW.portfolio("second_EW_top4" ,
                                    top4_second,
                                    crypto_returns_xts,
                                    second_train_period,
                                    second_test_period)

third_EW_top4 <- build.EW.portfolio("third_EW_top4" ,
                                    top4_third,
                                    crypto_returns_xts,
                                    third_train_period,
                                    third_test_period)


top4_EW_returns <- rbind(first_EW_top4$R,second_EW_top4$R,third_EW_top4$R)

### More Inefficient cryptos


first_EW_bottom4 <- build.EW.portfolio("first_EW_bottom4" ,
                                    bottom4_first,
                                    crypto_returns_xts,
                                    first_train_period,
                                    first_test_period)

second_EW_bottom4 <- build.EW.portfolio("second_EW_bottom4" ,
                                       bottom4_second,
                                       crypto_returns_xts,
                                       second_train_period,
                                       second_test_period)

third_EW_bottom4 <- build.EW.portfolio("third_EW_bottom4" ,
                                       bottom4_third,
                                       crypto_returns_xts,
                                       third_train_period,
                                       third_test_period)



top4_EW_returns <- rbind(first_EW_bottom4$R,second_EW_bottom4$R,third_EW_bottom4$R)


# Inverse Inefficency 



top4_first_weights_InvInef <- format.InvInef.weights.df(first_deltaH,colnames(first_deltaH)[2],top4_first)
top4_second_weights_InvInef <- format.InvInef.weights.df(second_deltaH,colnames(second_deltaH)[2],top4_second)
top4_third_weights_InvInef <- format.InvInef.weights.df(third_deltaH,colnames(third_deltaH)[2],top4_third)

first_InvInef_top4 <- build.inverse.inefficency.strategy("first_InvInef_top4",
                                                    top4_first_weights_InvInef,
                                                    crypto_returns_xts,
                                                    first_train_period,
                                                    first_test_period)

second_InvInef_top4 <- build.inverse.inefficency.strategy("second_InvInef_top4",
                                                         top4_second_weights_InvInef,
                                                         crypto_returns_xts,
                                                         second_train_period,
                                                         second_test_period)

third_InvInef_top4 <- build.inverse.inefficency.strategy("third_InvInef_top4",
                                                         top4_third_weights_InvInef,
                                                         crypto_returns_xts,
                                                         third_train_period,
                                                         third_test_period)



top4_InvInef_returns <- rbind(first_InvInef_top4$R,second_InvInef_top4$R,third_InvInef_top4$R)


bottom4_first_weights_InvInef <- format.InvInef.weights.df(first_deltaH,colnames(first_deltaH)[2],bottom4_first)
bottom4_second_weights_InvInef <- format.InvInef.weights.df(second_deltaH,colnames(second_deltaH)[2],bottom4_second)
bottom4_third_weights_InvInef <- format.InvInef.weights.df(third_deltaH,colnames(third_deltaH)[2],bottom4_third)


first_InvInef_bottom4 <- build.inverse.inefficency.strategy("first_InvInef_bottom4",
                                                         bottom4_first_weights_InvInef,
                                                         crypto_returns_xts,
                                                         first_train_period,
                                                         first_test_period)

second_InvInef_bottom4 <- build.inverse.inefficency.strategy("second_InvInef_bottom4",
                                                          bottom4_second_weights_InvInef,
                                                          crypto_returns_xts,
                                                          second_train_period,
                                                          second_test_period)

third_InvInef_bottom4 <- build.inverse.inefficency.strategy("third_InvInef_bottom4",
                                                         bottom4_third_weights_InvInef,
                                                         crypto_returns_xts,
                                                         third_train_period,
                                                         third_test_period)


bottom4_InvInef_returns <- rbind(first_InvInef_bottom4$R,second_InvInef_bottom4$R,third_InvInef_bottom4$R)





# Results




strategies_4assets <- list(first_MVP_top4,
                       first_MVP_bottom4,
                       first_maxSR_top4,
                       first_maxSR_bottom4,
                       first_EW_top4,
                       first_EW_bottom4,
                       first_InvInef_top4,
                       first_InvInef_bottom4,
                       second_MVP_top4,
                       second_MVP_bottom4,
                       second_maxSR_top4,
                       second_maxSR_bottom4,
                       second_EW_top4,
                       second_EW_bottom4,
                       second_InvInef_top4,
                       second_InvInef_bottom4,
                       third_MVP_top4,
                       third_MVP_bottom4,
                       third_maxSR_top4,
                       third_maxSR_bottom4,
                       third_EW_top4,
                       third_EW_bottom4,
                       third_InvInef_top4,
                       third_InvInef_bottom4)




results2020_4assets <- get.strats.KPIs(strategies_4assets,name_pattern='first',RF = 0,year_file = '2020',folder_name='Results/KPIs/4assets/',export = TRUE)
results2021_4assets <- get.strats.KPIs(strategies_4assets,name_pattern='second',RF = 0,year_file = '2021',folder_name='Results/KPIs/4assets/',export = TRUE)
results2022_4assets <- get.strats.KPIs(strategies_4assets,name_pattern='third',RF = 0,year_file = '2022',folder_name='Results/KPIs/4assets/',export = TRUE)



for(s in strategies_4assets){
  file_name <-paste("Results/Summary_plots/4assets/",s$name,'.png',sep = '')
  png(file_name)
  charts.PerformanceSummary(s$R,main = s$name)
  dev.off()
  
  
}


top4_2020_weights <- get_table_strats_weights(strategies_4assets,"first","top4",folder_path = 'Results/Weights/4assets/',export = TRUE) 
top4_2021_weights <- get_table_strats_weights(strategies_4assets,"second","top4",folder_path = 'Results/Weights/4assets/',export = TRUE)
top4_2022_weights <- get_table_strats_weights(strategies_4assets,"third","top4",folder_path = 'Results/Weights/4assets/',export = TRUE)

bottom4_2020_weights <- get_table_strats_weights(strategies_4assets,"first","bottom4",folder_path = 'Results/Weights/4assets/',export = TRUE) 
bottom4_2021_weights <- get_table_strats_weights(strategies_4assets,"second","bottom4",folder_path = 'Results/Weights/4assets/',export = TRUE) 
bottom4_2022_weights <- get_table_strats_weights(strategies_4assets,"third","bottom4",folder_path = 'Results/Weights/4assets/',export = TRUE) 

#------------------------------------------------------------------------------------------------------------------------------

# Strategies - (short sell restriction) - 8 assets within portfolios



first_MVP_top8 <-  build.portfolio.strats("first_MVP_top8" ,
                                          top8_first,
                                          crypto_returns_xts,
                                          first_train_period,
                                          first_test_period,
                                          pspec.lo.full,
                                          mvp.spec,
                                          neg_to_zero = TRUE)

second_MVP_top8 <-  build.portfolio.strats("second_MVP_top8" ,
                                           top8_second,
                                           crypto_returns_xts,
                                           second_train_period,
                                           second_test_period,
                                           pspec.lo.full,
                                           mvp.spec,
                                           neg_to_zero = TRUE)

third_MVP_top8 <-  build.portfolio.strats("third_MVP_top8" ,
                                          top8_third,
                                          crypto_returns_xts,
                                          third_train_period,
                                          third_test_period,
                                          pspec.lo.full,
                                          mvp.spec,
                                          neg_to_zero = TRUE)



top8_MVP_returns <- rbind(first_MVP_top8$R,second_MVP_top8$R,third_MVP_top8$R)


### More Inefficient cryptos


first_MVP_bottom8 <-  build.portfolio.strats("first_MVP_bottom8" ,
                                             bottom8_first,
                                             crypto_returns_xts,
                                             first_train_period,
                                             first_test_period,
                                             pspec.lo.full,
                                             mvp.spec,
                                             neg_to_zero = TRUE)


second_MVP_bottom8 <-  build.portfolio.strats("second_MVP_bottom8" ,
                                              bottom8_second,
                                              crypto_returns_xts,
                                              second_train_period,
                                              second_test_period,
                                              pspec.lo.full,
                                              mvp.spec,
                                              neg_to_zero = TRUE)


third_MVP_bottom8 <-  build.portfolio.strats("third_MVP_bottom8" ,
                                             bottom8_third,
                                             crypto_returns_xts,
                                             third_train_period,
                                             third_test_period,
                                             pspec.lo.full,
                                             mvp.spec,
                                             neg_to_zero = TRUE)


bottom8_MVP_returns <- rbind(first_MVP_bottom8$R,second_MVP_bottom8$R,third_MVP_bottom8$R)

## Max Sharpe ---->  not diversifying

### More Efficient cryptos
first_maxSR_top8 <-  build.portfolio.strats("first_maxSR_top8" ,
                                            top8_first,
                                            crypto_returns_xts,
                                            first_train_period,
                                            first_test_period,
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


second_maxSR_top8 <-  build.portfolio.strats("second_maxSR_top8" ,
                                             top8_second,
                                             crypto_returns_xts,
                                             second_train_period,
                                             second_test_period,
                                             pspec.lo.full,
                                             tp.sepc,
                                             maxSharp = TRUE,
                                             neg_to_zero = TRUE)


third_maxSR_top8 <-  build.portfolio.strats("third_maxSR_top8" ,
                                            top8_third,
                                            crypto_returns_xts,
                                            third_train_period,
                                            third_test_period,
                                            pspec.lo.full,
                                            tp.sepc,
                                            maxSharp = TRUE,
                                            neg_to_zero = TRUE)


top8_maxSR_returns <- rbind(first_maxSR_top8$R,second_maxSR_top8$R,third_maxSR_top8$R)

### More Inefficient cryptos

first_maxSR_bottom8 <-  build.portfolio.strats("first_maxSR_bottom8" ,
                                               bottom8_first,
                                               crypto_returns_xts,
                                               first_train_period,
                                               first_test_period,
                                               pspec.lo.full,
                                               tp.sepc,
                                               maxSharp = TRUE,
                                               neg_to_zero = TRUE)


second_maxSR_bottom8 <-  build.portfolio.strats("second_maxSR_bottom8" ,
                                                bottom8_second,
                                                crypto_returns_xts,
                                                second_train_period,
                                                second_test_period,
                                                pspec.lo.full,
                                                tp.sepc,
                                                maxSharp = TRUE,
                                                neg_to_zero = TRUE)


third_maxSR_bottom8 <-  build.portfolio.strats("third_maxSR_bottom8" ,
                                               bottom8_third,
                                               crypto_returns_xts,
                                               third_train_period,
                                               third_test_period,
                                               pspec.lo.full,
                                               tp.sepc,
                                               maxSharp = TRUE,
                                               neg_to_zero = TRUE)



top8_maxSR_returns <- rbind(first_maxSR_top8$R,second_maxSR_top8$R,third_maxSR_top8$R)


# EW 

### More Efficient cryptos

first_EW_top8 <- build.EW.portfolio("first_EW_top8" ,
                                    top8_first,
                                    crypto_returns_xts,
                                    first_train_period,
                                    first_test_period)

second_EW_top8 <- build.EW.portfolio("second_EW_top8" ,
                                     top8_second,
                                     crypto_returns_xts,
                                     second_train_period,
                                     second_test_period)

third_EW_top8 <- build.EW.portfolio("third_EW_top8" ,
                                    top8_third,
                                    crypto_returns_xts,
                                    third_train_period,
                                    third_test_period)


top8_EW_returns <- rbind(first_EW_top8$R,second_EW_top8$R,third_EW_top8$R)

### More Inefficient cryptos


first_EW_bottom8 <- build.EW.portfolio("first_EW_bottom8" ,
                                       bottom8_first,
                                       crypto_returns_xts,
                                       first_train_period,
                                       first_test_period)

second_EW_bottom8 <- build.EW.portfolio("second_EW_bottom8" ,
                                        bottom8_second,
                                        crypto_returns_xts,
                                        second_train_period,
                                        second_test_period)

third_EW_bottom8 <- build.EW.portfolio("third_EW_bottom8" ,
                                       bottom8_third,
                                       crypto_returns_xts,
                                       third_train_period,
                                       third_test_period)



top8_EW_returns <- rbind(first_EW_bottom8$R,second_EW_bottom8$R,third_EW_bottom8$R)


# Inverse Inefficency 



top8_first_weights_InvInef <- format.InvInef.weights.df(first_deltaH,colnames(first_deltaH)[2],top8_first)
top8_second_weights_InvInef <- format.InvInef.weights.df(second_deltaH,colnames(second_deltaH)[2],top8_second)
top8_third_weights_InvInef <- format.InvInef.weights.df(third_deltaH,colnames(third_deltaH)[2],top8_third)

first_InvInef_top8 <- build.inverse.inefficency.strategy("first_InvInef_top8",
                                                         top8_first_weights_InvInef,
                                                         crypto_returns_xts,
                                                         first_train_period,
                                                         first_test_period)

second_InvInef_top8 <- build.inverse.inefficency.strategy("second_InvInef_top8",
                                                          top8_second_weights_InvInef,
                                                          crypto_returns_xts,
                                                          second_train_period,
                                                          second_test_period)

third_InvInef_top8 <- build.inverse.inefficency.strategy("third_InvInef_top8",
                                                         top8_third_weights_InvInef,
                                                         crypto_returns_xts,
                                                         third_train_period,
                                                         third_test_period)



top8_InvInef_returns <- rbind(first_InvInef_top8$R,second_InvInef_top8$R,third_InvInef_top8$R)


bottom8_first_weights_InvInef <- format.InvInef.weights.df(first_deltaH,colnames(first_deltaH)[2],bottom8_first)
bottom8_second_weights_InvInef <- format.InvInef.weights.df(second_deltaH,colnames(second_deltaH)[2],bottom8_second)
bottom8_third_weights_InvInef <- format.InvInef.weights.df(third_deltaH,colnames(third_deltaH)[2],bottom8_third)


first_InvInef_bottom8 <- build.inverse.inefficency.strategy("first_InvInef_bottom8",
                                                            bottom8_first_weights_InvInef,
                                                            crypto_returns_xts,
                                                            first_train_period,
                                                            first_test_period)

second_InvInef_bottom8 <- build.inverse.inefficency.strategy("second_InvInef_bottom8",
                                                             bottom8_second_weights_InvInef,
                                                             crypto_returns_xts,
                                                             second_train_period,
                                                             second_test_period)

third_InvInef_bottom8 <- build.inverse.inefficency.strategy("third_InvInef_bottom8",
                                                            bottom8_third_weights_InvInef,
                                                            crypto_returns_xts,
                                                            third_train_period,
                                                            third_test_period)


bottom8_InvInef_returns <- rbind(first_InvInef_bottom8$R,second_InvInef_bottom8$R,third_InvInef_bottom8$R)




#----------------------------------------------------------------------------------------------------------

# Results




strategies_8assets <- list(first_MVP_top8,
                           first_MVP_bottom8,
                           first_maxSR_top8,
                           first_maxSR_bottom8,
                           first_EW_top8,
                           first_EW_bottom8,
                           first_InvInef_top8,
                           first_InvInef_bottom8,
                           second_MVP_top8,
                           second_MVP_bottom8,
                           second_maxSR_top8,
                           second_maxSR_bottom8,
                           second_EW_top8,
                           second_EW_bottom8,
                           second_InvInef_top8,
                           second_InvInef_bottom8,
                           third_MVP_top8,
                           third_MVP_bottom8,
                           third_maxSR_top8,
                           third_maxSR_bottom8,
                           third_EW_top8,
                           third_EW_bottom8,
                           third_InvInef_top8,
                           third_InvInef_bottom8)




results2020_8assets <- get.strats.KPIs(strategies_8assets,name_pattern='first',RF = 0,year_file = '2020',folder_name='Results/KPIs/8assets/',export = TRUE)
results2021_8assets <- get.strats.KPIs(strategies_8assets,name_pattern='second',RF = 0,year_file = '2021',folder_name='Results/KPIs/8assets/',export = TRUE)
results2022_8assets <- get.strats.KPIs(strategies_8assets,name_pattern='third',RF = 0,year_file = '2022',folder_name='Results/KPIs/8assets/',export = TRUE)



for(s in strategies_8assets){
  file_name <-paste("Results/Summary_plots/8assets/",s$name,'.png',sep = '')
  png(file_name)
  charts.PerformanceSummary(s$R,main = s$name)
  dev.off()
  
  
}


top8_2020_weights <- get_table_strats_weights(strategies_8assets,"first","top8",folder_path = 'Results/Weights/8assets/',export = TRUE) 
top8_2021_weights <- get_table_strats_weights(strategies_8assets,"second","top8",folder_path = 'Results/Weights/8assets/',export = TRUE)
top8_2022_weights <- get_table_strats_weights(strategies_8assets,"third","top8",folder_path = 'Results/Weights/8assets/',export = TRUE)

bottom8_2020_weights <- get_table_strats_weights(strategies_8assets,"first","bottom8",folder_path = 'Results/Weights/8assets/',export = TRUE) 
bottom8_2021_weights <- get_table_strats_weights(strategies_8assets,"second","bottom8",folder_path = 'Results/Weights/8assets/',export = TRUE) 
bottom8_2022_weights <- get_table_strats_weights(strategies_8assets,"third","bottom8",folder_path = 'Results/Weights/8assets/',export = TRUE) 


