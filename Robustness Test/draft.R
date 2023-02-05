
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
source("functions_backtest.R") # functions for the backtest
source("risk_free.R") # obtaining risk free data
source('load_data.R') # Load all needed data


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




## Portfolios build with 4 crypto currencies

psize = 4

mostN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = psize) #top ranks gets a positive sign
lessN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = - psize) #bottom ranks gets a positive sign

mostN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = psize)
lessN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = - psize)

mostN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = psize)
lessN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = - psize)

status_efficiency ="more"
train = PERIODS[[1]]$train
test = PERIODS[[1]]$test
asset_names = mostN_first
opt = 'DEoptim'
n_assets = length(asset_names)


i=0
df_weights <- data.frame()
while(i<100){
  specs <-asset_names %>%

    pspec.box.relaxed() %>%
    tp.sepc()

  optm_port <- optimize.portfolio(crypto_returns_xts[train,asset_names] ,
                                  specs,maxSR=TRUE,optimize_method = "DEoptim",trace=FALSE)

  print(extractWeights(optm_port))
  w <- extractWeights(optm_port) %>%  as.data.frame() %>%  t() %>%  as.data.frame()
  rownames(w) <- NULL
  
  df_weights <-  rbind(df_weights,w)
  i=i+1
}

view(df_weights)

df_weights %>%  writexl::write_xlsx("Test_weights.xlsx")

Return.portfolio(crypto_returns_xts[test,asset_names],weights = colMeans(w))


# specs <-asset_names %>%  
#   portfolio.spec() %>% 
#   
#   add.constraint(type = "box", min=0.01, max=0.99) %>% 
#   add.constraint(type = "full_investment") %>% 
#   add.objective(type = "risk", name = "StdDev") %>% 
#   add.objective(type = 'return',name='mean')
# 
# optm_port <- optimize.portfolio(crypto_returns_xts[train,asset_names] , 
#                                 specs,maxSR=TRUE,optimize_method = "DEoptim",trace=FALSE,search_size = 100000)
# 
# 
# extractWeights(optm_port)


# specs <-asset_names %>%  
#   
#   pspec.box.relaxed() %>% 
#   tp.sepc()
# 
# optm_port <- optimize.portfolio(crypto_returns_xts[train,asset_names] , 
#                                 specs,maxSR=TRUE,optimize_method = "DEoptim",trace=FALSE)
# 
# extractWeights(optm_port)

