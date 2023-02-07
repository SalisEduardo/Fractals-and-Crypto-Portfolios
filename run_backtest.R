
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


# Backtest  Crypto Index -----------------------

STRATS_DEOPTM_index <- compleate_backtest_v2(PERIODS,optm = 'DEoptim',portfolio_sizes=c(19))



KPIS_backtest_index <- get.Backtest.KPIs(STRATS_DEOPTM_index)


KPIS_backtest_index <- tibble::rownames_to_column(KPIS_backtest_index, "Strats_names")

KPIS_backtest_index[c("Policy","Efficency_group","N_assets","Test_period")]<- str_split_fixed(KPIS_backtest_index$Strats_names, '_', 4)


# selected_kpis_cols <- c("Strats_names","Policy","Efficency_group","N_assets","Test_period","Total Return","Annualized Return","Annualized Std Dev","Annualized Sharpe (Rf=0%)","Historical VaR (95%)")
# 
# KPIS_backtest_index <- KPIS_backtest_index[selected_kpis_cols]

KPIS_backtest_index  %>%  writexl::write_xlsx("Results/Complete_Backtest/BacktestKPIs_index_iteration.xlsx")

saveRDS(STRATS_DEOPTM_index, file = "STRATS_DEOPTM_index.RDS") 
saveRDS(KPIS_backtest_index, file = "KPIS_backtest_index.RDS")  



# Backtest Portfolio of  4, 6 , 8 Cryptos -------------------------------
STRATS_DEOPTM_468 <- compleate_backtest_v2(PERIODS,optm = 'DEoptim',path_weights="Results/Complete_Backtest/Weights_iteration_v2/")


KPIS_backtest_468  <- get.Backtest.KPIs(STRATS_DEOPTM_468)


KPIS_backtest_468  <- tibble::rownames_to_column(KPIS_backtest_468 , "Strats_names")

KPIS_backtest_468 [c("Policy","Efficency_group","N_assets","Test_period")]<- str_split_fixed(KPIS_backtest_468 $Strats_names, '_', 4)


# selected_kpis_cols <- c("Strats_names","Policy","Efficency_group","N_assets","Test_period","Total Return","Annualized Return","Annualized Std Dev","Annualized Sharpe (Rf=0%)","Historical VaR (95%)")

# KPIS_backtest_468  <- KPIS_backtest_468 [selected_kpis_cols]

KPIS_backtest_468   %>%  writexl::write_xlsx("Results/Complete_Backtest/BacktestKPIs_456.xlsx")



# Backtest Portfolio of 4 Cryptos  --------------------------------------------------------------------------

STRATS_DEOPTM_4 <- compleate_backtest_v2(PERIODS,optm = 'DEoptim',portfolio_sizes = c(4))

KPIS_backtest_4 <- get.Backtest.KPIs(STRATS_DEOPTM_4)

KPIS_backtest_4 <- tibble::rownames_to_column(KPIS_backtest_4, "Strats_names")

KPIS_backtest_4[c("Policy","Efficency_group","N_assets","Test_period")]<- str_split_fixed(KPIS_backtest_4$Strats_names, '_', 4)

KPIS_backtest_4  %>%  writexl::write_xlsx("Results/Complete_Backtest/BacktestKPIs_itearation.xlsx")



saveRDS(STRATS_DEOPTM_4, file = "STRATS_DEOPTM_4.RDS") 
saveRDS(KPIS_backtest_4, file = "KPIS_backtest_4.RDS")  









