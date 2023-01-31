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






STRATS_DEOPTM_468 <- compleate_backtest(PERIODS,optm = 'DEoptim')



STRATS_DEOPTM_468[[45]]$name 
STRATS_DEOPTM_468[[45]]$w


STRATS_DEOPTM_468[[46]]$name 
STRATS_DEOPTM_468[[46]]$w 



KPIS_backtest <- get.Backtest.KPIs(STRATS_DEOPTM_468)


KPIS_backtest <- tibble::rownames_to_column(KPIS_backtest, "Strats_names")

KPIS_backtest[c("Policy","Efficency_group","N_assets","Test_period")]<- str_split_fixed(KPIS_backtest$Strats_names, '_', 4)


selected_kpis_cols <- c("Strats_names","Policy","Efficency_group","N_assets","Test_period","Total Return","Annualized Return","Annualized Std Dev","Annualized Sharpe (Rf=0%)","Historical VaR (95%)")

KPIS_backtest <- KPIS_backtest[selected_kpis_cols]

KPIS_backtest  %>%  writexl::write_xlsx("Results/Complete_Backtest/BacktestKPIs.xlsx")


