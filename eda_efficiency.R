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
library(ggrepel)
library(latex2exp)


excel_origin_date = "1899-12-30"

source('load_data.R') # Load all needed data

ts_efficiency <- readxl::read_excel('efficiency_TS.xlsx') %>%  
  mutate(Date %>% as.Date(origin = excel_origin_date))



mktcap_lonformat <- crypto_mktCap_xts['2020/']  %>% 
  fortify.zoo %>% 
  as.tibble() %>%  
  dplyr::rename("Date" = Index ) %>% 
  gather(key = "Crypto", value = "MktCap", -Date )


cumrets_longfromat <- crypto_returns_xts['2020/'] %>%  
  calc_cumrets() %>% 
  fortify.zoo %>% 
  as.tibble() %>%  
  dplyr::rename("Date" = Index ) %>% 
gather(key = "Crypto", value = "Cummreturn", -Date) 


ts_efficiency 



