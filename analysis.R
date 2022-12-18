source("main.R")
source("functions.R")


# Build report tables

all_strategies <- c(strategies_4assets,strategies_8assets)

daily_returns_2020 <- gatther_Rets(all_strategies,name_pattern = 'first') 
daily_returns_2021 <- gatther_Rets(all_strategies,name_pattern = 'second') 
daily_returns_2022 <- gatther_Rets(all_strategies,name_pattern = 'third') 

cumrets_2020 <- daily_returns_2020 %>%  calc_cumrets()
cumrets_2021 <- daily_returns_2021 %>%  calc_cumrets()
cumrets_2022 <- daily_returns_2022 %>%  calc_cumrets()


#exporting files

daily_returns_2020 %>%  write.csv("Results/Series_returns/daily_returns_2020.csv")
daily_returns_2021 %>%  write.csv("Results/Series_returns/daily_returns_2021.csv") 
daily_returns_2022 %>%  write.csv("Results/Series_returns/daily_returns_2022.csv") 

cumrets_2020 %>%  write.csv("Results/Series_returns/cumulative_returns_2020.csv")
cumrets_2021 %>%  write.csv("Results/Series_returns/cumulative_returns_2021.csv")
cumrets_2022 %>%  write.csv("Results/Series_returns/cumulative_returns_2022.csv")



# Comparing  results within 4 cryptocurrencies Portfolios --------------------------------

series_4cryptos_2020 <- gatther_Rets(strategies_4assets,name_pattern = 'first') 
cumrets_4cryptos_2020 <- cumrets_to_longer(series_4cryptos_2020)
plot_cumrets_4cryptos_2020 <- plot_compared_performance(cumrets_4cryptos_2020,"Strategies Perfromance",color_colum = Strat)
agg_cumrets_4cryptos_2020<- plot_compared_performance(cumrets_4cryptos_2020,"Strategies Perfromance")


series_4cryptos_2021 <- gatther_Rets(strategies_4assets,name_pattern = 'second') 
cumrets_4cryptos_2021 <- cumrets_to_longer(series_4cryptos_2021)
plot_cumrets_4cryptos_2021 <- plot_compared_performance(cumrets_4cryptos_2021,"Strategies Perfromance",color_colum = Strat)
agg_cumrets_4cryptos_2021<- plot_compared_performance(cumrets_4cryptos_2021,"Strategies Perfromance")


series_4cryptos_2022 <- gatther_Rets(strategies_4assets,name_pattern = 'third') 
cumrets_4cryptos_2022 <- cumrets_to_longer(series_4cryptos_2022)
plot_cumrets_4cryptos_2022 <- plot_compared_performance(cumrets_4cryptos_2022,"Strategies Perfromance",color_colum = Strat)
agg_cumrets_4cryptos_2022<- plot_compared_performance(cumrets_4cryptos_2022,"Strategies Perfromance")



mvp_4cryptos_2020_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'first_MVP') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Mininimum Variance  with 4 Cryptocurrencies') 
mvp_4cryptos_2020_plot

maxSR_4cryptos_2020_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'first_maxSR') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Maximum Sharp with 4 Cryptocurrencies') 
maxSR_4cryptos_2020_plot



EW_4cryptos_2020_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'first_EW') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Equal Weighted with 4 Cryptocurrencies') 
EW_4cryptos_2020_plot 

invInef_4cryptos_2020_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'first_Inv') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Inverse Inefficiency with 4 Cryptocurrencies') 
invInef_4cryptos_2020_plot



## 2021
mvp_4cryptos_2021_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'second_MVP') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Mininimum Variance  with 4 Cryptocurrencies') 
mvp_4cryptos_2021_plot


maxSR_4cryptos_2021_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'second_maxSR') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Maximum Sharp with 4 Cryptocurrencies') 
maxSR_4cryptos_2021_plot


EW_4cryptos_2021_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'second_EW') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Equal Weighted with 4 Cryptocurrencies') 
EW_4cryptos_2021_plot

invInef_4cryptos_2021_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'second_Inv') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Inverse Inefficiency with 4 Cryptocurrencies') 
invInef_4cryptos_2021_plot 

## 2022
mvp_4cryptos_2022_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'third_MVP') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Mininimum Variance  with 4 Cryptocurrencies') 
mvp_4cryptos_2022_plot


maxSR_4cryptos_2022_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'third_maxSR') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Maximum Sharp with 4 Cryptocurrencies') 
maxSR_4cryptos_2022_plot


EW_4cryptos_2022_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'third_EW') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Equal Weighted with 4 Cryptocurrencies') 
EW_4cryptos_2022_plot

invInef_4cryptos_2022_plot <- strategies_4assets %>% 
  gatther_Rets(name_pattern = 'third_Inv') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Inverse Inefficiency with 4 Cryptocurrencies') 
invInef_4cryptos_2022_plot



# Comparing results within 8 cryptocurrencies Portfolios --------------------------------

series_8cryptos_2020 <- gatther_Rets(strategies_8assets,name_pattern = 'first') 
cumrets_8cryptos_2020 <- cumrets_to_longer(series_8cryptos_2020)
plot_cumrets_8cryptos_2020 <- plot_compared_performance(cumrets_8cryptos_2020,"Strategies Perfromance",color_colum = Strat)
agg_cumrets_8cryptos_2020<- plot_compared_performance(cumrets_8cryptos_2020,"Strategies Perfromance")


series_8cryptos_2021 <- gatther_Rets(strategies_8assets,name_pattern = 'second') 
cumrets_8cryptos_2021 <- cumrets_to_longer(series_8cryptos_2021)
plot_cumrets_8cryptos_2021 <- plot_compared_performance(cumrets_8cryptos_2021,"Strategies Perfromance",color_colum = Strat)
agg_cumrets_8cryptos_2021<- plot_compared_performance(cumrets_8cryptos_2021,"Strategies Perfromance")


series_8cryptos_2022 <- gatther_Rets(strategies_8assets,name_pattern = 'third') 
cumrets_8cryptos_2022 <- cumrets_to_longer(series_8cryptos_2022)
plot_cumrets_8cryptos_2022 <- plot_compared_performance(cumrets_8cryptos_2022,"Strategies Perfromance",color_colum = Strat)
agg_cumrets_8cryptos_2022<- plot_compared_performance(cumrets_8cryptos_2022,"Strategies Perfromance")



mvp_8cryptos_2020_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'first_MVP') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Mininimum Variance  with 8 Cryptocurrencies') 
mvp_8cryptos_2020_plot

maxSR_8cryptos_2020_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'first_maxSR') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Maximum Sharp with 8 Cryptocurrencies') 
maxSR_8cryptos_2020_plot



EW_8cryptos_2020_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'first_EW') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Equal Weighted with 8 Cryptocurrencies') 
EW_8cryptos_2020_plot 

invInef_8cryptos_2020_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'first_Inv') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Inverse Inefficiency with 8 Cryptocurrencies') 
invInef_8cryptos_2020_plot



## 2021
mvp_8cryptos_2021_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'second_MVP') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Mininimum Variance  with 8 Cryptocurrencies') 
mvp_8cryptos_2021_plot


maxSR_8cryptos_2021_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'second_maxSR') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Maximum Sharp with 8 Cryptocurrencies') 
maxSR_8cryptos_2021_plot


EW_8cryptos_2021_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'second_EW') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Equal Weighted with 8 Cryptocurrencies') 
EW_8cryptos_2021_plot

invInef_8cryptos_2021_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'second_Inv') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Inverse Inefficiency with 8 Cryptocurrencies') 
invInef_8cryptos_2021_plot 

## 2022
mvp_8cryptos_2022_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'third_MVP') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Mininimum Variance  with 8 Cryptocurrencies') 
mvp_8cryptos_2022_plot


maxSR_8cryptos_2022_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'third_maxSR') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Maximum Sharp with 8 Cryptocurrencies') 
maxSR_8cryptos_2022_plot


EW_8cryptos_2022_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'third_EW') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Equal Weighted with 8 Cryptocurrencies') 
EW_8cryptos_2022_plot

invInef_8cryptos_2022_plot <- strategies_8assets %>% 
  gatther_Rets(name_pattern = 'third_Inv') %>% 
  cumrets_to_longer() %>%  plot_compared_performance(title_plot = 'Inverse Inefficiency with 8 Cryptocurrencies') 
invInef_8cryptos_2022_plot







# Comparing results of 4 x 8  cryptocurrencies Portfolios  --------------------------------


periods_pattern <- c("first","second","third")
strats_pattern <- c("MVP,maxSR","EW","InvInef")


## All series combined
series_2020 <- gatther_Rets(all_strategies,name_pattern = 'first') 
cumrets_2020 <- cumrets_to_longer(series_2020)
cumrets_2020$port_size <- ifelse(grepl("4",cumrets_2020$Strat),'4crypto','8crypto')

plot_cumrets_2020 <- plot_compared_performance(cumrets_2020,"Strategies Perfromance",color_colum = Strat)

series_2021 <- gatther_Rets(all_strategies,name_pattern = 'second') 
cumrets_2021 <- cumrets_to_longer(series_2021)
cumrets_2021$port_size <- ifelse(grepl("4",cumrets_2021$Strat),'4crypto','8crypto')

plot_cumrets_2021 <- plot_compared_performance(cumrets_2021,"Strategies Perfromance",color_colum = Strat)
#size_agg_cumrets_2020 <- plot_compared_performance(cumrets_2020,"Strategies Perfromance",color_colum = port_size)


series_2022 <- gatther_Rets(all_strategies,name_pattern = 'third') 
cumrets_2022 <- cumrets_to_longer(series_2022)
cumrets_2022$port_size <- ifelse(grepl("4",cumrets_2022$Strat),'4crypto','8crypto')

plot_cumrets_2022 <- plot_compared_performance(cumrets_2022,"Strategies Perfromance",color_colum = Strat)
#size_agg_cumrets_2020 <- plot_compared_performance(cumrets_2020,"Strategies Perfromance",color_colum = port_size)



# Comparing with allocations portfolios of different size  ------------

### TOP


#### 2020
mvp_TOP_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_MVP_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Mininimum Variance  Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/mvp_TOP_2020_plot.png")
mvp_TOP_2020_plot
dev.off()




maxSR_TOP_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_maxSR_top') %>%  #Both allocates amost all in one crytpocurrency (EOS) 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Maximum Sharpe Top 4 x Top 8',color_colum=port_size) 


png("Results/Comparative_Plots/maxSR_TOP_2020_plot.png")
maxSR_TOP_2020_plot
dev.off()



EW_TOP_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_EW_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Equal Weighted Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/EW_TOP_2020_plot.png")
EW_TOP_2020_plot
dev.off()





InvInef_TOP_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_InvInef_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Inverse Inefficiency Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/InvInef_TOP_2020_plot.png")
InvInef_TOP_2020_plot
dev.off()


#### 2021

mvp_TOP_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_MVP_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Mininimum Variance  Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/mvp_TOP_2021_plot.png")
mvp_TOP_2021_plot
dev.off()

maxSR_TOP_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_maxSR_top') %>%   
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Maximum Sharpe Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/maxSR_TOP_2021_plot.png")
maxSR_TOP_2021_plot
dev.off()



EW_TOP_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_EW_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Equal Weighted Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/EW_TOP_2021_plot.png")
EW_TOP_2021_plot
dev.off()



InvInef_TOP_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_InvInef_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Inverse Inefficiency Top 4 x Top 8',color_colum=port_size) 



png("Results/Comparative_Plots/InvInef_TOP_2021_plot.png")
InvInef_TOP_2021_plot
dev.off()

#### 2022

mvp_TOP_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_MVP_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Mininimum Variance  Top 4 x Top 8',color_colum=port_size) 
mvp_TOP_2022_plot

png("Results/Comparative_Plots/mvp_TOP_2022_plot.png")
mvp_TOP_2022_plot
dev.off()



maxSR_TOP_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_maxSR_top') %>%   
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Maximum Sharpe Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/maxSR_TOP_2022_plot.png")
maxSR_TOP_2022_plot
dev.off()



EW_TOP_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_EW_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Equal Weighted Top 4 x Top 8',color_colum=port_size) 

png("Results/Comparative_Plots/EW_TOP_2022_plot.png")
EW_TOP_2022_plot
dev.off()



InvInef_TOP_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_InvInef_top') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Inverse Inefficiency Top 4 x Top 8',color_colum=port_size) 



png("Results/Comparative_Plots/InvInef_TOP_2022_plot.png")
InvInef_TOP_2022_plot
dev.off()



### BOTTOM


#### 2020
mvp_BOTTOM_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_MVP_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Mininimum Variance  Bottom 4 x Bottom 8',color_colum=port_size) 

png("Results/Comparative_Plots/mvp_BOTTOM_2020_plot.png")
mvp_BOTTOM_2020_plot
dev.off()


maxSR_BOTTOM_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_maxSR_bottom') %>%  #Both allocates amost all in one crytpocurrency (EOS) 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Maximum Sharpe Bottom 4 x Bottom 8',color_colum=port_size) 


png("Results/Comparative_Plots/maxSR_BOTTOM_2020_plot.png")
maxSR_BOTTOM_2020_plot
dev.off()




EW_BOTTOM_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_EW_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Equal Weighted Bottom 4 x Bottom 8',color_colum=port_size) 

png("Results/Comparative_Plots/EW_BOTTOM_2020_plot.png")
EW_BOTTOM_2020_plot
dev.off()




InvInef_BOTTOM_2020_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'first_InvInef_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Inverse Inefficiency Bottom 4 x Bottom 8',color_colum=port_size) 

png("Results/Comparative_Plots/InvInef_BOTTOM_2020_plot.png")
InvInef_BOTTOM_2020_plot
dev.off()


#### 2021

mvp_BOTTOM_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_MVP_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Mininimum Variance  Bottom 4 x Bottom 8',color_colum=port_size) 


png("Results/Comparative_Plots/mvp_BOTTOM_2021_plot.png")
mvp_BOTTOM_2021_plot
dev.off()



maxSR_BOTTOM_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_maxSR_bottom') %>%   
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Maximum Sharpe Bottom 4 x Bottom 8',color_colum=port_size) 

png("Results/Comparative_Plots/maxSR_BOTTOM_2021_plot.png")
maxSR_BOTTOM_2021_plot
dev.off()


EW_BOTTOM_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_EW_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Equal Weighted Bottom 4 x Bottom 8',color_colum=port_size) 

png("Results/Comparative_Plots/EW_BOTTOM_2021_plot.png")
EW_BOTTOM_2021_plot
dev.off()



InvInef_BOTTOM_2021_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'second_InvInef_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Inverse Inefficiency Bottom 4 x Bottom 8',color_colum=port_size) 


png("Results/Comparative_Plots/InvInef_BOTTOM_2021_plot.png")
InvInef_BOTTOM_2021_plot
dev.off()




#### 2022

mvp_BOTTOM_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_MVP_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Mininimum Variance  Bottom 4 x Bottom 8',color_colum=port_size) 



png("Results/Comparative_Plots/mvp_BOTTOM_2022_plot.png")
mvp_BOTTOM_2022_plot
dev.off()


maxSR_BOTTOM_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_maxSR_bottom') %>%   
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Maximum Sharpe Bottom 4 x Bottom 8',color_colum=port_size) 

png("Results/Comparative_Plots/maxSR_BOTTOM_2022_plot.png")
maxSR_BOTTOM_2022_plot
dev.off()


EW_BOTTOM_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_EW_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Equal Weighted Bottom 4 x Bottom 8',color_colum=port_size) 

png("Results/Comparative_Plots/EW_BOTTOM_2022_plot.png")
EW_BOTTOM_2022_plot
dev.off()


InvInef_BOTTOM_2022_plot <- all_strategies %>% 
  gatther_Rets(name_pattern = 'third_InvInef_bottom') %>% 
  cumrets_to_longer() %>%
  mutate(port_size=ifelse(grepl("4",Strat),'4crypto','8crypto')) %>%  
  plot_compared_performance(title_plot = 'Inverse Inefficiency Bottom 4 x Bottom 8',color_colum=port_size) 


png("Results/Comparative_Plots/InvInef_BOTTOM_2022_plot.png")
InvInef_BOTTOM_2022_plot
dev.off()



