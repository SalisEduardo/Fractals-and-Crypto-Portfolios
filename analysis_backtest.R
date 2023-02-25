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

source("functions.R") # set of functions
source("functions_backtest.R") # functions for the backtest
source("risk_free.R") # obtaining risk free data
source('load_data.R') # Load all needed data


policy_keyval <- c("maxSR"="MSR","EW"="EWP","PropInef"="INBP","InvInef"="EBP")

# Results of 4 crypto's Portfolio


STRATS_DEOPTM_4 <- readRDS(file = "STRATS_DEOPTM_4.RDS") 
KPIS_backtest_4 <- readRDS(file = "KPIS_backtest_4.RDS")  
KPIS_backtest_4 <- KPIS_backtest_4 %>% 
  dplyr::mutate(`Annualized Std Dev` = `Annualized Std Dev` * sqrt(360/252)) %>% # Annualize by 360
  dplyr::mutate(Efficiency_group = ifelse(KPIS_backtest_4$Efficiency_group=="most","more",KPIS_backtest_4$Efficiency_group)) %>% 
  dplyr::mutate(Policy = ifelse(KPIS_backtest_4$Policy %in% names(policy_keyval), policy_keyval[KPIS_backtest_4$Policy], KPIS_backtest_4$Policy))



STRATS_DEOPTM_index <- readRDS(file = "STRATS_DEOPTM_index.RDS") 
KPIS_backtest_index <- readRDS(file = "KPIS_backtest_index.RDS") 
KPIS_backtest_index <- KPIS_backtest_index %>% 
  dplyr::rename("Efficiency_group"=Efficency_group) %>% #spelling mistake
  dplyr::mutate(`Annualized Std Dev` = `Annualized Std Dev` * sqrt(360/252)) %>% # Annualize by 360
  dplyr::mutate(Policy = ifelse(KPIS_backtest_index$Policy %in% names(policy_keyval), policy_keyval[KPIS_backtest_index$Policy], KPIS_backtest_index$Policy)) %>% 
  dplyr::mutate(Efficiency_group = ifelse(Efficiency_group == "index", "all",Efficiency_group))


ALL_KPI <- rbind(KPIS_backtest_4,KPIS_backtest_index)
return_sharp_colplot <- function(kpi,year){
  plt <- kpi %>%  
    filter(Test_period == year) %>% 
    ggplot(aes(x=Policy,y=`Annualized Return`,fill=Efficiency_group)) +
    geom_col(position ="dodge") + 
    geom_point(aes(y=`Annualized Sharpe (Rf=0%)`,x=Policy,fill=Efficiency_group),position = position_dodge(width = 0.9)) + 
    geom_text(aes(label = round(`Annualized Sharpe (Rf=0%)`, 2), y = `Annualized Sharpe (Rf=0%)`,color=Efficiency_group), position = position_dodge(width = 0.9), vjust = -0.5) +
    geom_text(aes(label = round(`Annualized Return`, 2), y = `Annualized Return`),color='black', position = position_dodge(width = 0.9), vjust = 1) +
    scale_x_discrete(limits = unique(KPIS_backtest_4$Policy)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Annualized Sharpe (Rf=0%)")) +
    labs(x='Allocation Policy',y="Annualized Return",title = paste(year,"Return and Sharp Ration across portfolios")) +
    theme_classic()
  
  plt
  
}


# return_sharp_colplot(KPIS_backtest_4,year = '2020')
# return_sharp_colplot(KPIS_backtest_4,year = '2021')
# return_sharp_colplot(KPIS_backtest_4,year = '2022')


scatterplot_risk_return <- function(kpi,year,n_assets = NULL,folder_path="Results/Plots_Backtest/"){
  if(is.null(n_assets)){
    n_assets <- unique(kpi$N_assets)[1] 
  }
  
  
  file_plt = paste(folder_path,"Portfolio_",as.character(n_assets),"_cryptos_",as.character(year),".png",sep='') 
  plt <- kpi %>% 
    filter(Test_period == year) %>% 
    ggplot() + 
    geom_point(aes(x = `Annualized Std Dev`,
                   y=`Annualized Return`,
                   color=Efficiency_group,
                   shape = Efficiency_group,
                   size= `Annualized Sharpe (Rf=0%)`)) +
    scale_size_continuous(guide = "none") +
    geom_vline(xintercept =  mean(KPIS_backtest_4[KPIS_backtest_4$Test_period == year,"Annualized Std Dev"]),size = 1.5, linetype = "dashed",color = "darkblue")+
    
    geom_hline(yintercept =  mean(KPIS_backtest_4[KPIS_backtest_4$Test_period == year,"Annualized Return"]),size = 1.5, linetype = "dashed",color = "darkblue")+
 
    geom_text_repel(aes(x = `Annualized Std Dev`,
                        y=`Annualized Return`,
                        color=Efficiency_group,
                        label=Policy),
                    hjust=0, vjust=0,show.legend = FALSE) + 
    xlab(latex2exp::TeX("$\\sigma^A_p$")) + 
    ylab(latex2exp::TeX("$r^A_p$")) + 
  
    labs(title = "",color="Efficiency Level",shape="Efficiency Level",size=latex2exp::TeX("$SR_p$")) + 
    theme_bw()+  
    guides(shape = guide_legend(override.aes = list(size = 8)),
           color = guide_legend(override.aes = list(size = 4)))  + 
    theme(legend.title = element_text(size = 20), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(2, "lines"),
          axis.line = element_line(size = 2),
          axis.text=element_text(size=17.5),
          axis.title=element_text(size=20,face="bold"))
  
  ggsave(file_plt,plt)
  plt
}



scatterplot_risk_return(KPIS_backtest_4,year = '2020') 
scatterplot_risk_return(KPIS_backtest_4,year = '2021')
scatterplot_risk_return(KPIS_backtest_4,year = '2022')

scatterplot_risk_return(KPIS_backtest_index,year = '2020')
scatterplot_risk_return(KPIS_backtest_index,year = '2021')
scatterplot_risk_return(KPIS_backtest_index,year = '2022')

scatterplot_risk_return(ALL_KPI,year = '2020',n_assets = "All")
scatterplot_risk_return(ALL_KPI,year = '2021',n_assets = "All")
scatterplot_risk_return(ALL_KPI,year = '2022',n_assets = "All")


# Performance Time Series
dailyrets_4cryptos2020 <- gatther_Rets(STRATS_DEOPTM_4,name_pattern = '2020')
cumrets2020_4cryptos <- cumrets_to_longer(dailyrets_4cryptos2020,create_group = FALSE)
cumrets2020_4cryptos[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(cumrets2020_4cryptos$Strat, '_', 4)

dailyrets_4cryptos2021 <- gatther_Rets(STRATS_DEOPTM_4,name_pattern = '2021')
cumrets2021_4cryptos <- cumrets_to_longer(dailyrets_4cryptos2021,create_group = FALSE)
cumrets2021_4cryptos[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(cumrets2021_4cryptos$Strat, '_', 4)

dailyrets_4cryptos2022 <- gatther_Rets(STRATS_DEOPTM_4,name_pattern = '2022')
cumrets2022_4cryptos <- cumrets_to_longer(dailyrets_4cryptos2022,create_group = FALSE)
cumrets2022_4cryptos[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(cumrets2022_4cryptos$Strat, '_', 4)



dailyrets_index2020 <- gatther_Rets(STRATS_DEOPTM_index,name_pattern = '2020')
cumrets2020_index <- cumrets_to_longer(dailyrets_index2020,create_group = FALSE)
cumrets2020_index[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(cumrets2020_index$Strat, '_', 4)

dailyrets_index2021 <- gatther_Rets(STRATS_DEOPTM_index,name_pattern = '2021')
cumrets2021_index <- cumrets_to_longer(dailyrets_index2021,create_group = FALSE)
cumrets2021_index[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(cumrets2021_index$Strat, '_', 4)

dailyrets_index2022 <- gatther_Rets(STRATS_DEOPTM_index,name_pattern = '2022')
cumrets2022_index <- cumrets_to_longer(dailyrets_index2022,create_group = FALSE)
cumrets2022_index[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(cumrets2022_index$Strat, '_', 4)


consolidate_daily <- function(list_dailyrets){
  for(i in 1:length(list_dailyrets)){
    colnames(list_dailyrets[[i]]) <- sapply(colnames(list_dailyrets[[i]])  , function(x) substr(x, 1, nchar(x) - 5))
  }
  
  consolidated <- do.call(rbind, list_dailyrets)
  colnames(consolidated)[1] <- "Date"
  return(consolidated)

}

all_dailyrets_4cryptos <- consolidate_daily(list(dailyrets_4cryptos2020,dailyrets_4cryptos2021,dailyrets_4cryptos2022))
all_cumrets_4cryptos <- cumrets_to_longer(all_dailyrets_4cryptos,create_group = FALSE)
all_cumrets_4cryptos[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(all_cumrets_4cryptos$Strat, '_', 4)

all_dailyrets_index <- consolidate_daily(list(dailyrets_index2020,dailyrets_index2021,dailyrets_index2022))
all_cumrets_index <- cumrets_to_longer(all_dailyrets_index,create_group = FALSE)
all_cumrets_index[c("Policy","Efficiency_group","N_assets","Test_period")]<- str_split_fixed(all_cumrets_index$Strat, '_', 4)


ALL_cumrets_2020 <- rbind(cumrets2020_index,cumrets2020_4cryptos) %>%  arrange(Strat,Date)
ALL_cumrets_2021 <- rbind(cumrets2021_index,cumrets2021_4cryptos) %>%  arrange(Strat,Date)
ALL_cumrets_2022 <- rbind(cumrets2022_index,cumrets2022_4cryptos) %>%  arrange(Strat,Date)

ALL_periods_strategies_cumrets <- rbind(ALL_cumrets_2020,ALL_cumrets_2021,ALL_cumrets_2022) %>% 
  dplyr::mutate(Efficiency_group = ifelse(Efficiency_group == "index", "all",Efficiency_group))


plot_all_ts <- function(df_cumrets){
  year_plt <- unique(df_cumrets$Test_period)[1] # assuming just one period
  
  plt1 <- df_cumrets %>%  ggplot(aes(x=Date,y=return)) +
    geom_line(aes(color = Strat),size = 1,alpha=0.68) +
    labs(x="",y='Cummulative Returns',title = paste("All strategies performance comparison", year_plt))+
    theme_classic()
  
  plt2 <- df_cumrets %>%  ggplot(aes(x=Date,y=return)) +
    geom_line(aes( color = Efficiency_group), size = 1,alpha=0.68) +
    labs(x="",y='Cummulative Returns',title = paste("Aggregated performance by efficiency group",year_plt))+
    theme_classic()
  
  
  plt_list <- list(plt1,plt2)
  
  portfolios_groups <- unique(df_cumrets$Policy)
  efficiency_group <- unique(df_cumrets$Efficiency_group)
  
  i <- 3
  for(policy in portfolios_groups){
    
    plt_policy <- df_cumrets %>%  
      filter(Policy == policy) %>% 
      ggplot(aes(x=Date,y=return)) +
      geom_line(aes( color = Efficiency_group), size = 1,alpha=0.68) +
      labs(x="",y='Cummulative Returns',title = paste("Performance of ", policy," by efficiency group",year_plt))+
      theme_classic()
    plt_list[[i]] <- plt_policy
    i <- i + 1
  }
  
  for(e in efficiency_group){
    
    plt_gp <- df_cumrets %>%  
      filter(Efficiency_group == e) %>% 
      ggplot(aes(x=Date,y=return)) +
      geom_line(aes(color = Policy), size = 1,alpha=0.68) +
      labs(x="",y='Cummulative Returns',title = paste("Performance between",e, "efficient Cryptocurrencies -",year_plt))+
      theme_classic()
    plt_list[[i]] <- plt_gp
    i <- i + 1
  }
  
  return(plt_list)
  
}


plot_all_ts_v2 <- function(df_cumrets,folder_path="Results/Plots_Backtest/"){
  #year_plt <- unique(df_cumrets$Test_period)[1] # assuming just one period
  portfolios_groups <- unique(df_cumrets$Policy)
  plt_list <- list()
  i <- 1
  for(p in portfolios_groups){
  
    portfolios_plt_list <- list()
    k <- 1
    
    for (y in c("2020","2021","2022")) {
      plt <- df_cumrets %>%  
        filter(Policy == p,Test_period == y) %>% 
        ggplot(aes(x=Date,y=return)) +
        geom_line(aes(color = Efficiency_group,linetype = Efficiency_group), size = 1,alpha=0.68) +
        ylab(latex2exp::TeX("$r^C_p$"))+
        labs(x="",color='',linetype = '',title = y)+
        theme_classic() + 
         
        theme(legend.title = element_text(size = 15), 
              legend.text = element_text(size = 15),
              legend.key.size = unit(2, "lines"),
              axis.line = element_line(size = 1.5),
              axis.text=element_text(size=15),
              axis.title=element_text(size=15,face="bold"))
      
      portfolios_plt_list[[k]] <- plt
      k <- k + 1 
      
    }
    portfolios_plt <- ggpubr::ggarrange(plotlist=portfolios_plt_list,ncol=1,nrow = 3,common.legend = TRUE) 
    file_plt <-paste(folder_path,p,".png",sep='')
    ggsave(file_plt,portfolios_plt)
    print(portfolios_plt)
    plt_list[[i]] <- portfolios_plt
    i <- i + 1 
  }
  
  return(plt_list)
  
}


plot_all_ts_v2(ALL_periods_strategies_cumrets)

# plot_all_ts(all_cumrets_4cryptos)
# plot_all_ts(cumrets2020_4cryptos)
# plot_all_ts(cumrets2021_4cryptos)
# plot_all_ts(cumrets2022_4cryptos)


