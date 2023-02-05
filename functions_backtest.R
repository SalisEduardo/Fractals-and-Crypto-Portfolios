
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


# Modifyed functions
execute_defaultpolicies_v2 <- function(opt,asset_names,train,test,status_efficiency ="most", n_loops=20,folder_w= "Results/Complete_Backtest/Weights_iteration/"){
  # Specs
  periodDeltaH <- get_deltaH(train)
  
  n_assets <- length(asset_names)
  
  portfolio_specs  <-  asset_names %>% portfolio.spec(assets = asset_names) %>% 
    add.constraint(type = "box",min=0.01, max=0.99) %>% 
    add.constraint(type = "full_investment") 
  
  #MVP
  mvp_specs <- portfolio_specs %>%  add.objective(type = 'risk',name = 'StdDev', risk_aversion=9999)
  
  #Iteration To get MVP weights
  iterator_mvp = 0
  table_mvp_weights <- data.frame()
  while (iterator_mvp <= n_loops ) {
    
    
    
    MVP_opt <- optimize.portfolio(crypto_returns_xts[train,asset_names] , 
                                  mvp_specs,
                                  optimize_method = opt,
                                  trace=FALSE,search_size = 700000 )
    
    iteration_mvp_weights <- extractWeights(MVP_opt) 
    iteration_mvp_weights <- ifelse(iteration_mvp_weights < 0 , 0 ,iteration_mvp_weights)
    iteration_mvp_weights<- iteration_mvp_weights %>% as.data.frame() %>%  t() %>%  as.data.frame()
    rownames(iteration_mvp_weights) <- NULL
    
    
    table_mvp_weights <-  rbind(table_mvp_weights,iteration_mvp_weights)
    
    iterator_mvp = iterator_mvp + 1
    
  }
  
  
  mvp_weights <- colMeans(table_mvp_weights)
  
  
  
  MVP <- list(
    name = paste("MVP",status_efficiency,as.character(n_assets),test,sep='_'),
    p = mvp_specs,
    train = train,
    test = test,
    opt = MVP_opt,
    w  =  mvp_weights %>%  as.data.frame() %>% t() %>% as.data.frame(), #transform to dataframe to export latter
    R = Return.portfolio(crypto_returns_xts[test,asset_names],
                         weights = mvp_weights)
  )
  
  
  
  
  #maxSR
  
  
  #Iteration To get MVP weights
  
  
  iterator_maxSR = 0
  table_maxsr_weights <- data.frame()
  maxSR_specs <- portfolio_specs %>%   
    add.objective(type = "risk", name = "StdDev") %>%
    add.objective(type = "return", name = "mean")
  
  while (iterator_maxSR  <= n_loops) {
    maxSR_opt <- optimize.portfolio(crypto_returns_xts[train,asset_names] , 
                                    maxSR_specs,
                                    optimize_method = opt,
                                    maxSR=TRUE,
                                    trace=FALSE,search_size = 700000)
    
    
    
    iteration_maxsr_weights <- extractWeights(maxSR_opt)
    iteration_maxsr_weights <- ifelse(iteration_maxsr_weights < 0, 0, iteration_maxsr_weights)
    iteration_maxsr_weights <- iteration_maxsr_weights %>%  as.data.frame() %>% t() %>%  as.data.frame()
    
    rownames(iteration_maxsr_weights) <- NULL
    
    table_maxsr_weights <-  rbind(table_maxsr_weights,iteration_maxsr_weights)
    
    iterator_maxSR = iterator_maxSR + 1
    
  }
  
  maxSR_weights <- colMeans(table_maxsr_weights)
  
  maxSR <- list(
    name = paste("maxSR",status_efficiency,as.character(n_assets),test,sep='_'),
    p = portfolio_specs,
    train = train,
    test = test,
    opt = maxSR_opt,
    w  =  maxSR_weights %>% t() %>% as.data.frame(), #transform to dataframe to export latter
    R = Return.portfolio(crypto_returns_xts[test,asset_names],
                         weights = maxSR_weights)
  )
  
  #Equal weight
  EW <- build.EW.portfolio( paste("EW",status_efficiency,as.character(n_assets),test,sep='_'),
                            asset_names,
                            crypto_returns_xts,
                            train,
                            test)
  
  #Inverse Inefficiency
  weights_InvInef <- format.InvInef.weights.df(periodDeltaH,colnames(periodDeltaH)[2],asset_names)
  
  InvInef <- build.inverse.inefficency.strategy(paste("InvInef",status_efficiency,as.character(n_assets),test,sep='_'),
                                                weights_InvInef,
                                                crypto_returns_xts,
                                                train,
                                                test)
  
  #PropInef
  weights_PropInef <- format.PropInef.weights.df(periodDeltaH,colnames(periodDeltaH)[2],asset_names)
  
  PropInef <- build.prop.inefficency.strategy(paste("PropInef",status_efficiency,as.character(n_assets),test,sep='_'),
                                              weights_PropInef,
                                              crypto_returns_xts,
                                              train,
                                              test)
  
  #Resulrs
  strats <- list(MVP,maxSR,EW,InvInef,PropInef)
  
  weights_Tab_name <- paste(status_efficiency,as.character(n_assets),test,sep='_')
  
  weights <- get_strats_weights(strategies_list = strats,group_name  = weights_Tab_name,folder_path=folder_w,export=TRUE)
  
  return(strats)
  
}


compleate_backtest_v2 <- function(periods_keyval,optm='DEoptim',portfolio_sizes=c(4,6,8),path_weights="Results/Complete_Backtest/Weights_iteration/"){
  
  strategies_backtest <- list()
  
  
  for(psize in portfolio_sizes){
    if(psize < length(crypto_names)){
      ## Portfolios build with 4 crypto currencies
      mostN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = psize) #top ranks gets a positive sign
      lessN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = - psize) #bottom ranks gets a positive sign
      
      mostN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = psize)
      lessN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = - psize)
      
      mostN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = psize)
      lessN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = - psize)
      
      all_mostN <- list(mostN_first,mostN_second,mostN_third)
      all_lessN <- list(lessN_first,lessN_second,lessN_third)
      
      
      mostN_first_strats <-execute_defaultpolicies_v2(opt=optm,
                                                      asset_names=mostN_first,
                                                      train=first_train_period,
                                                      test=first_test_period,
                                                      status_efficiency ="most")
      
      mostN_second_strats <-execute_defaultpolicies_v2(opt=optm,
                                                       asset_names=mostN_second,
                                                       train=second_train_period,
                                                       test=second_test_period,
                                                       status_efficiency ="most")
      mostN_third_strats <-execute_defaultpolicies_v2(opt=optm,asset_names=mostN_third,train=third_train_period,test=third_test_period,status_efficiency ="most")
      
      lessN_first_strats <-execute_defaultpolicies_v2(opt=optm,asset_names=lessN_first,train=first_train_period,test=first_test_period,status_efficiency ="less")
      lessN_second_strats <-execute_defaultpolicies_v2(opt=optm,asset_names=lessN_second,train=second_train_period,test=second_test_period,status_efficiency ="less")
      lessN_third_strats <-execute_defaultpolicies_v2(opt=optm,asset_names=lessN_third,train=third_train_period,test=third_test_period,status_efficiency ="less")
      
      
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
    else if(psize == length(crypto_names)){
      
      first_index <-execute_defaultpolicies_v2(opt=optm,
                                               asset_names=crypto_names,
                                               train=first_train_period,
                                               test=first_test_period,
                                               status_efficiency ="index")
      
      second_index <- execute_defaultpolicies_v2(opt=optm,
                                                 asset_names=crypto_names,
                                                 train=second_train_period,
                                                 test=second_test_period,
                                                 status_efficiency ="index")
      
      third_index <-execute_defaultpolicies_v2(opt=optm,
                                               asset_names=crypto_names,
                                               train=third_train_period,
                                               test=third_test_period,
                                               status_efficiency ="index")
      for(s in 1:length(first_index)){
        strategies_backtest <- append(strategies_backtest,first_index[s])
      }
      for(s in 1:length(second_index)){
        strategies_backtest <- append(strategies_backtest,second_index[s])
      }
      for(s in 1:length(third_index)){
        strategies_backtest <- append(strategies_backtest,third_index[s])
      }
      
      
    } 
    else{
      print("Invalid Number of assets")
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
      table.DownsideRisk(s$R,MAR = 0, p=0.95),
      table.Distributions(s$R)
      
    )
    colnames(df_KPIS_strat) <- c(s$name)
    df_KPIS_strat <- df_KPIS_strat %>% t() %>% as.data.frame() 
    
    df_KPIS <- rbind(df_KPIS , df_KPIS_strat)
  }
  
  
  return(df_KPIS)
  
}



