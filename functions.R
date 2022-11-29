
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(RiskPortfolios)
library(covRobust)
library(quantmod)
library(tidyverse)
library(dplyr)
library(purrr)
library(ggplot2)
library(readxl)
library(MFDFA)
library(readxl)
library(plotly)
library(nonlinearTseries)
library(bizdays)
library(plyr)
library(ggdark)
#Specs---------------------------------------------------------------------------------------------------


## Long only , full investment
pspec.lo.full <- function(assets_names){
  p_spec <- portfolio.spec(assets = assets_names) %>% 
    add.constraint(type = "full_investment") %>% 
    add.constraint(type = "long_only")
  return(p_spec)
}

## Box
pspec.box.full <- function(assets_names, min_box=0,max_box=1){
  p_spec <- portfolio.spec(assets = assets_names) %>% 
    add.constraint(type = "full_investment") %>%
    add.constraint(type = "box", min=min_box, max=max_box)
}


#Allocation---------------------------------------------------------------------------------------------------



### Minimum variance portfolio

mvp.spec <- function(p){
  p_MVP <- p %>% 
    #add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'StdDev', risk_aversion=9999)
  return(p_MVP)
}

### Tangency portfolio - (add maxSR=TRUE in the optimization to get max Sharp)

tp.sepc <- function(p){
  tp <- p %>%
    add.objective(type = "risk", name = "StdDev") %>%
    add.objective(type = "return", name = "mean")
  return(tp)
}


### Max return portfolio
maxRet.spec <- function(p){
  p_maxRet <- p %>%
    add.objective(type = "return", name = "mean")
  return(p_maxRet)
}

maxMV.util.specs <- function(p,lambda){
  p_maxMV <-p %>% 
    add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'var',risk_aversion=lambda)
  return(p_maxMV)
}

minCVaR.spec <- function(p,args=list(p=0.95,clean="boudt",method='historical'),cteCOV= FALSE){
  p_CVaR <- p %>%
    add.objective(type = "risk",name = "ES",arguments = args,enabled=TRUE)
  return(p_CVaR)
  
}




#Strats implementation---------------------------------------------------------------------------------------------------


build.portfolio.strats <- function(strats_name,assets_names,return_series,train_period,test_period,base_specs,strats_specs,optimizor='ROI',maxSharp=FALSE,neg_to_zero=FALSE){
  portfolio_specs <- assets_names %>% base_specs %>% strats_specs
  
  if(maxSharp){
    optimize_train <- optimize.portfolio(return_series[train_period,assets_names] , 
                                         portfolio_specs, 
                                         maxSR=TRUE,
                                         optimize_method = optimizor,
                                         trace=TRUE)
  }else{
    optimize_train <- optimize.portfolio(return_series[train_period,assets_names] , 
                                         portfolio_specs,
                                         optimize_method = optimizor,
                                         trace=TRUE)
  }
  
  
  
  portfolio_weights <- extractWeights(optimize_train) 
  
  if(neg_to_zero){
    portfolio_weights <- ifelse(portfolio_weights < 0, 0, portfolio_weights)
  }
  
  Returns_strategy <- Return.portfolio(return_series[test_period,assets_names],
                                       weights = portfolio_weights)
  
  portfolio_weights <- portfolio_weights %>% as.data.frame() %>% t() %>% as.data.frame()
  #rownames(portfolio_weights) <- strats_name
  
  return(list(
    name = strats_name,
    p = portfolio_specs,
    train = train_period,
    test = test_period,
    opt = optimize_train,
    w  =  portfolio_weights ,
    R = Returns_strategy
  ))
}

build.EW.portfolio <- function(strats_name,assets_names,return_series,train_period,test_period){
  portfolio_weights <- rep(1/length(assets_names),length(assets_names))
  Returns_strategy <- Return.portfolio(return_series[test_period,assets_names],
                                       weights = portfolio_weights)
  return(list(
    name = strats_name,
    assets = assets_names  ,
    train = train_period,
    test = test_period,
    w  =  portfolio_weights,
    R = Returns_strategy
  ))
  
}

calc.inverse.inefficiency.weights <- function(fractality,axis=0){
  if(axis==0){
    inverse <- (1/fractality)
    sum_inverse <- sum(inverse)
    weights <- inverse/sum_inverse
    
  }else if(axis==1){
    inverse <- (1/fractality)
    sum_inverse <- rowSums(inverse)
    weights <- inverse/sum_inverse
  }

  
  return(weights)
  
}

format.InvInef.weights.df <- function(assets_fract,fracl_column,selected_assets_names){
  assets_fract <- assets_fract[fracl_column] %>%  t() %>%  as.data.frame()
  colnames(assets_fract) <- selected_assets_names
  assets_fract <- assets_fract[selected_assets_names]
  rownames(assets_fract) <- NULL
  assets_fract <- assets_fract %>% calc.inverse.inefficiency.weights(axis = 1)
  return(assets_fract)
}

build.inverse.inefficency.strategy <- function(strats_name,initial_weights,return_series,train_period,test_period){
  
  R <- return_series[test_period,colnames(initial_weights)] #Ordering the columns and selecting the test period
  weights_vec <- as.numeric(initial_weights)
  
  InvInef_returns <- Return.portfolio(R,weights = weights_vec)
  
  #rownames(initial_weights) <- strats_name
  
  return(list(
    name = strats_name,
    train = train_period,
    assets = colnames(initial_weights),
    test = test_period,
    w  =  initial_weights,
    R = InvInef_returns
  ))
  
}





#Results and KPIS functions------------------------------------------------------------------

table.modigliani <- function(R,period,riskfree,start_date = "2019-01-01",end_date = "2022-09-29"){
  sp500 <- quantmod::getSymbols("^GSPC", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date)) %>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(sp500) <- c("Returns")
  
  nasdaq100 <- quantmod::getSymbols("^NDX", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date))%>% 
    Ad() %>% 
    Return.calculate() %>% 
    na.omit()  
  
  colnames(nasdaq100) <- c("Returns")
  
  russel1000 <- quantmod::getSymbols("^RUI", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date))%>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(russel1000) <- c("Returns")
  
  
  ibrx50 <- quantmod::getSymbols("^IBX50", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date)) %>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(ibrx50) <- c("Returns")
  
  
  ibov <- quantmod::getSymbols("^BVSP", auto.assign = FALSE, from = as.Date(start_date), to = as.Date(end_date))%>%
    Ad() %>%
    Return.calculate() %>%
    na.omit()
  
  colnames(ibov) <- c("Returns")
  
  modig_nasdaq100 <- data.frame(portfolio.returns = Modigliani(R,nasdaq100[period],Rf=riskfree))
  rownames(modig_nasdaq100) <- paste("Modigliani-Nasdaq100 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_sp500 <- data.frame(portfolio.returns = Modigliani(R,sp500[period],Rf=riskfree))
  rownames(modig_sp500) <- paste("Modigliani-SP500 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_russel1000 <- data.frame(portfolio.returns = Modigliani(R,russel1000[period],Rf=riskfree))
  rownames(modig_russel1000) <- paste("Modigliani-Russel1000 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_ibrx50 <- data.frame(portfolio.returns = Modigliani(R,ibrx50[period],Rf=riskfree))
  rownames(modig_ibrx50) <- paste("Modigliani-IBrX50 (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  modig_ibov <- data.frame(portfolio.returns = Modigliani(R,ibov[period],Rf=riskfree))
  rownames(modig_ibov) <- paste("Modigliani-Ibovespa (Rf=",as.character(round(mean(riskfree),4)),")",sep = '')
  
  df<- rbind(modig_sp500,
             modig_nasdaq100,
             modig_russel1000,
             modig_ibrx50,
             modig_ibov)
  return(df)
  
}

get.strats.KPIs <- function(strategies_list,name_pattern,RF, year_file,folder_name='KPIs',mar= 0,prob=0.95,export=FALSE){
  df_returns_all <- data.frame()
  df_dist_all <- data.frame()
  df_DR_all <- data.frame()
  df_DR_ratio_all <- data.frame()
  df_DD_all <- data.frame()
  df_DD_ratio_all <- data.frame()
  modig_all <- data.frame()
  
  for (s in strategies_list){
    if(grepl(name_pattern,s$name)){
      print(s$name)
      
      df_returns <- table.AnnualizedReturns(s$R,Rf = RF)
      colnames(df_returns) <- s$name
      df_returns <- t(df_returns)
      df_returns_all <- rbind(df_returns_all,df_returns)
      
      df_dist <- table.Distributions(s$R)
      colnames(df_dist) <- s$name
      df_dist <- t(df_dist)
      df_dist_all <- rbind(df_dist_all,df_dist)
      
      df_DR <- table.DownsideRisk(s$R,MAR = 0, p=0.95)
      colnames(df_DR) <- s$name
      df_DR <- t(df_DR)
      df_DR_all <- rbind(df_DR_all,df_DR)
      
      df_DR_ratio <- table.DownsideRiskRatio(s$R,MAR = 0)
      colnames(df_DR_ratio) <- s$name
      df_DR_ratio <- t(df_DR_ratio)
      df_DR_ratio_all <- rbind(df_DR_ratio_all,df_DR_ratio)
      
      df_DD <- table.Drawdowns(s$R)
      #colnames(df_DD) <- s$name
      df_DD$strategy <- s$name
      #df_DD <- t(df_DD)
      df_DD_all <- rbind(df_DD_all,df_DD)
      
      df_DD_ratio <- table.DrawdownsRatio(s$R)
      colnames(df_DD_ratio) <- s$name
      df_DD_ratio <- t(df_DD_ratio)
      df_DD_ratio_all <- rbind(df_DD_ratio_all,df_DD_ratio)
      
      # df_modig <- table.modigliani(s$R,period = s$test,riskfree = RF)
      # colnames(df_modig) <- s$name
      # df_modig <- t(df_modig)
      # modig_all <- rbind(modig_all,df_modig) 
    }
    
  }
  
  
  df_returns_all <-t(df_returns_all) 
  
  df_dist_all <- t(df_dist_all) 
  
  df_DR_all <- t(df_DR_all) 
  
  df_DR_ratio_all <- t(df_DR_ratio_all) 
  
  #df_DD_all <- t(df_DD_all)
  
  df_DD_ratio_all <- t(df_DD_ratio_all) 
  
  #modig_all <- t(modig_all)
  
  
  if(export){
    
    df_dist_all %>% write.csv(file  = paste(folder_name,'/Distributions',year_file,".csv",sep=''),row.names = TRUE)
    df_returns_all %>% write.csv(file  = paste(folder_name,'/Returns',year_file,".csv",sep=''),row.names = TRUE)
    df_DR_all %>% write.csv(file  = paste(folder_name,'/DownsideRisk',year_file,".csv",sep=''),row.names = TRUE)
    df_DR_ratio_all %>% write.csv(file  = paste(folder_name,'/DownsideRiskRatio',year_file,".csv",sep=''),row.names = TRUE)
    df_DR_ratio_all %>% write.csv(file  = paste(folder_name,'/DrawdownsRatio',year_file,".csv",sep=''),row.names = TRUE)
    modig_all %>% write.csv(file  = paste(folder_name,'/Modigliani',year_file,".csv",sep=''),row.names = TRUE)
    df_DD_all %>% write.csv(file  = paste(folder_name,'/Drawdowns',year_file,".csv",sep=''),row.names = TRUE)
    
  }
  
  
  
  return(list(df_returns_all,df_dist_all,df_DR_all,df_DR_ratio_all,df_DD_all,df_DD_ratio_all,modig_all))
  
}

totalReturn <- function(R){
  cumRets <- cumprod(1 + R) - 1
  TotalRets <- cumRets %>%  coredata() %>%  tail(1)
  rownames(TotalRets) <- c("Total Return")
  #TotalRets <- TotalRets * 100 # Percentage
  return(TotalRets)
  
}

meanReturns <- function(R){
  u  <- colMeans(R) %>% as.data.frame() %>%  t()
  rownames(u) <- c("Mean Return")
  return(u)
}

performance_summary_v1 <- function(R,RF=0){
  tab1 <- table.AnnualizedReturns(R,Rf=RF) %>%t() %>%  as.data.frame() %>% tibble::rownames_to_column("Ticker")
  tab2 <- maxDrawdown(R) %>% t() %>%  as.data.frame()  %>% tibble::rownames_to_column("Ticker")
  tab3 <-meanReturns(R) %>% t() %>%  as.data.frame()  %>% tibble::rownames_to_column("Ticker")
  tab4 <-totalReturn(R) %>% t() %>%  as.data.frame()  %>% tibble::rownames_to_column("Ticker")
  
  merge_tab <- list(tab1,tab2,tab3,tab4) %>% reduce(full_join,by='Ticker')
  
  #merge_tab <- tibble::rownames_to_column(merge_tab,"Ticker")
  
  return(merge_tab)
}


get_table_strats_weights <- function(strategies_list,period_name,group_name,folder_path,export=FALSE){
  # ONLY FOR STRATS WITH THE SAME ASSETS
  
  DFweights <- NULL
  rnames <- c()
  for(s in strategies_list){
    if((grepl(period_name,s$name) == TRUE) && (grepl(group_name,s$name) == TRUE) && (grepl("_EW_",s$name)==FALSE) ){
      strat_name <- str_extract_all(s$name,"(?<=_).+(?=_)")[[1]]
      rnames <- append(rnames,strat_name)
      
      DFweights <- rbind(DFweights,s$w)
      
      
    }
  }
  rownames(DFweights) <- rnames
  DFweights <- DFweights %>%  round(4)
  
  if(export){
    fname <-paste(period_name,group_name,sep='_')
    file_path <- paste(folder_path,fname,'.csv',sep='')
    DFweights %>% write.csv(file_path)
    
  }
  
  return(DFweights)
}





# Fractality---------------------------------------------------------------------------------------------------


calcMDM <- function(x, N= dim(x)[1],scale=10:(N/4),q=-4:4,m=1){
  b <- MFDFA(x, scale, m, q)
  
  #effic <-  max(b[["Hq"]]) - min(b[["Hq"]])
  mdm <- (abs(b[["Hq"]][1] - 0.5) + abs(b[["Hq"]][9]-0.5))/2
  
  return(mdm)
  
}

calcDeltaH <- function(x, N= dim(x)[1],scale=10:(N/4),q=-4:4,m=1){
  b <- MFDFA(x, scale, m, q)
  
  #effic <-  max(b[["Hq"]]) - min(b[["Hq"]])
  deltaH <- max(b[["Hq"]]) - min(b[["Hq"]]) #Delta h
  
  return(deltaH)
  
}

table.assets.fractality <- function(R,fractality_function, name_fractality){
  frac <- sapply(R, fractality_function)
  frac <- frac %>%  as.data.frame()
  
  colnames(frac) <- c(name_fractality) 
  frac$Ticker <- rownames(frac)
  rownames(frac) <- NULL
  
  return(frac[c("Ticker",name_fractality)])
  
  
}

get_top_effic_names <- function(df_ranks,rankCol,top_effics){
  df_ranks <- df_ranks[c("Ticker",rankCol)] %>%  top_n(-top_effics)   # top in de descending order

  names <- df_ranks$Ticker
  
  return(names)
}

# Data Transform-----------------------------------------------------------------------

gatther_Rets <- function(strategies_list,name_pattern = NULL){
  if(is.null(name_pattern) == FALSE){
    strategies_list <- keep(strategies_list, function(s) grepl(name_pattern,s$name))
  }
  
  returns_list <- lapply(strategies_list, function(x){x$R})
  names_col <- lapply(strategies_list, function(x){x$name}) 
  
  series_returns <- do.call(merge,returns_list)
  
  colnames(series_returns) <- names_col
  series_returns <- series_returns %>% fortify.zoo %>% as.tibble() %>%  dplyr::rename("Date" = Index )

  return(series_returns)
  
}

returns_to_longer <- function(returns_gatthered){
  returns_gatthered[,-1] <- ((returns_gatthered[,-1] + 1) %>%  cumprod()) -1 #cummulative returns
  long_df <- returns_gatthered %>%  gather(key = "Strat", value = "return", -Date)
  return(long_df)
}

# Data Visualization ----------------------------------------------------------------

plot_compared_performance <- function(df_long_rets){
  df_long_rets %>%  ggplot(aes(x=Date,y=return)) +
    geom_line(aes(color = Strat), size = 1) +
    labs(x="Data",y='Retorno Acumulado',title = 'Desempenho das carteiras')+
    theme_classic()
  
}

