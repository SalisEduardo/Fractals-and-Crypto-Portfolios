source("main.R")
source("functions.R")

# Inverse Inefficiency all cryptocurrencies --------------------------------------

# Portfolio 
crypto_names <- colnames(crypto_returns_xts) 

# Weights
all_first_weights_InvInef <- format.InvInef.weights.df(first_deltaH,colnames(first_deltaH)[2],crypto_names)
all_second_weights_InvInef <- format.InvInef.weights.df(second_deltaH,colnames(second_deltaH)[2],crypto_names)
all_third_weights_InvInef <- format.InvInef.weights.df(third_deltaH,colnames(third_deltaH)[2],crypto_names)

## Exporting

### Better format ro export
weightsInvInef_2020 <- all_first_weights_InvInef %>%  
  t() %>% 
  as.data.frame() %>% 
  dplyr::rename(InfIneff2020 = V1)

weightsInvInef_2021 <- all_second_weights_InvInef %>%  
  t() %>% 
  as.data.frame() %>% 
  dplyr::rename(InfIneff2021 = V1)

weightsInvInef_2022 <- all_third_weights_InvInef %>%  
  t() %>% 
  as.data.frame() %>% 
  dplyr::rename(InfIneff2022 = V1)

weightsInvInef <- cbind(weightsInvInef_2020,weightsInvInef_2021,weightsInvInef_2022)

weightsInvInef %>%  write.csv("Results/Crypto_Indexes/Weights/InvInef_weights.csv")

# Strategies
first_InvInef_all <- build.inverse.inefficency.strategy("first_InvInef_all",
                                                        all_first_weights_InvInef,
                                                        crypto_returns_xts,
                                                        first_train_period,
                                                        first_test_period)

second_InvInef_all <- build.inverse.inefficency.strategy("second_InvInef_all",
                                                         all_second_weights_InvInef,
                                                         crypto_returns_xts,
                                                         second_train_period,
                                                         second_test_period)


third_InvInef_all <- build.inverse.inefficency.strategy("third_InvInef_all",
                                                         all_third_weights_InvInef,
                                                         crypto_returns_xts,
                                                        third_train_period,
                                                        third_test_period)


# Series
InvInef_index_daily_returns <-  rbind(first_InvInef_all$R,second_InvInef_all$R,third_InvInef_all$R) %>% 
  fortify.zoo %>% as.tibble() %>%  
  dplyr::rename("Date" = Index,"InvInef" = portfolio.returns )
InvInef_index_cumrets <- InvInef_index_daily_returns %>%  calc_cumrets()

## KPIs

get.strats.KPIs(list(first_InvInef_all),name_pattern='InvInef',RF = t10_daily_2020,year_file = '2020',folder_name='Results/Crypto_Indexes/KPIs/InvInef/',export = TRUE)
get.strats.KPIs(list(second_InvInef_all),name_pattern='InvInef',RF = t10_daily_2021,year_file = '2021',folder_name='Results/Crypto_Indexes/KPIs/InvInef/',export = TRUE)
get.strats.KPIs(list(third_InvInef_all),name_pattern='InvInef',RF = t10_daily_2022,year_file = '2022',folder_name='Results/Crypto_Indexes/KPIs/InvInef/',export = TRUE)




# Equal Weighted Index ----------------------------------------------------------------------------------

first_EW <- build.EW.portfolio("first_EW" ,
                               crypto_names,
                               crypto_returns_xts,
                               first_train_period,
                               first_test_period)

second_EW <- build.EW.portfolio("second_EW" ,
                                crypto_names,
                                crypto_returns_xts,
                                second_train_period,
                                second_test_period)

third_EW <- build.EW.portfolio("third_EW" ,
                               crypto_names,
                               crypto_returns_xts,
                               third_train_period,
                               third_test_period)





# Weights
equal_weights <- get_table_strats_weights(list(first_EW,second_EW,third_EW),"","","",export=FALSE)
  
rownames(equal_weights) <- c("EW_2020","EW_2021","EW_2022")
colnames(equal_weights) <- crypto_names

equal_weights <- equal_weights %>%  t() %>%  as.data.frame()
equal_weights %>%  write.csv("Results/Crypto_Indexes/Weights/EqualWeights.csv")


# KPIs

get.strats.KPIs(list(first_EW),name_pattern='EW',RF = t10_daily_2020,year_file = '2020',folder_name='Results/Crypto_Indexes/KPIs/EW/',export = TRUE)
get.strats.KPIs(list(second_EW),name_pattern='EW',RF = t10_daily_2021,year_file = '2021',folder_name='Results/Crypto_Indexes/KPIs/EW/',export = TRUE)
get.strats.KPIs(list(third_EW),name_pattern='EW',RF = t10_daily_2022,year_file = '2022',folder_name='Results/Crypto_Indexes/KPIs/EW/',export = TRUE)


# Series
EW_index_daily_returns <-  rbind(first_EW$R,second_EW$R,third_EW$R) %>% fortify.zoo %>% as.tibble() %>%  dplyr::rename("Date" = Index,"EW" = portfolio.returns )
EW_index_cumrets <- EW_index_daily_returns %>%  calc_cumrets()



# Minimum Variance ----------------------------------------------------------------------------------

first_MVP <- build.portfolio.strats("first_MVP" ,
                                    crypto_names,
                                    crypto_returns_xts,
                                    first_train_period,
                                    first_test_period,
                                    #pspec.lo.full,
                                    pspec.box.full,
                                    mvp.spec,
                                    neg_to_zero = TRUE)


second_MVP <- build.portfolio.strats("second_MVP" ,
                                     crypto_names,
                                     crypto_returns_xts,
                                     second_train_period,
                                     second_test_period,
                                     #pspec.lo.full,
                                     pspec.box.full,
                                     mvp.spec,
                                     neg_to_zero = TRUE)


third_MVP <- build.portfolio.strats("third_MVP" ,
                                    crypto_names,
                                    crypto_returns_xts,
                                    third_train_period,
                                    third_test_period,
                                    #pspec.lo.full,
                                    pspec.box.full,
                                    mvp.spec,
                                    neg_to_zero = TRUE)

# Weights
mvp_weights <- rbind(first_MVP$w,second_MVP$w,third_MVP$w)
rownames(mvp_weights) <- c('MVP_2020','MVP_2021','MVP_2022')

mvp_weights <- mvp_weights %>%  t() %>%  as.data.frame()
mvp_weights %>%  write.csv("Results/Crypto_Indexes/Weights/MVPWeights.csv")


# KPIs

get.strats.KPIs(list(first_MVP),name_pattern='MVP',RF = t10_daily_2020,year_file = '2020',folder_name='Results/Crypto_Indexes/KPIs/MVP/',export = TRUE)
get.strats.KPIs(list(second_MVP),name_pattern='MVP',RF = t10_daily_2021,year_file = '2021',folder_name='Results/Crypto_Indexes/KPIs/MVP/',export = TRUE)
get.strats.KPIs(list(third_MVP),name_pattern='MVP',RF = t10_daily_2022,year_file = '2022',folder_name='Results/Crypto_Indexes/KPIs/MVP/',export = TRUE)


# Series
MVP_index_daily_returns <-  rbind(first_MVP$R,second_MVP$R,third_MVP$R) %>% fortify.zoo %>% as.tibble() %>%  dplyr::rename("Date" = Index,"MVP" = portfolio.returns )
MVP_index_cumrets <- MVP_index_daily_returns %>%  calc_cumrets()


# Max Sharp Ratio ----------------------------------------------------------------------------------

first_maxSR <- build.portfolio.strats("first_maxSR" ,
                                      crypto_names,
                                      crypto_returns_xts,
                                      first_train_period,
                                      first_test_period,
                                      #pspec.lo.full,
                                      pspec.box.full,
                                      tp.sepc,
                                      maxSharp = TRUE,
                                      neg_to_zero = TRUE)


second_maxSR <- build.portfolio.strats("second_maxSR" ,
                                       crypto_names,
                                       crypto_returns_xts,
                                       second_train_period,
                                       second_test_period,
                                       #pspec.lo.full,
                                       pspec.box.full,
                                       tp.sepc,
                                       maxSharp = TRUE,
                                       neg_to_zero = TRUE)


third_maxSR <- build.portfolio.strats("third_maxSR" ,
                                      crypto_names,
                                      crypto_returns_xts,
                                      third_train_period,
                                      third_test_period,
                                      #pspec.lo.full,
                                      pspec.box.full,
                                      tp.sepc,
                                      maxSharp = TRUE,
                                      neg_to_zero = TRUE)

# Weights

maxsr_weights <- rbind(first_maxSR$w,second_maxSR$w,third_maxSR$w)
rownames(maxsr_weights) <- c('maxSR_2020','maxSR_2021','maxSR_2022')

maxsr_weights <- maxsr_weights %>%  t() %>%  as.data.frame()
maxsr_weights %>%  write.csv("Results/Crypto_Indexes/Weights/maxSRWeights.csv")


# KPIs

get.strats.KPIs(list(first_maxSR),name_pattern='maxSR',RF = t10_daily_2020,year_file = '2020',folder_name='Results/Crypto_Indexes/KPIs/maxSR/',export = TRUE)
get.strats.KPIs(list(second_maxSR),name_pattern='maxSR',RF = t10_daily_2021,year_file = '2021',folder_name='Results/Crypto_Indexes/KPIs/maxSR/',export = TRUE)
get.strats.KPIs(list(third_maxSR),name_pattern='maxSR',RF = t10_daily_2022,year_file = '2022',folder_name='Results/Crypto_Indexes/KPIs/maxSR/',export = TRUE)


# Series
maxSR_index_daily_returns <-  rbind(first_maxSR$R,second_maxSR$R,third_maxSR$R) %>% fortify.zoo %>% as.tibble() %>%  dplyr::rename("Date" = Index,"maxSR" = portfolio.returns )
maxSR_index_cumrets <- maxSR_index_daily_returns %>%  calc_cumrets()



# Comparison -----------------------------------------------------------------------------

indexes_rets <- list(EW_index_daily_returns,InvInef_index_daily_returns,MVP_index_daily_returns,maxSR_index_daily_returns) %>% reduce(full_join,by='Date')

index_cumrets <- indexes_rets %>%  calc_cumrets()
index_longDF_cumrets <- indexes_rets %>% cumrets_to_longer(create_group = FALSE)


index_plot <- plot_compared_performance(index_longDF_cumrets,"Cryptocurrencies Index Performances",color_colum=Strat)


indexes_rets %>% write.csv("Results/Crypto_Indexes/Series/index_daily.csv")
index_cumrets %>% write.csv("Results/Crypto_Indexes/Series/index_cumulative.csv")



