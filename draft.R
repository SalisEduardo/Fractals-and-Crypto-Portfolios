






first_deltaH <- table.assets.fractality(crypto_returns_xts[first_train_period],calcDeltaH,"DeltaH_2018_2019")
second_deltaH <- table.assets.fractality(crypto_returns_xts[second_train_period],calcDeltaH,"DeltaH_2019_2020")
third_deltaH <- table.assets.fractality(crypto_returns_xts[third_train_period],calcDeltaH,"DeltaH_2020_2021")

all_deltaH <- list(first_deltaH,second_deltaH,third_deltaH) %>% reduce(left_join, by = "Ticker")


effic_ranks <- sapply(all_deltaH[-1], rank) %>%  as.data.frame()
colnames(effic_ranks) <- c("RankEffic_2018_2019", "RankEffic_2019_2020", "RankEffic_2020_2021")
effic_ranks$Ticker <- all_deltaH$Ticker
effic_ranks <- effic_ranks[c('Ticker',"RankEffic_2018_2019", "RankEffic_2019_2020", "RankEffic_2020_2021")]


## Portfolios build with 4 crypto currencies

psize = 6

mostN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = psize) #top ranks gets a positive sign
lessN_first <- get_top_effic_names(effic_ranks,"RankEffic_2018_2019",top_effics = - psize) #bottom ranks gets a positive sign

mostN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = psize)
lessN_second <- get_top_effic_names(effic_ranks,"RankEffic_2019_2020",top_effics = - psize)

mostN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = psize)
lessN_third <- get_top_effic_names(effic_ranks,"RankEffic_2020_2021",top_effics = - psize)

status_efficiency ="most"
train = PERIODS[[1]]$train
test = PERIODS[[1]]$test
asset_names = mostN_first
opt = 'DEoptim'
n_assets = length(asset_names)

periodDeltaH <- period_deltaH[[train]]


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
periodDeltaH <- get_deltaH(train)



MVP <-  build.portfolio.strats( paste("MVP",status_efficiency,as.character(n_assets),test,sep='_'),
                                asset_names,
                                crypto_returns_xts,
                                train,
                                test,
                                pspec.lo.full,
                                mvp.spec, 
                                neg_to_zero = TRUE,
                                optimizor = opt)


maxSR <-  build.portfolio.strats(paste("maxSR",status_efficiency,as.character(n_assets),test,sep='_'),
                                 asset_names,
                                 crypto_returns_xts,
                                 train,
                                 test,
                                 pspec.lo.full,
                                 tp.sepc,
                                 maxSharp = TRUE,
                                 neg_to_zero = TRUE,
                                 optimizor = opt)


EW <- build.EW.portfolio( paste("EW",status_efficiency,as.character(n_assets),test,sep='_'),
                          asset_names,
                          crypto_returns_xts,
                          train,
                          test)


weights_InvInef <- format.InvInef.weights.df(periodDeltaH,colnames(periodDeltaH)[2],asset_names)

InvInef <- build.inverse.inefficency.strategy(paste("InvInef",status_efficiency,as.character(n_assets),test,sep='_'),
                                              weights_InvInef,
                                              crypto_returns_xts,
                                              train,
                                              test)


