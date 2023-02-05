
#setwd("C:/Users/Leandro/Desktop/Paper Eduardo/Experimentos - MF-DFA")

# Load packages:

library(MFDFA)
library(readxl)
library(moments)

# ---------------------------------------

# Load returns data:

#returns <- read_excel("returns.xlsx")
returns <- crypto_returns_xts['2018/'] %>% fortify.zoo %>% as.tibble() %>%  dplyr::rename("Date" = Index )



n <- dim(returns)[2] - 1 # number of cryptocurrencies 


ResEstatisticas = matrix(0,nrow = 7,ncol = n)

for(i in 1:n){
  ResEstatisticas[1,i] = mean(as.matrix(returns[,i+1]))
  ResEstatisticas[2,i] = median(as.matrix(returns[,i+1]))
  ResEstatisticas[3,i] = sd(as.matrix(returns[,i+1]))
  ResEstatisticas[4,i] = max(as.matrix(returns[,i+1]))
  ResEstatisticas[5,i] = min(as.matrix(returns[,i+1]))
  ResEstatisticas[6,i] = skewness(as.matrix(returns[,i+1]))
  ResEstatisticas[7,i] = kurtosis(as.matrix(returns[,i+1]))
  
}

# ---------------------------------------

# Split samples:

returns1 <- returns[1:729,] # 2018 and 2019
N1 <- dim(returns1)[1]

returns2 <- returns[366:1096,] # 2019 and 2020
N2 <- dim(returns2)[1]

returns3 <- returns[731:1461,] # 2020 and 2021
N3 <- dim(returns3)[1]

# ---------------------------------------

# Compute MF-DFA results for 3 subsamples and for 19 cryptocurrencies


# Set MF-DFA parameters:

q <- -4:4
m <- 1 # polinomial degree (avoid overfitting)


Resultados = matrix(0,n*3,6)


# Sample 1:
scale <- 10:(N1/4)
j = 1
for(i in 1:n){

  b <- MFDFA(as.matrix(returns1[,(i+1)]), scale, m, q)
  
  Resultados[j,1] = max(b[["spec"]][["hq"]]) # alpha max
  Resultados[j,2] = min(b[["spec"]][["hq"]]) # alpha min
  Resultados[j,3] = b[["spec"]][["hq"]][4] # alpha zero
  Resultados[j,4] = max(b[["spec"]][["hq"]]) - min(b[["spec"]][["hq"]]) # delta alpha
  Resultados[j,5] = max(b[["Hq"]]) - min(b[["Hq"]]) # Delta h
  Resultados[j,6] = ((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))-(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4]))/((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))+(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4])) # Theta
  rm(b)
  j = j + 3
}

# Sample 2:
scale <- 10:(N2/4)
j = 2
for(i in 1:n){
  
  b <- MFDFA(as.matrix(returns2[,(i+1)]), scale, m, q)
  
  Resultados[j,1] = max(b[["spec"]][["hq"]]) # alpha max
  Resultados[j,2] = min(b[["spec"]][["hq"]]) # alpha min
  Resultados[j,3] = b[["spec"]][["hq"]][4] # alpha zero
  Resultados[j,4] = max(b[["spec"]][["hq"]]) - min(b[["spec"]][["hq"]]) # delta alpha
  Resultados[j,5] = max(b[["Hq"]]) - min(b[["Hq"]]) # Delta h
  Resultados[j,6] = ((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))-(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4]))/((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))+(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4])) # Theta
  rm(b)
  j = j + 3
}

# Sample 3:
scale <- 10:(N3/4)
j = 3
for(i in 1:n){
  
  b <- MFDFA(as.matrix(returns3[,(i+1)]), scale, m, q)
  
  Resultados[j,1] = max(b[["spec"]][["hq"]]) # alpha max
  Resultados[j,2] = min(b[["spec"]][["hq"]]) # alpha min
  Resultados[j,3] = b[["spec"]][["hq"]][4] # alpha zero
  Resultados[j,4] = max(b[["spec"]][["hq"]]) - min(b[["spec"]][["hq"]]) # delta alpha
  Resultados[j,5] = max(b[["Hq"]]) - min(b[["Hq"]]) # Delta h
  Resultados[j,6] = ((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))-(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4]))/((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))+(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4])) # Theta
  rm(b)
  j = j + 3
}

Resultados %>%  view()

#write.table(Resultados, file = "Results")
#write.table(ResEstatisticas, file = "ResultsEst")
