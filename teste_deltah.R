
#retornos <- diff(log(Mprices))
retornos <- crypto_returns_xts['2019/2020',c("LTC",'DOGE','BTC')]

N <- dim(retornos)[1] 
n <- dim(retornos)[2] 

# ---------------------------------------

# Set MF-DFA parameters:

scale <- 10:(N/4) # literature suggestion
q <- -4:4
m <- 1 # degree polinomial (avoid overfitting)

# ---------------------------------------

# RUN MF-DFA:


Resultados = matrix(0,n-1,1)

for(i in 1:(n-1)){
  
  b <- MFDFA(retornos[,i], scale, m, q)
  
  Resultados[i,1] = max(b[["Hq"]]) - min(b[["Hq"]]) #Delta h
}


Resultados

