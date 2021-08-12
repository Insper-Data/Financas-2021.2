#Bibliotecas
library(plyr)
library(BatchGetSymbols)
library(tidyquant)
library(tidyverse)
library(rvest)

#Buscando ativos no IBOV
ibov = GetIbovStocks()
ibov$tickersSA = paste(ibov$tickers,".SA", sep = "")

#Estabelecendo data inicial e final
di = Sys.Date()-360
df = Sys.Date()

#Criando vetor de ativos desejados
#cart = c("ABEV3.SA","B3SA3.SA","LAME3.SA")
cart = ibov$tickersSA
#Buscando dados pelo Yahoo
portfolioPrices = NULL
for(ticker in cart) {
  portfolioPrices = cbind(portfolioPrices,
                          getSymbols.yahoo(ticker, from = di, to= df, periodicity = "monthly", auto.assign = FALSE)[,4])
}
#Calculando retornos dos ativos
portfolioReturns = na.omit(ROC(portfolioPrices))
#Criando matriz de correlacao dos retornos
correl = cor(portfolioReturns)
#Criando matriz de covariância dos retornos
covar = cov(portfolioReturns)



# Scraping para IBRX50 ----------------------------------------------------


url = 'https://br.advfn.com/indice/ibrx-50'

scraping_ibrx50 = url %>% 
  read_html() %>%
  html_nodes(".Column2") %>% 
  html_text()

scraping_ibrx50 = scraping_ibrx50[-1]
scraping_ibrx50 = scraping_ibrx50[-length(scraping_ibrx50)]
scraping_ibrx50[length(scraping_ibrx50)+1] = "AMER3"
scraping_ibrx50 = sort(scraping_ibrx50)
scraping_ibrx50_SA = paste(scraping_ibrx50,".SA",sep = "")
cart = scraping_ibrx50_SA

cart = ibov$tickersSA
#Buscando dados pelo Yahoo
portfolioPrices = NULL
for(ticker in cart) {
  portfolioPrices = cbind(portfolioPrices,
                          getSymbols.yahoo(ticker, from = di, to= df, periodicity = "monthly", auto.assign = FALSE)[,4])
}