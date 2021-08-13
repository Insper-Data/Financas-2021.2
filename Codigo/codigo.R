
#--------------------------------------------------------------------------------------------
#   BIBLIOTECAS
#--------------------------------------------------------------------------------------------

 library(plyr)
 library(BatchGetSymbols)
 library(tidyquant)
 library(tidyverse)
 library(rvest)
 library(tidyr)
 library(quantmod)
 library(magrittr)

#--------------------------------------------------------------------------------------------
# Buscando ativos - IBOV
#--------------------------------------------------------------------------------------------

 ibov = GetIbovStocks()
 ibov$tickersSA = paste(ibov$tickers,".SA", sep = "")


# Estabelecendo data inicial e final

 di = Sys.Date()-360
 df = Sys.Date()


# Criando vetor de ativos desejados

 cartIBOV = c("ABEV3.SA","B3SA3.SA","LAME3.SA")
 cartIBOV = ibov$tickersSA


#--------------------------------------------------------------------------------------------
# Baixando os dados das acoes no YAHOO
#--------------------------------------------------------------------------------------------

 
 portfolioPrices = NULL

 for(ticker in cartIBOV) {
  portfolioPrices = cbind(portfolioPrices,
                          getSymbols.yahoo(ticker, 
                                           from = di, to= df, 
                                           periodicity = "daily", 
                                           auto.assign = FALSE)[,4])
}

 portfolioReturns = na.omit(ROC(portfolioPrices))
 
 #--------------------------------------------------------------------------------------------
 # Tidying PortfolioPrices 
 #-------------------------------------------------------------------------------------------- 
 
 portfolioPrices_tb <- data.frame(portfolioPrices)
   
 
 head(rownames(portfolioPrices_tb))
 class(rownames(portfolioPrices_tb))
 
 Index <- rownames(portfolioPrices_tb) %>% 
   as.Date() 
 
 portfolioPrices_tb <- rownames_to_column(portfolioPrices_tb, var = "Data") %>% as_tibble()
 portfolioPrices_tb

 portfolioPrices_tb <- portfolioPrices_tb %>% 
   pivot_longer(cols = ABEV3.SA.Close:YDUQ3.SA.Close, names_to = "Ticker", values_to = "Preco_Fechamento")
   
 portfolioPrices_tb <- portfolioPrices_tb %>%
   separate(Ticker, into = c("Ticker", "Nada"), sep = ".Close", remove = FALSE) %>% 
   select(Ticker, Data, Preco_Fechamento)
 


 #--------------------------------------------------------------------------------------------
 # Tidying PortfolioReturns 
 #-------------------------------------------------------------------------------------------- 
   
   
 portfolioReturns_tb <- data.frame(portfolioReturns)
 
 
 head(rownames(portfolioReturns_tb))
 class(rownames(portfolioReturns_tb))
 
 Index1 <- rownames(portfolioReturns_tb) %>% 
   as.Date() 
 
 portfolioReturns_tb <- rownames_to_column(portfolioReturns_tb, var = "Data") %>% as_tibble()
 portfolioReturns_tb
 
 portfolioReturns_tb <- portfolioReturns_tb %>% 
   pivot_longer(cols = ABEV3.SA.Close:YDUQ3.SA.Close, names_to = "Ticker", values_to = "Retorno")
 
 portfolioReturns_tb <- portfolioReturns_tb %>%
   separate(Ticker, into = c("Ticker", "Nada"), sep = ".Close", remove = FALSE) %>% 
   select(Ticker, Data, Retorno)


 #--------------------------------------------------------------------------------------------
 # Unindo as bases
 #--------------------------------------------------------------------------------------------
 
 
Database <- portfolioPrices_tb %>% 
   left_join(portfolioReturns_tb, by= c("Ticker", "Data"))
 
 

  
#--------------------------------------------------------------------------------------------
# Buscando ativos - IBRX50
#--------------------------------------------------------------------------------------------

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
 cartIBRX = scraping_ibrx50_SA
 
 cartIBRX = ibov$tickersSA 
 

#--------------------------------------------------------------------------------------------
# Baixando os dados das acoes no YAHOO
#-------------------------------------------------------------------------------------------- 

 portfolioPrices = NULL
 for(ticker in cartIBRX) {
   portfolioPrices = cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from = di, to= df, periodicity = "daily", auto.assign = FALSE)[,4])
 } 
 
 



# Criando matriz de correlacao dos retornos

 correl = cor(portfolioReturns)


# Criando matriz de covariancia dos retornos

 covar = cov(portfolioReturns)

##oi

