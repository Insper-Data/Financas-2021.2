
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

 
 #--------------------------------------------------------------------------------------------
 # Baixando retornos
 #-------------------------------------------------------------------------------------------- 
 
 portfolioPrices_tb <- data.frame(portfolioPrices)
 
 Index <- rownames(portfolioPrices_tb) %>% 
    as.Date() 
 
 portfolioPrices_tb <- rownames_to_column(portfolioPrices_tb, var = "Data") %>% as_tibble()
 
 
 portfolioPrices_tb <- portfolioPrices_tb %>% 
    select(-'ASAI3.SA.Close')
 
 
 portfolioReturns = na.omit(ROC(portfolioPrices_tb)) 
 
 
 portfolioReturns_tb <- data.frame(portfolioReturns)
 
 
 Index1 <- rownames(portfolioReturns_tb) %>% 
    as.Date() 
 
 portfolioReturns_tb <- rownames_to_column(portfolioReturns_tb, var = "Data") %>% as_tibble()
 
 
 


 #--------------------------------------------------------------------------------------------
 # Tidying PortfolioReturns e PortfolioPrices
 #-------------------------------------------------------------------------------------------- 
   
 
 portfolioPrices_tb <- portfolioPrices_tb %>% 
    pivot_longer(cols = ABEV3.SA.Close:YDUQ3.SA.Close, names_to = "Ticker", values_to = "Preco_Fechamento")
 
 portfolioPrices_tb <- portfolioPrices_tb %>%
    separate(Ticker, into = c("Ticker", "Nada"), sep = ".Close", remove = FALSE) %>% 
    select(Ticker, Data, Preco_Fechamento)
 
 
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
 
 cartIBRX = cartIBRX$tickersSA 
 
 cartIBRX <- cartIBRX %>% 
    data.frame() 
 
 colnames(cartIBRX) <- "Ticker"
 
 cartIBRX <- cartIBRX %>% 
    mutate(IBRX50 = "Yes")

 
 
 #--------------------------------------------------------------------------------------------
 # Criando colunas para indicar IBOV e IBRX
 #--------------------------------------------------------------------------------------------  
 
 Database <- Database %>% 
    full_join(cartIBRX, by = "Ticker") 
 
 
 Database$IBRX50[which(is.na(Database$IBRX50))] <- "No"
 
 
#--------------------------------------------------------------------------------------------
# Baixando os dados das acoes no YAHOO
#-------------------------------------------------------------------------------------------- 

 #portfolioPrices = NULL
 #for(ticker in cartIBRX) {
 # portfolioPrices = cbind(portfolioPrices,
 #                         getSymbols.yahoo(ticker, from = di, to= df, periodicity = "daily", auto.assign = FALSE)[,4])
 #} 
 
 



# Criando matriz de correlacao dos retornos

 #correl = cor(portfolioReturns)


# Criando matriz de covariancia dos retornos

 #covar = cov(portfolioReturns)



