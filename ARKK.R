### Load Library ###
rm(list=ls())
library("quantmod")
library("tidyverse")
library("pdftools")

### Load Data ###
d <- as.POSIXlt(Sys.Date())
d$year <- d$year-1
d

getSymbols(c("ARKK"),src="yahoo", from = d, to = Sys.Date())

MF_holdings <- pdf_text("https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_INNOVATION_ETF_ARKK_HOLDINGS.pdf") %>% 
  readr::read_lines()
MF_holdings <- MF_holdings[3:55]
 
MF_tickers <- MF_holdings %>% 
  str_squish() %>% 
  str_replace_all(",", "") %>% 
  str_extract("\\b[A-Z]+\\b(?!.*\\b[A-Z]+\\b)")
MF_tickers

getSymbols(MF_tickers, src="yahoo", from = d, to = Sys.Date())


for (i in MF_tickers[2:length(MF_tickers)]){
  tryCatch({
  to_add = paste0(i, "$", i, ".Close")
  to_add = eval(parse(text = to_add))
  closing <- cbind(closing, to_add)
  }, error = function(e){c("ERROR :", conditionMessage(e), "\n")})
}
  
