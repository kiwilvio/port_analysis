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

closing <- TSLA$TSLA.Close

for (i in MF_tickers[2:length(MF_tickers)]){
  tryCatch({
  to_add = paste0(i, "$", i, ".Close")
  to_add = eval(parse(text = to_add))
  closing <- cbind(closing, to_add)
  }, error = function(e){c("ERROR :", conditionMessage(e), "\n")})
}

# View NA (Unity IPOd recently)
colSums(is.na(data))

closing <- tibble(date=index(closing), as.tibble(closing))


for (i in MF_tickers){
  tryCatch({
    to_rename = paste0(i, "_returns")
    to_add = paste0("(closing$", i, ".Close - lag(closing$", i, ".Close)) / lag(closing$", i, ".Close)")
    to_add = eval(parse(text = to_add))
    closing <- cbind(closing, to_add)
    closing <- rename(closing, !!to_rename := to_add)
  }, error = function(e){c("ERROR :", conditionMessage(e), "\n")})
}

closing_prices <- closing[1:46]
returns <- closing[c(1, 47:91)]

x <- abs((na.fill0(slice(closing_prices[-c(1)], 171), 0) - na.fill0(slice(closing_prices[-c(1)], 1), 0))) / na.fill0(slice(closing_prices[-c(1)], 1), 0)

x <- x %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 

x <- x %>% arrange(desc(`1`))
x
