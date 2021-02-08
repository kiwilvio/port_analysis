### Load Library ###
rm(list=ls())
library("quantmod")
library("tidyverse")
library("pdftools")

### Load Data ###

# Get today's and one year ago's date so we can see one year's worth of data
d <- as.POSIXlt(Sys.Date())
d$year <- d$year-1
d

# Get entire portfolio's returns for the past year
getSymbols(c("ARKW"),src="yahoo", from = d, to = Sys.Date(), verbose = T)

# Load in individual holdings 
ARKW_HOLDINGS <- read.csv("~/Desktop/code/R/port_analysis/ARKW_HOLDINGS.csv")
ARKW_HOLDINGS <- ARKW_HOLDINGS %>% 
  filter((ticker != "") & (ticker != "TREE UW")) %>% 
  arrange(ticker)

getSymbols(ARKW_HOLDINGS$ticker, src="yahoo", from = d, to = Sys.Date(), verbose = T)

closing <- AAPL$AAPL.Close

for (i in ARKW_HOLDINGS$ticker[-c(1)]) {
  tryCatch({
    to_add = paste0(i, "$", i, ".Close")
    to_add = eval(parse(text = to_add))
    closing <- cbind(closing, to_add)
  }, error = function(e){c("ERROR :", conditionMessage(e), "\n")})
}

closing <- closing %>% 
  t() %>% 
  {bind_cols(ticker=rownames(.), as_tibble(.))} %>% 
  mutate(ticker = str_replace(ticker, ".Close", "")) 

View(closing)

# View NA to show companies that IPO'd during the year
rowSums(is.na(closing))

closing_matrix <- as.matrix.data.frame(closing[-c(1)])
k <- which(is.na(closing_matrix), arr.ind=TRUE)
closing_matrix[k] <- rowMeans(closing_matrix, na.rm=TRUE)[k[,1]]
closing <- bind_cols(closing[1], as_tibble(closing_matrix))

rowSums(is.na(closing))

# for (i in ARKW_HOLDINGS$ticker){
#   tryCatch({
#     to_rename = paste0(i, "_returns")
#     to_add = paste0("(closing$", i, ".Close - lag(closing$", i, ".Close)) / lag(closing$", i, ".Close)")
#     to_add = eval(parse(text = to_add))
#     closing <- cbind(closing, to_add)
#     closing <- rename(closing, !!to_rename := to_add)
#   }, error = function(e){c("ERROR :", conditionMessage(e), "\n")})
# }

closing_prices <- closing[1:56]
returns <- closing[c(1, 57:91)]
View(closing_prices)
View(returns)

(last(closing_prices[-c(1)]) - first(closing_prices[-c(1)])) / first(closing_prices[-c(1)])
