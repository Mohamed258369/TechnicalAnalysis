library(quantmod)
library(TTR)

symbols <- readLines("Portfolio.txt")

load_stock_data <- function(symbol) {
  getSymbols(symbol, src = "yahoo",
             auto.assign = FALSE)
}

stocks <- lapply(symbols, load_stock_data)
names(stocks) <- symbols

print(head(stocks[[1]])) 
calculate_statistics <- function(stock) {
  prices <- as.numeric(Cl(stock))
  prices <- prices[!is.na(prices)]
  
  frequency <- table(prices)
  
  data.frame(
    Mean = mean(prices),
    Median = median(prices),
    Mode = as.numeric(names(frequency)[which.max(frequency)]),
    Standard_Deviation = sd(prices),
    Moving_Average = tail(TTR::SMA(prices, n = 20), 1)
  )
}

statistics <- lapply(stocks, calculate_statistics)
names(statistics) <- names(stocks)

print(statistics)

for (symbol in names(stocks)) {
  chartSeries(
    stocks[[symbol]],
    name = paste(symbol, "Stock Price"),
    theme = chartTheme("white")
  )
}
display_data <- function(stock) {
  print(head(as.data.frame(stock)))
}

lapply(stocks, display_data)
