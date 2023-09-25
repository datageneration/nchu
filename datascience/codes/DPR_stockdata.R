# Data Programming with R
# Collecting Stock data and plotting stock data as time series

Installed <- TRUE  # For checking if package is installed
toInstall <- c("quantmod", "ggplot2", "magrittr","broom")
if(Installed){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, require, character.only = TRUE) # call into library

# Setting time period
start = as.Date("2010-07-01") 
end = as.Date("2023-09-25")
quantmod::getSymbols("AAPL")
getSymbols("AAPL") # Get stock data of Apple
# Get data from tiingo instead of yahoo (optional)
# getSymbols("EGRNF", src = "tiingo", api.key ="[API key]") 

# Download Dow Jones Industrial Average
getSymbols("^DJI", src="yahoo") # Dow Jones Industrial Average

# Download Taiwan Weighted Index
getSymbols("^TWII", src="yahoo") # TWSE:IND

# Simple plot
plot(TWII, col="darkblue")
     
# Plot candle stick and other charts using quantmod
chartSeries(DJI)
chartSeries(DJI, type = c("auto", "candlesticks", "matchsticks", "bars","line"), subset='last 4 months',theme = "white")
barChart(DJI,multi.col=TRUE,theme = 'white')
lineChart(DJI,line.type = 'l', theme = 'white') # line, choices include l, h, c, b
lineChart(DJI,line.type = 'h',theme = chartTheme('white', up.col='steelblue')) # histogram
candleChart(DJI,subset = '2020-01/2022-01', multi.col=TRUE,theme = chartTheme('white'))
## grey => Open[t] < Close[t] and Op[t] < Cl[t-1]
## white => Op[t] < Cl[t] and Op[t] > Cl[t-1]
## red => Op[t] > Cl[t] and Op[t] < Cl[t-1]
## black => Op[t] > Cl[t] and Op[t] > Cl[t-1]

## Plotting multiple series using ggplot2
# Collect stock names from Yahoo Finance
getSymbols(c("AAPL", "MSFT", "AMZN", "TSLA", "GOOGL"), src = "yahoo", from = start, to = end)

# Prepare data as xts (time series object)
stocks = as.xts(data.frame(AAPL = AAPL[, "AAPL.Adjusted"], 
                           MSFT = MSFT[, "MSFT.Adjusted"], 
                           AMZN = AMZN[, "AMZN.Adjusted"],
                           GOOGL = GOOGL[, "GOOGL.Adjusted"],
                           TSLA = TSLA[, "TSLA.Adjusted"]))

class(stocks)
# Index by date
names(stocks) = c("Apple", "Microsoft", "Amazon", "Google", "Tesla")
index(stocks) = as.Date(index(stocks))

# Plot
stocks_series = tidy(stocks) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(linewidth=.5) +
  theme_bw()
stocks_series

# Plot TWII
TWII_series = tidy(TWII$TWII.Adjusted) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(linewidth=.5) +
  theme_bw()
TWII_series

stocks_series = tidy(stocks) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(linewidth=.5) +
  theme_bw() +
  labs(title = "Daily Stock Prices, 7/1/2010 - 09/30/2022",
     subtitle = "End of Day Adjusted Prices",
     caption = "Source: Yahoo Finance") +
  xlab("Date") + ylab("Price") +
  scale_color_manual(values = c("steelblue", "firebrick","purple", "forestgreen","darkgray")) +
  theme(text = element_text(family = "Palatino"), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position="top")
stocks_series
