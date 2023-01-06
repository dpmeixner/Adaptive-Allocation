# ref: https://bookdown.org/compfinezbook/introcompfinr/Determining-Mean-Variance-Effici.html#alternative-derivation-of-an-efficient-portfolio

library(lubridate)
library(logger)
options(digits=4)

### Begin Configurable Parameters 

# List of possible tickers to include
tickers = c('DRN', 'TMF', 'TQQQ', 'EDC', 'URTY')
# Number of months used to calculate return of each ticker
timing_period = 2
# Number of months used to calculate volailitiy of each ticker
volatility_period = 2
# Number of months to backtest
backtest_period = 6
# Number of assets to hold during any given month
assets = 3
log_threshold("INFO")

### End Configurable Parameters

stopifnot(length(tickers) >= assets)

# Price history needs to include additional months of data to start
buffer_months = max(timing_period, volatility_period)
total_months = backtest_period + buffer_months

get_price_history <- function(tickers) {
  # Form the Yahoo Finance URL for ticker price data up to today
  base_url = 'https://query1.finance.yahoo.com/v7/finance/download/%s?period1=%d&period2=%d&interval=1d&events=history&includeAdjustedClose=true'
  today = Sys.Date()
  # TODO: This calculation doesn't work if total months ago doesn't have this date (e.g. Oct 31 8 months ago would be Feb 31 which isn't a real date). Second calculation goes back ~month*28 days, so could really end up as (months-1)
  #startDate = today - months(total_months)
  startDate = seq(Sys.Date(), length = 2, by = paste("-", total_months, " months", sep=""))[2]
  period1 = as.numeric(as.POSIXct(sprintf("%s 0:00:00 GMT", startDate)))
  # Add a day so end is midnight tonight 
  period2 = as.numeric(as.POSIXct(sprintf("%s 0:00:00 GMT", today + days(1))))

  # TODO: Can sapply be used here instead of loop?
  # Create a list with price history of every ticker
  price_history = list()
  for (ticker in tickers) {
    ticker_history = read.csv(sprintf(base_url, ticker,  period1, period2))
    ticker_history$return = c(0, tail(ticker_history$Adj.Close, -1) /
                              head(ticker_history$Adj.Close, -1) -1)
    ticker_history$Date = as.Date(ticker_history$Date)
    price_history = append(price_history, list(ticker_history))
  }
  names(price_history) <- tickers

  return(price_history)
}

calc_sigma <- function(prices) {  
  sigma.mat = cov(do.call(cbind, prices))
  top.mat = cbind(2*sigma.mat, 1)
  bot.vec = c(rep(1, assets), 0)
  Am.mat = rbind(top.mat, bot.vec)
  b.vec = c(rep(0, assets), 1)
  z.m.mat = solve(Am.mat)%*%b.vec
  x.vec = z.m.mat[1:assets,1]

  zero_tickers = {}
  n_tickers = assets

  # If a ticker is returned with negative allocation, it should be removed and
  # the calculation should be redone
  while (any(x.vec < 0)) {
    remove_idx = as.vector(which(x.vec < 0))
    remove_ticker = names(x.vec)[remove_idx]
    n_tickers = n_tickers - length(remove_idx)
    log_debug("negative allocation, removing ", 
            toString(remove_ticker))
    Am.mat = Am.mat[-remove_idx, -remove_idx]
    sigma.mat = sigma.mat[-remove_idx,-remove_idx]
    b.vec = c(rep(0, n_tickers), 1)
    z.m.mat = solve(Am.mat)%*%b.vec
    x.vec = z.m.mat[1:n_tickers,1]
    
    zero_tickers[remove_ticker]  = 0
  }
  
  if (length(zero_tickers) > 0) {
    result = data.frame(as.list(x.vec), as.list(zero_tickers))
  } else {
    result = x.vec
  }
  print(result)
  cat("\n")
  sig2.px = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)

  return(sig2.px)
}

# Get a list with price history for all tickers
price_history = get_price_history(tickers)
# Generate a list of dates in YY-MM format
year_months = ym(unique(substr(price_history[[1]]$Date,1,7)))
# For each time period, calculate the corresponding asset allocation
for(time_period in as.list(tail(year_months, -buffer_months) + months(1))) {
  start_date = ymd(time_period) - months(volatility_period)
  end_date = ymd(time_period) - days(1)
  # Get the closing price at the beginning of the time period
  previous_close_row = tail(which(price_history[[1]]$Date < 
                                  ymd(time_period) - months(timing_period)),
                            1)
  # Get the price data for all tickers in the time period of ineterest
  price_subsets = list()
  returns = c()
  for (t in tickers) {
    df = price_history[[t]]
    df_subset = df[(df$Date >= start_date) & (df$Date <= end_date),]
    price_subsets = append(price_subsets, list(df_subset))

    # Calculate daily returns
    return = tail(df_subset$Adj.Close, 1) / df[previous_close_row, 'Adj.Close']
    returns = append(returns, return)
  }
  names(price_subsets) <- tickers
 
  # Calculate returns for each ticker in descending order
  return_order = order(-returns)

  # Print the current time period
  cat(substr(time_period, 1, 7), "\n")
  # Extract only the tickers needed for this time period
  prices = sapply(price_subsets[return_order[1:assets]], `[`, 'return')
  names(prices) <- tickers[return_order[1:assets]]
  # Calculate the allocation for this time period
  calc_sigma(prices)
}

# Print the last day of data for informational purposes
log_debug("Data calculated through ", max(price_history[[1]]$Date), "\n")
