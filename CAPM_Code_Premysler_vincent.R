
library(quantmod)
library(tseries)
library(xts)
library(dplyr)

tickers <- c("QQQ","AZN", "BKR", "AVGO", "BIIB", "CDNS", "ADBE", "CHTR", "CPRT", "CSGP", "CTAS", "CSCO", "CMCSA", "COST", "CSX", "CTSH", "DXCM", "FANG", "DLTR", "EA", "ON", "EXC", "TTD", "FAST", "META", "FI", "FTNT", "GILD", "GOOG", "GOOGL", "HON", "ILMN", "INTC", "INTU", "ISRG", "MRVL", "IDXX", "KLAC", "KHC", "LRCX", "LULU", "MELI", "MAR", "MCHP", "MDLZ", "MNST", "MSFT", "MU", "NFLX", "NVDA", "NXPI", "ODFL", "ORLY", "PCAR", "PANW", "PAYX", "PYPL", "PEP", "QCOM", "REGN", "ROST", "SIRI", "SBUX", "SNPS", "TSLA", "TXN", "TMUS", "VRSK", "VRTX", "WBA", "WBD", "WDAY", "XEL", "ADP", "AMD", "AMZN", "AMGN", "AEP", "CDW", "ADI", "ROP", "ANSS", "AAPL", "AMAT", "ASML", "TEAM", "ADSK")

# Set the start and end dates for data retrieval
start_date <- as.Date("2017-01-01")
end_date <- as.Date("2024-04-12")

# Create an empty list to store the data
stock_data <- list()

# Loop through each ticker symbol and retrieve data
for (symbol in tickers) {
  tryCatch({
    data <- getSymbols(symbol, auto.assign = FALSE, 
                       from = start_date, to = end_date, periodicity = "weekly")
    stock_data[[symbol]] <- data
  }, error = function(e) {
    cat(sprintf("Error retrieving data for %s: %s\n", symbol, e$message))
  })
}
stock_df <- do.call(cbind, stock_data)
close_cols <- grep("Close", names(stock_df[1]), value = TRUE)

close_df=stock_df[, close_cols]

# Merge riskfree and pricedata


# read in files
Data <- close_df

# check class type
class(close_df)

# View first few records
head(close_df)

# View last few records
tail(close_df)

# Use dimensions of data to get price history, nrow, ncol gives number of rows
# and columns
NRows <- as.numeric(nrow(close_df))
NCols <- as.numeric(ncol(close_df))
NRows
NCols

# Format dates, col 1 is date, col 2 is index, col 3 is risk-free, rest are stocks
#Dates <- as.Date(close_df$Date, format="%m/%d/%Y")
#PriceData <- cbind(Dates, Data[,2:NCols])

# Get stock price history and view
PriceData <- as.data.frame(close_df)
RiskFree=read.csv('/users/vincentpremysler/rf.csv',row.names=1)

# Merge riskfree and pricedata
merged_data=cbind(RiskFree,PriceData)
rownames(PriceData) <- NULL
NRowsPrices <- as.numeric(nrow(PriceData))
NColsPrices <- as.numeric(ncol(PriceData))
head(PriceData)
tail(PriceData)

# Compute returns

Returns <- (merged_data[2:NRowsPrices,1:NColsPrices]/merged_data[1:(NRowsPrices-1),1:NColsPrices]-1)
head(Returns)
tail(Returns)

# Replace returns with values for TB3MS which is risk-free rate
# convert the risk-free to monthly
Returns[1:nrow(Returns),1] <- merged_data[2:NRows,1]/100/12
head(Returns)
tail(Returns)
NRowsMatrix <- ncol(Returns)-2

# Create matrix to store regression results from time series regression
# Need to store alpha and beta values, there are two less rows because
# we do not need values for the benchmark index and risk-free rate
TSRegressionResults <- matrix(0, nrow=NRowsMatrix, ncol=7)
# Names for outputs in columns
colnames(TSRegressionResults) <- c("alpha", "beta", "p-value alpha", "p-value beta", 
                                   "r-squared", "MeanExcessReturn", "VarResiduals")

# Compute excess returns on index
ExcessReturnIndex <- Returns[,2]-Returns[,1]

# For loop for first passs time series regressions to get intercepts, beta coefficients,
# and other information
for (i in 1:NRowsMatrix)
{
  
  # Compute excess return for stocks
  ExcessReturnStock <- Returns[,i+2]-Returns[,1]
  
  # Regress the returns of stocks over the index to determine their alpha and beta, as well as test how much this explains
  TSRegression <- lm(ExcessReturnStock ~ ExcessReturnIndex)
  TSRegressionObject <- summary(TSRegression)
  
  # Store alpha, beta, p alpha, p beta, and r-squared for each stock
  TSRegressionResults[i,1] <- coef(TSRegressionObject)[1]
  TSRegressionResults[i,2] <- coef(TSRegressionObject)[2]
  TSRegressionResults[i,3] <- coef(TSRegressionObject)[7]
  TSRegressionResults[i,4] <- coef(TSRegressionObject)[8]
  TSRegressionResults[i,5] <- TSRegressionObject$r.squared
  TSRegressionResults[i,6] <- mean(Returns[,i+2]-Returns[,1])
  TSRegressionResults[i,7] <- TSRegressionObject$sigma^2
  
}

# View regression results
round(TSRegressionResults,4)

# Second pass cross sectional regression
# Average return minus risk-free rate for each asset is y (dependent variable)
# independent variable is betas from first pass time series regression
# Intercept should be statistically zero and we fail to reject null of Ho = 0
# Betas should be statistically different from zero and we reject the null Ho = 0
# Does slope equal Rm - Rf - we use t-test to check and we fail to reject null 
CSRegressionResults <- lm(TSRegressionResults[,6]~TSRegressionResults[,2])
CSRegressionObject <- summary(CSRegressionResults)
CSRegressionObject
tstat1 <- (coef(CSRegressionObject)[2]-mean(ExcessReturnIndex))/coef(CSRegressionObject)[4]
tstat1

# Second pass extended cross sectional regression
# Average return minus risk-free rate for each asset is y (dependent variable)
# independent variables are betas from first pass time series regression
# and variance of residuals from first pass time series regression
# Intercept should be statistically zero and we fail to reject null of Ho = 0
# Betas should be statistically different from zero and we reject the null Ho = 0
# Variance of the residuals should not be statistically different from zero but they are
# Does slope equal Rm - Rf - we use t-test to check and we reject the null
ExCSRegressionResults <- lm(TSRegressionResults[,6]~TSRegressionResults[,2]+TSRegressionResults[,7])
ExCSRegressionObject <- summary(ExCSRegressionResults)
ExCSRegressionObject
tstat2 <- (coef(ExCSRegressionObject)[2]-mean(ExcessReturnIndex))/coef(ExCSRegressionObject)[5]
tstat2

