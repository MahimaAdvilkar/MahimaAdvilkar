# Assuming necessary libraries are already installed
library(quantmod)
library(forecast)
library(pander)

# Fetch TSLA stock data
getSymbols("TSLA", from="2022-02-15", to="2024-02-15")

# Extract closing prices
TSLA_Close_Prices <- Cl(TSLA)

#Plotting the closing stock price of TESLA
plot(TSLA_Close_Prices, main = "TESLA's Closing Stock Price") #generate plot of stock data

# Define training and testing sets
training <- head(TSLA_Close_Prices, 492)  # First 492 for training
testing <- tail(TSLA_Close_Prices, 10)    # Last 10 for testing

# Create a ts object for the training data
TSLA.ts <- ts(as.numeric(training), frequency=252)  # Frequency adjusted to typical trading days

# Perform forecasts
pred.mv <- meanf(TSLA.ts, h=10)$mean
pred.naive <- naive(TSLA.ts, h=10)$mean
pred.snaive <- snaive(TSLA.ts, h=10)$mean
pred.rwf <- rwf(TSLA.ts, h=10, drift=TRUE)$mean

# Combine forecasts into a table for display
pred.table <- cbind(pred.mv, pred.naive, pred.snaive, pred.rwf)

# Display the forecasting table using pander
pander(pred.table, caption = "Forecasting Table")

# Plotting the actual and forecasted prices
# Convert predictions and testing data to numeric vectors for plotting
predictions <- as.numeric(unlist(pred.table))
testing_values <- as.numeric(testing)


plot(493:502, testing_values, type="l", xlim=c(493,502), 
     ylim=c(min(c(testing_values, predictions)), max(c(testing_values, predictions))), 
     xlab="Observation Sequence", ylab="Stock Price", main="TSLA Stock Price Forecast")





points(493:502, pred.mv, pch=15, col="red")
points(493:502, pred.naive, pch=16, col="blue")
points(493:502, pred.rwf, pch=18, col="navy")
points(493:502, pred.snaive, pch=17, col="purple")
lines(493:502, pred.mv, lty=2, col="red")
lines(493:502, pred.naive, lty=2, col="blue")
lines(493:502, pred.rwf, lty=2, col="navy")
lines(493:502, pred.snaive, lty=2, col="purple")
legend("topright", legend=c("Moving Average", "Naive", "Drift", "Seasonal Naive"), 
       col=c("red", "blue", "navy", "purple"), pch=15:18, lty=2, bty="n", cex=0.8)

# Statistical analysis for forecast accuracy
true.value <- as.numeric(testing)
PE.mv <- 100 * (true.value - pred.mv) / true.value
PE.naive <- 100 * (true.value - pred.naive) / true.value
PE.snaive <- 100 * (true.value - pred.snaive) / true.value
PE.rwf <- 100 * (true.value - pred.rwf) / true.value

MAPE <- c(mean(abs(PE.mv), na.rm=TRUE), mean(abs(PE.naive), na.rm=TRUE), mean(abs(PE.snaive), na.rm=TRUE), mean(abs(PE.rwf), na.rm=TRUE))
e.mv <- true.value - pred.mv
e.naive <- true.value - pred.naive
e.snaive <- true.value - pred.snaive
e.rwf <- true.value - pred.rwf

MAD <- c(mean(abs(e.mv)), mean(abs(e.naive)), mean(abs(e.snaive)), mean(abs(e.rwf)))
MSE <- c(mean(e.mv^2), mean(e.naive^2), mean(e.snaive^2), mean(e.rwf^2))

accuracy.table <- cbind(MAPE, MAD, MSE)
row.names(accuracy.table) = c("Moving Average", "Naive", "Seasonal Naive", "Drift")
pander(accuracy.table, caption="Overall Performance of the Four Forecasting Methods")
