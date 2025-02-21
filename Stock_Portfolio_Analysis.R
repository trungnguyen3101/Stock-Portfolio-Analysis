# Stock Return and Portfolio Analysis in R

# ============================
# 1. Load Required Libraries
# ============================

# Install and load necessary packages
install.packages("readr", dependencies = TRUE)
library(readr)

install.packages("formattable", dependencies = TRUE)
library(formattable)

install.packages("moments", dependencies = TRUE)
library(moments)

install.packages("quadprog", dependencies = TRUE)
library(quadprog)

install.packages("lubridate", dependencies = TRUE)
library(lubridate)

# ============================
# 2. Data Loading
# ============================

# Load stock data
Stock <- read_csv("Stock.csv")

# Load Fama-French 3 factors data
FF3 <- read_csv("FF3factors.csv")
colnames(FF3) <- c('Date', 'Mkt_RF', 'SMB', 'HML', 'RF')
FF3$Date <- ymd(FF3$Date)

# ============================
# 3. Calculate Log Returns
# ============================

# Compute log returns for MSFT, MMM, INTC
lr_MSFT <- diff(log(Stock$MSFT))
lr_MMM <- diff(log(Stock$MMM))
lr_INTC <- diff(log(Stock$INTC))

# Convert log returns to percentages
lr_MSFT_perc <- percent(lr_MSFT)
lr_MMM_perc <- percent(lr_MMM)
lr_INTC_perc <- percent(lr_INTC)

# ============================
# 4. Descriptive Statistics
# ============================

# Function to compute descriptive stats
descriptive_stats <- function(x) {
  c(
    Min = min(x),
    Max = max(x),
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x),
    N = length(x)
  )
}

# Apply function to each stock
stats_MSFT <- descriptive_stats(lr_MSFT_perc)
stats_MMM <- descriptive_stats(lr_MMM_perc)
stats_INTC <- descriptive_stats(lr_INTC_perc)

# Combine into a table
stats_table <- data.frame(MSFT = stats_MSFT, MMM = stats_MMM, INTC = stats_INTC)
print(stats_table)

# ============================
# 5. Correlation Matrix
# ============================

log_returns <- data.frame(MSFT = lr_MSFT_perc, MMM = lr_MMM_perc, INTC = lr_INTC_perc)
cor_matrix <- cor(log_returns)
print(cor_matrix)

# ============================
# 6. Regression Analysis
# ============================

# Merge stock data with Fama-French factors
Stock$Date <- as.Date(Stock$Date, format = "%d/%m/%Y")
merged_data <- merge(Stock, FF3, by = "Date")

# Calculate excess returns
merged_data$StockMSFT <- lr_MSFT - merged_data$RF
merged_data$StockMMM <- lr_MMM - merged_data$RF
merged_data$StockINTC <- lr_INTC - merged_data$RF

# Regression with MKTRF only
model_MSFT <- lm(StockMSFT ~ Mkt_RF, data = merged_data)
model_MMM <- lm(StockMMM ~ Mkt_RF, data = merged_data)
model_INTC <- lm(StockINTC ~ Mkt_RF, data = merged_data)

summary(model_MSFT)
summary(model_MMM)
summary(model_INTC)

# Regression with MKTRF, SMB, HML
model_MSFT_ff3 <- lm(StockMSFT ~ Mkt_RF + SMB + HML, data = merged_data)
model_MMM_ff3 <- lm(StockMMM ~ Mkt_RF + SMB + HML, data = merged_data)
model_INTC_ff3 <- lm(StockINTC ~ Mkt_RF + SMB + HML, data = merged_data)

summary(model_MSFT_ff3)
summary(model_MMM_ff3)
summary(model_INTC_ff3)

# ============================
# 7. Portfolio Optimization
# ============================

# Calculate mean and covariance
mean_returns <- colMeans(log_returns)
cov_matrix <- cov(log_returns)

# Set up for optimization
Amat <- cbind(rep(1, 3), mean_returns)
beq <- c(1, 0.0005)

# No short sale constraint
result_noshort <- solve.QP(Dmat = 2 * cov_matrix, dvec = rep(0, 3), Amat = Amat, bvec = beq, meq = 2)
weights_noshort <- result_noshort$solution
portfolio_return <- sum(weights_noshort * mean_returns)
portfolio_std <- sqrt(t(weights_noshort) %*% cov_matrix %*% weights_noshort)
sharpe_ratio <- (portfolio_return - 0.0002) / portfolio_std
print(sharpe_ratio)

# ============================
# 8. Efficient Frontier
# ============================

mu_p <- seq(min(mean_returns) * 0.75, max(mean_returns) * 1.25, length = 500)
weights <- matrix(0, 500, 3)
sigma_p <- rep(0, 500)

for (i in 1:500) {
  beq <- c(1, mu_p[i])
  result <- solve.QP(Dmat = 2 * cov_matrix, dvec = rep(0, 3), Amat = Amat, bvec = beq, meq = 2)
  weights[i, ] <- result$solution
  sigma_p[i] <- sqrt(t(result$solution) %*% cov_matrix %*% result$solution)
}

# Plot Efficient Frontier
plot(sigma_p, mu_p, type = "l", col = "blue", lwd = 2, xlab = "Risk (Std Dev)", ylab = "Return", main = "Efficient Frontier")

# ============================
# 9. Task 2: Loops and Conditionals
# ============================

# Pattern Printing
n <- 20
for (i in 1:n) {
  if (i %% 2 == 1) {
    print(seq((i + 1) * i / 2, (i - 1) * i / 2 + 1))
  } else {
    print(seq((i - 1) * i / 2 + 1, (i + 1) * i / 2))
  }
}

# Pokemon Card Collection Simulation
cardcollect <- function(n) {
  collected <- numeric(n)
  count <- 0
  while (sum(collected) < n) {
    i <- sample(1:n, 1)
    collected[i] <- 1
    count <- count + 1
  }
  return(count)
}

# Run 1000 simulations
set.seed(123)
results <- replicate(1000, cardcollect(48))

# Plot histogram
hist(results, breaks = 50, main = "Pokemon Card Collection Simulation", xlab = "Number of Cards Bought", col = "skyblue")

# Print average
cat("Average number of cards needed:", mean(results), "\n")
