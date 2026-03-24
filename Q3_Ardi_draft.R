# Remove all objects from the current environment
rm(list = ls())

# Define parameters
S0 <- 392.74      # Current stock price
K  <- 1.05*S0       # Strike price
T  <- 1      # Time to expiration (in years) = time to maturity ??
n  <- 10      # 
r  <- 0.038     # Risk-free interest rate
sigma  <- 0.269077702 # Volatility (25%)

#b  <- r        # Cost of carry (same as r for stock options)

dt <- T / n  # Time step
u <- exp(sigma * sqrt(dt))  # Up factor
d <- 1 / u  # Down factor
q <- (exp(r * dt) - d) / (u - d)  # Risk-neutral probability
discount <- exp(-r * dt)  # Discount factor


# Initialize stock price tree
stock_prices <- matrix(0, nrow = n + 1, ncol = n + 1)
for (i in 0:n) {
  for (j in 0:i) {
    stock_prices[j + 1, i + 1] <- S0 * (u^j) * (d^(i - j))
  }
}


# Initialize option value tree
option_values <- matrix(0, nrow = n + 1, ncol = n + 1)
for (j in 0:n) {
  option_values[j + 1, n + 1] <- max(0, K - stock_prices[j + 1, n + 1]) # put option value
}

# Backward induction to calculate option price
for (i in (n - 1):0) {
  for (j in 0:i) {
    option_values[j + 1, i + 1] <- discount * (q * option_values[j + 2, i + 2] + (1 - q) * option_values[j + 1, i + 2])
  }
}

# Output option price
cat(paste0("The current price of the European put option using the binomial tree model is $", 
           format(option_values[1, 1], digits=5), ".\n"))

#45.036

