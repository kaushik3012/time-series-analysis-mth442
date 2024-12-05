
# Load the necessary library
library(astsa)

# Log transformation of the data
Xt <- log(jj)

# Create time variable
time <- seq(1960, 1980.75, by=0.25)

# Create indicator variables for quarters
Q1 <- ifelse(cycle(jj) == 1, 1, 0)
Q2 <- ifelse(cycle(jj) == 2, 1, 0)
Q3 <- ifelse(cycle(jj) == 3, 1, 0)
Q4 <- ifelse(cycle(jj) == 4, 1, 0)

# Fit the regression model
model <- lm(Xt ~ time + Q1 + Q2 + Q3 + Q4 - 1) # -1 removes the intercept

# Estimated average annual increase in logged earnings per share
beta_estimate <- coef(model)["time"]
beta_estimate

# Change from third to fourth quarter
alpha3 <- coef(model)["Q3"]
alpha4 <- coef(model)["Q4"]

percentage_change <- (exp(alpha4 - alpha3) - 1) * 100
percentage_change

# Plot the data and fitted values
plot(time, Xt, type = "l", main = "Logged Earnings per Share with Fitted Values", ylab = "Log(Earnings per Share)")
lines(time, fitted(model), col = "red")

# Examine residuals
residuals <- resid(model)

plot(time, residuals, type = "h", main = "Residuals", ylab = "Residuals")
abline(h = 0, col = "blue")


# Load the required package and data
library(astsa)
data(jj)

# Log-transform the data
Xt <- log(jj)

# Create a time variable for quarters
time <- seq(1960, 1980.75, by=0.25)

# Create indicator variables for quarters
Q1 <- ifelse(cycle(jj) == 1, 1, 0)
Q2 <- ifelse(cycle(jj) == 2, 1, 0)
Q3 <- ifelse(cycle(jj) == 3, 1, 0)
Q4 <- ifelse(cycle(jj) == 4, 1, 0)

# Fit the regression model
model <- lm(Xt ~ time + Q1 + Q2 + Q3 + Q4-1)

# Extract the coefficients
beta_estimate <- coef(model)["time"]
alpha3 <- coef(model)["Q3"]
alpha4 <- coef(model)["Q4"]

# Calculate the percentage change from Q3 to Q4
percentage_change <- ((alpha4 - alpha3) / alpha3) * 100

# Plot the data and fitted values
plot(time, Xt, type="l", col="blue", ylab="Logged Earnings per Share", xlab="Time (Years)")
lines(time, fitted(model), col="red", lty=2)
legend("topleft", legend=c("Observed Xt", "Fitted Values"), col=c("blue", "red"), lty=c(1, 2))

# Examine residuals
residuals <- resid(model)
plot(time, residuals, type="l", col="green", ylab="Residuals", xlab="Time (Years)")
abline(h=0, col="black")

# Summary of the model
summary(model)

# Output the results
cat("Estimated average annual increase in logged earnings per share:", beta_estimate, "\n")
cat("Percentage change from Q3 to Q4:", percentage_change, "%\n")