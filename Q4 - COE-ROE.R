# Compare COE and ROE
install.packages("readxl")
library(readxl)

## 1.COE
# Estimation of COE using CAPM
# Define constants
risk_free_rate <- 0.02  # Assume 2% risk-free rate, adjust as necessary
equity_market_premium <- 0.06  # Assume 6% equity market premium

# Calculate COE for each bank based on beta values over time
coefficient_data <- beta_data %>%
  mutate(
    COE = risk_free_rate + Beta * equity_market_premium
  )

# Filter COE for the next 3 years (assuming recent years data is available and projections)
next_3_years_coe <- coefficient_data %>%
  filter(ISO_Year %in% c(2022, 2023, 2024))

## 2. ROE
# Install and load necessary package
if (!require(forecast)) install.packages("forecast", dependencies = TRUE)
library(forecast)

# Load the data
roe_data <- read_excel("C:\\Users\\Anna\\OneDrive - UGent\\Documenten\\Management of Financial Institutions\\European Bank Analysis\\ROE.xlsx", sheet = "ROE")

# Initialize a list to store forecast results for each bank
forecast_results <- list()
  
# Loop through each bank column and forecast ROE using linear regression
for (bank in colnames(roe_data)[-1]) {  # Skip the 'Year' column
  if (sum(!is.na(roe_data[[bank]])) > 1) {  # Ensure there is enough data
    years <- roe_data$Year
    roe_values <- roe_data[[bank]]
    
    # Fit a linear regression model
    model <- lm(roe_values ~ years)
    
    # Generate predictions for the next 3 years
    new_years <- data.frame(years = (max(roe_data$Year) + 1):(max(roe_data$Year) + 3))
    forecasted_df[[bank]] <- predict(model, newdata = new_years)
  } else {
    # If not enough data, set forecast to NA
    forecasted_df[[bank]] <- rep(NA, 3)
    warning(paste("Not enough data to forecast for", bank))
  }
}

# Print the forecasted data frame
print(forecasted_df)

## Growth rates
# Create a vector to store the growth rate (CAGR) for each bank
growth_rates <- c()

# Calculate CAGR for each bank
for (bank in colnames(roe_data)[-1]) {  # Skip the 'Year' column
  # Get non-missing data points for the bank
  non_missing_values <- roe_data[[bank]][!is.na(roe_data[[bank]])]
  
  # Ensure there is enough data to calculate growth rate
  if (length(non_missing_values) > 1) {
    start_value <- non_missing_values[1]
    end_value <- tail(non_missing_values, 1)
    num_years <- length(non_missing_values)
    
    # Calculate CAGR
    cagr <- (end_value / start_value)^(1 / (num_years - 1)) - 1
    
    # Store the CAGR in the vector
    growth_rates[bank] <- cagr
  } else {
    # If not enough data, set growth rate to NA
    growth_rates[bank] <- NA
    warning(paste("Not enough data to calculate growth rate for", bank))
  }
}

# Display the growth rates for each bank
growth_rates

