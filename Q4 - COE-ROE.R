# Compare COE and ROE

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

# Estimation of expected ROE
