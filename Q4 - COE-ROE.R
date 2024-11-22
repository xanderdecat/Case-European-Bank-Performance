
# Define constants for COE estimation
risk_free_rate <- 0.02  # Assume 2% risk-free rate, adjust as necessary
equity_market_premium <- 0.06  # Assume 6% equity market premium

# Future Beta Data for 2024-2026
future_betas <- data.frame(
  Bank = c("BNP_PARIBAS", "CAIXABANK", "SKANDINAVISKA_ENSKILDA_BANKEN_A",
           "ABN_AMRO_BANK", "BANK_OF_IRELAND_GROUP", "ALPHA_SERVICES_AND_HOLDINGS",
           "BANCA_MONTE_DEI_PASCHI"),
  ISO_Year = rep(c(2024, 2025, 2026), each = 7),
  Beta = c(1.23, 1.04, 0.99, 0, 1.22, 0.81, 1.82,
           1.25933, 0.62399, 0.91467, 0, 0.29133, 0.899733, 1.978,
           1.2039, 0.51, 0.84267, 0, -0.01495, 0.78562, 2.1117)
)

# Display the updated data frame
future_betas


# Add future beta data to existing dataset
beta_data_future <- beta_data %>%
  bind_rows(future_betas)

# Calculate COE for 2024, 2025, and 2026
coefficient_data <- beta_data_future %>%
  mutate(
    COE = risk_free_rate + Beta * equity_market_premium
  ) %>%
  filter(ISO_Year %in% c(2024, 2025, 2026))

# Define projected ROE for each bank for 2024-2026 (example data)
projected_roe <- data.frame(
  Bank = c("BNP_PARIBAS", "CAIXABANK", "SKANDINAVISKA_ENSKILDA_BANKEN_A",
           "ABN_AMRO_BANK", "BANK_OF_IRELAND_GROUP", "ALPHA_SERVICES_AND_HOLDINGS",
           "BANCA_MONTE_DEI_PASCHI"),
  ISO_Year = rep(c(2024, 2025, 2026), each = 7),
  ROE = c( 0.0936 ,  0.0949 ,  0.1004 ,  0.1511,  0.1345 ,  0.1285 ,  0.1605 ,
           0.141 ,  0.1433 ,  0.1004 ,  0.0826 ,  0.0822 ,  0.1368 ,  0.1126 ,
           0.1093 , 0.09, 0.10974, 0.12948,  0.1286 ,  0.0933 ,  0.0979 )
)

# Combine COE and projected ROE data
comparison_data <- coefficient_data %>%
  left_join(projected_roe, by = c("Bank", "ISO_Year"))

# Plot COE vs ROE for each bank for 2024-2026
ggplot(comparison_data, aes(x = ISO_Year, group = Bank)) +
  geom_line(aes(y = COE, color = "COE")) +
  geom_point(aes(y = COE, color = "COE")) +
  geom_line(aes(y = ROE, color = "ROE")) +
  geom_point(aes(y = ROE, color = "ROE")) +
  facet_wrap(~ Bank, scales = "free_y") +
  labs(
    title = "Comparison of Cost of Equity (COE) and Return on Equity (ROE) for 2024-2026",
    x = "Year",
    y = "Rate",
    color = "Metric"
  ) +
  theme_minimal()


