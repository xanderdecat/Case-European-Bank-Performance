# CAPM betas in `beta_data` see Q1
# CAPM regression summaries in `capm_results` see Q1

# Initialize a data frame to store idiosyncratic risk (residual variance) for each bank
idiosyncratic_risk_data <- data.frame(Bank = character(), ISO_Year = integer(), Idiosyncratic_Variance = numeric(), stringsAsFactors = FALSE)

# Loop through each bank to extract idiosyncratic risk from CAPM model residuals
for (bank in names(capm_results)) {
  
  # Get the CAPM model residuals for the bank
  capm_residuals <- capm_results[[bank]]$residuals
  
  # Calculate residual variance for each year
  residual_variance_by_year <- data.frame(
    ISO_Year = unique(yearly_betas$ISO_Year),
    Idiosyncratic_Variance = sapply(unique(yearly_betas$ISO_Year), function(year) {
      year_residuals <- capm_residuals[yearly_betas$ISO_Year == year]
      var(year_residuals, na.rm = TRUE)
    })
  )
  
  # Append bank name and bind to main idiosyncratic_risk_data
  residual_variance_by_year$Bank <- bank
  idiosyncratic_risk_data <- bind_rows(idiosyncratic_risk_data, residual_variance_by_year)
}

# Plot the evolution of betas over time for each bank
ggplot(beta_data, aes(x = ISO_Year, y = Beta, color = Bank)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly CAPM Betas for Selected European Banks",
       x = "Year",
       y = "Beta",
       color = "Bank") +
  theme_minimal()

# Plot idiosyncratic risk (residual variance) over time for each bank
ggplot(idiosyncratic_risk_data, aes(x = ISO_Year, y = Idiosyncratic_Variance, color = Bank)) +
  geom_line() +
  geom_point() +
  labs(title = "Evolution of Idiosyncratic Risk (Residual Variance) Over Time",
       x = "Year",
       y = "Idiosyncratic Variance",
       color = "Bank") +
  theme_minimal()

# Beta 3 years found on Orbis 
reported_betas <- data.frame(
  Bank = c("BNP_PARIBAS", "CAIXABANK", "SKANDINAVISKA_ENSKILDA_BANKEN_A",
           "ABN_AMRO_BANK", "BANK_OF_IRELAND_GROUP", "ALPHA_SERVICES_AND_HOLDINGS",
           "BANCA_MONTE_DEI_PASCHI"),
  ISO_Year = 2024,
  Reported_Beta = c(1.23, 1.04, 0.99, 0.91657, 1.22, 0.81, 1.28)
)

# Merge calculated and reported betas for comparison
beta_comparison <- beta_data %>%
  left_join(reported_betas, by = c("Bank", "ISO_Year"))

# Visualize calculated vs reported betas
library(ggplot2)

ggplot(beta_comparison, aes(x = ISO_Year)) +
  geom_line(aes(y = Beta, color = "Calculated Beta"), size = 1) +
  geom_point(aes(y = Beta, color = "Calculated Beta"), size = 2) +
  geom_line(aes(y = Reported_Beta, color = "Reported Beta"), linetype = "dashed", size = 1) +
  geom_point(aes(y = Reported_Beta, color = "Reported Beta"), size = 2, shape = 4) +
  facet_wrap(~ Bank, scales = "free_y") +
  labs(
    title = "Comparison of Calculated vs Reported Betas Over Time for Each Bank",
    x = "Year",
    y = "Beta",
    color = "Beta Type"
  ) +
  theme_minimal()

