### Case European Bank Performance

## STEP 1: Import Libraries and Load Data

# Install necessary libraries if not already installed
install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("broom")

# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)

# Set the path to your dataset
file_path <- "/Users/xanderdecat/Documents/Ghent University/05 UGent 2e Master HIR/Management of Financial Institutions/Data_European banks_14102024.xlsx"

# Load the data
market_data <- read_excel(file_path, sheet = "Market")
fama_french_data <- read_excel(file_path, sheet = "Europe_3_Factors_Daily", skip = 1)
ri_banks <- read_excel(file_path, sheet = "RI_banks")

# Clean and format market_data
colnames(market_data)[1] <- "Date"
market_data$Date <- as.Date(market_data$Date)

# Clean and format Fama-French data
colnames(fama_french_data) <- c("Date", "Mkt_RF", "SMB", "HML", "RF")
fama_french_data$Date <- as.Date(as.character(fama_french_data$Date), format = "%Y%m%d")
fama_french_data <- fama_french_data %>%
  filter(Date >= as.Date("2007-12-31") & Date <= as.Date("2024-10-31"))

# Clean and format RI_banks
colnames(ri_banks)[1] <- "Date"
ri_banks$Date <- as.Date(ri_banks$Date)

## STEP 2: Calculate Weekly Returns for Banks, Market, and Fama-French Factors

# Select relevant banks and add ISO Year/Week columns
selected_banks <- ri_banks %>%
  select(Date, `BNP PARIBAS`, `CAIXABANK`, `SKANDINAVISKA ENSKILDA BANKEN A`, 
         `ABN AMRO BANK`, `BANK OF IRELAND GROUP`, `ALPHA SERVICES AND HOLDINGS`, 
         `BANCA MONTE DEI PASCHI`) %>%
  mutate(ISO_Year = isoyear(Date), ISO_Week = isoweek(Date))

# Clean and format selected_banks
colnames(selected_banks) <- c("Date", "BNP_PARIBAS", "CAIXABANK", "SKANDINAVISKA_ENSKILDA_BANKEN_A", "ABN_AMRO_BANK", "BANK_OF_IRELAND_GROUP", "ALPHA_SERVICES_AND_HOLDINGS", "BANCA_MONTE_DEI_PASCHI", "ISO_Year", "ISO_Week")

# Calculate weekly returns for each bank
weekly_bank_returns <- selected_banks %>%
  group_by(ISO_Year, ISO_Week) %>%
  summarize(
    last_date = max(Date),
    across(-Date, ~ last(.), .names = "last_{col}"),
    .groups = 'drop'
  ) %>%
  arrange(ISO_Year, ISO_Week) %>%
  mutate(across(starts_with("last_") & !contains("last_date"), ~ log(. / lag(.)), .names = "return_{col}"))

# Calculate weekly returns for the "STOXX EUROPE 600 E" market index
market_weekly_returns <- market_data %>%
  mutate(ISO_Year = isoyear(Date), ISO_Week = isoweek(Date)) %>%
  group_by(ISO_Year, ISO_Week) %>%
  summarize(
    last_date = max(Date),
    last_market_index = last(`STOXX EUROPE 600 E`),
    .groups = 'drop'
  ) %>%
  arrange(ISO_Year, ISO_Week) %>%
  mutate(weekly_market_return = log(last_market_index / lag(last_market_index)))

# Aggregate Fama-French factors to weekly frequency
fama_french_weekly <- fama_french_data %>%
  mutate(ISO_Year = isoyear(Date), ISO_Week = isoweek(Date)) %>%
  group_by(ISO_Year, ISO_Week) %>%
  summarize(
    last_date = max(Date),
    Mkt_RF_weekly = last(Mkt_RF),
    SMB_weekly = last(SMB),
    HML_weekly = last(HML),
    RF_weekly = last(RF),
    .groups = 'drop'
  )

## STEP 3: Perform CAPM and Fama-French Regressions for Each Bank

# List of bank names
banks <- c("BNP_PARIBAS", "CAIXABANK", "SKANDINAVISKA_ENSKILDA_BANKEN_A",
           "ABN_AMRO_BANK", "BANK_OF_IRELAND_GROUP", "ALPHA_SERVICES_AND_HOLDINGS",
           "BANCA_MONTE_DEI_PASCHI")

# Convert all return columns to numeric in case any were misinterpreted
weekly_bank_returns <- weekly_bank_returns %>%
  mutate(across(starts_with("return_last_"), as.numeric))

# Initialize lists to store CAPM and Fama-French regression summaries
capm_results <- list()
fama_french_results <- list()

# Loop through each bank to perform regressions
for (bank in banks) {
  
  # Select weekly returns for the bank
  bank_weekly_returns <- weekly_bank_returns %>%
    select(ISO_Year, ISO_Week, paste0("return_last_", bank))
  
  # Merge with market returns and risk-free rate
  capm_data <- bank_weekly_returns %>%
    inner_join(market_weekly_returns %>% select(ISO_Year, ISO_Week, weekly_market_return),
               by = c("ISO_Year", "ISO_Week")) %>%
    inner_join(fama_french_weekly %>% select(ISO_Year, ISO_Week, RF_weekly),
               by = c("ISO_Year", "ISO_Week")) %>%
    mutate(
      excess_bank_return = as.numeric(get(paste0("return_last_", bank))) - as.numeric(RF_weekly),
      excess_market_return = as.numeric(weekly_market_return) - as.numeric(RF_weekly)
    )
  
  # Run CAPM regression: Excess Bank returns ~ Excess Market returns
  capm_model <- lm(excess_bank_return ~ excess_market_return, data = capm_data)
  capm_results[[bank]] <- summary(capm_model)
  
  # Merge with Fama-French factors
  ff_data <- capm_data %>%
    inner_join(fama_french_weekly %>% select(ISO_Year, ISO_Week, Mkt_RF_weekly, SMB_weekly, HML_weekly),
               by = c("ISO_Year", "ISO_Week"))
  
  # Run Fama-French regression: Excess Bank returns ~ Market + SMB + HML
  ff_model <- lm(excess_bank_return ~ excess_market_return + SMB_weekly + HML_weekly, data = ff_data)
  fama_french_results[[bank]] <- summary(ff_model)
}

# View CAPM and Fama-French summary for BNP Paribas as an example
capm_results[["BNP_PARIBAS"]]
fama_french_results[["BNP_PARIBAS"]]

## STEP 4: Visualize Results

## Initialize an empty dataframe to store CAPM betas for visualization
beta_data <- data.frame(Bank = character(), ISO_Year = integer(), Beta = numeric(), stringsAsFactors = FALSE)

for (bank in banks) {
  # Select weekly returns for the bank
  bank_weekly_returns <- weekly_bank_returns %>%
    select(ISO_Year, ISO_Week, paste0("return_last_", bank))
  
  # Merge with market weekly returns
  capm_data <- bank_weekly_returns %>%
    inner_join(market_weekly_returns %>% select(ISO_Year, ISO_Week, weekly_market_return),
               by = c("ISO_Year", "ISO_Week"))
  
  # Remove any rows with NA values
  capm_data <- capm_data %>%
    filter(!is.na(get(paste0("return_last_", bank))) & !is.na(weekly_market_return))
  
  # Calculate yearly betas for the bank using CAPM
  yearly_betas <- capm_data %>%
    group_by(ISO_Year) %>%
    do({
      data <- .
      # Check if there are sufficient non-NA rows to perform regression
      if (nrow(data) > 1) {  # Ensure at least two points for regression
        model <- lm(as.formula(paste0("return_last_", bank, " ~ weekly_market_return")), data = data)
        tidy(model) %>%
          filter(term == "weekly_market_return") %>%
          select(estimate) %>%
          rename(Beta = estimate) %>%
          mutate(ISO_Year = unique(data$ISO_Year))
      } else {
        # Return an empty data frame if insufficient data for regression
        data.frame(Beta = NA, ISO_Year = unique(data$ISO_Year))
      }
    }) %>%
    filter(!is.na(Beta)) %>%  # Filter out NA betas
    mutate(Bank = bank)
  
  # Append the yearly betas to the main beta_data dataframe
  beta_data <- bind_rows(beta_data, yearly_betas)
}

# Plot yearly CAPM betas for each bank
ggplot(beta_data, aes(x = ISO_Year, y = Beta, color = Bank)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly CAPM Betas for Selected European Banks",
       x = "Year",
       y = "Beta",
       color = "Bank") +
  theme_minimal()

## STEP 5: Calculating Performance Metrics

# Initialize a data frame to store the performance metrics
performance_metrics <- data.frame(
  Bank = character(),
  Avg_Weekly_Return = numeric(),
  Std_Dev_Weekly_Return = numeric(),
  Sharpe_Ratio = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each bank to calculate performance metrics
for (bank in banks) {
  # Select weekly returns for the bank and rename the column temporarily
  bank_returns <- weekly_bank_returns %>%
    select(ISO_Year, ISO_Week, Weekly_Return = paste0("return_last_", bank)) %>%
    inner_join(fama_french_weekly %>% select(ISO_Year, ISO_Week, RF_weekly), 
               by = c("ISO_Year", "ISO_Week")) %>%
    mutate(
      RF_weekly = as.numeric(RF_weekly),  # Convert RF_weekly to numeric if needed
      excess_return = Weekly_Return - RF_weekly  # Calculate excess return
    )
  
  
  # Calculate metrics
  avg_return <- mean(bank_returns$excess_return, na.rm = TRUE)
  std_dev_return <- sd(bank_returns$excess_return, na.rm = TRUE)
  sharpe_ratio <- avg_return / std_dev_return  # Sharpe ratio calculation
  
  # Append the metrics to the data frame
  performance_metrics <- rbind(performance_metrics, data.frame(
    Bank = bank,
    Avg_Weekly_Return = avg_return,
    Std_Dev_Weekly_Return = std_dev_return,
    Sharpe_Ratio = sharpe_ratio
  ))
}

# View the performance metrics for each bank
print(performance_metrics)
