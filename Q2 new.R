rm(list = ls())

getwd()
setwd("/Users/charlinevanhaute/Documents/Management fin inst")


# Installeer pakketten als je ze nog niet hebt
install.packages(c("tidyverse", "readxl", "lubridate"))

# Laad de pakketten
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)

# Importeer de data vanuit een Excel-bestand (pas het pad en het blad aan naar jouw data)
balance_sheet_BNP<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-05-11-2024-BNP.xlsx")
balance_sheet_CAIXA<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-05-11-2024-CAIXA.xlsx")
balance_sheet_SKANDINAVISKA<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-05-11-2024-SKANDINAVISKA.xlsx")
balance_sheet_ABNAMRO<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-05-11-2024-ABNAMRO.xlsx")
balance_sheet_BANKOFIRELAND<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-05-11-2024-BANKofIRELAND.xlsx")
balance_sheet_ALPHA<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-05-11-2024-ALPHA.xlsx")
balance_sheet_BANCA_MONTE<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-05-11-2024-BANCA-MONTEI.xlsx")

### STAP 1 BOOK VALUES
#Equity en datum uit balancesheets halen 
equity_BNP <- balance_sheet_BNP %>%
  slice(14,132)
equity_CAIXA <- balance_sheet_CAIXA %>%
  slice(14,111)
equity_SKANDINAVISKA <- balance_sheet_SKANDINAVISKA %>%
  slice(15,121)
equity_ABNAMRO <- balance_sheet_ABNAMRO %>%
  slice(14,132)
equity_BANKOFIRELAND <- balance_sheet_BANKOFIRELAND %>%
  slice(14,125)
equity_ALPHA <- balance_sheet_ALPHA %>%
  slice(14,124)
equity_BANCAMONTE <- balance_sheet_BANCA_MONTE %>%
  slice(14,105)

# Transponeer het dataframe
equity_BNP <- t(equity_BNP)
equity_CAIXA <- t(equity_CAIXA)
equity_SKANDINAVISKA <- t(equity_SKANDINAVISKA)
equity_ABNAMRO <- t(equity_ABNAMRO)
equity_BANKOFIRELAND <- t(equity_BANKOFIRELAND)
equity_ALPHA <- t(equity_ALPHA)
equity_BANCAMONTE <- t(equity_BANCAMONTE)



# Wijzig kolomnamen
# Wijzig de kolomnamen direct met names()
names(equity_BNP)[1:2] <- c("Date", "ShareholdersEquity_BNP")
names(equity_CAIXA)[1:2] <- c("Date", "ShareholdersEquity_CAIXA")
names(equity_SKANDINAVISKA)[1:2] <- c("Date", "ShareholdersEquity_SKANDINAVISKA")
names(equity_ABNAMRO)[1:2] <- c("Date", "ShareholdersEquity_ABNAMRO")
names(equity_BANKOFIRELAND)[1:2] <- c("Date", "ShareholdersEquity_BANKOFIRELAND")
names(equity_ALPHA)[1:2] <- c("Date", "ShareholdersEquity_ALPHA")
names(equity_BANCAMONTE)[1:2] <- c("Date", "ShareholdersEquity_BANCAMONTE")



#omzetten in dataframe
equity_BNP <- as.data.frame(equity_BNP)
equity_CAIXA <- as.data.frame(equity_CAIXA)
equity_SKANDINAVISKA <- as.data.frame(equity_SKANDINAVISKA)
equity_ABNAMRO <- as.data.frame(equity_ABNAMRO)
equity_BANKOFIRELAND <- as.data.frame(equity_BANKOFIRELAND)
equity_ALPHA <- as.data.frame(equity_ALPHA)
equity_BANCAMONTE <- as.data.frame(equity_BANCAMONTE)

# Zet de 'Date' kolom om naar een datumtype
equity_BNP$Date <- as.Date(equity_BNP$Date, format = "%d-%m-%Y")
equity_CAIXA$Date <- as.Date(equity_CAIXA$Date, format = "%d-%m-%Y")
equity_SKANDINAVISKA$Date <- as.Date(equity_SKANDINAVISKA$Date, format = "%d-%m-%Y")
equity_ABNAMRO$Date <- as.Date(equity_ABNAMRO$Date, format = "%d-%m-%Y")
equity_BANKOFIRELAND$Date <- as.Date(equity_BANKOFIRELAND$Date, format = "%d-%m-%Y")
equity_ALPHA$Date <- as.Date(equity_ALPHA$Date, format = "%d-%m-%Y")
equity_BANCAMONTE$Date <- as.Date(equity_BANCAMONTE$Date, format = "%d-%m-%Y")

#Lijsten samenvoegen

#Lijsten samenvoegen
equity_list <- list(equity_BNP, equity_CAIXA, equity_SKANDINAVISKA, equity_ABNAMRO, equity_BANKOFIRELAND, equity_ALPHA, equity_BANCAMONTE)
shareholdersequity <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), equity_list)
shareholdersequity <- shareholdersequity[-nrow(shareholdersequity), ]

##eenheden corrigeren
# Zet de ShareholdersEquity kolommen om van duizenden naar eenheden
shareholdersequity <- shareholdersequity %>%
  mutate(
    ShareholdersEquity_BNP = as.numeric(ShareholdersEquity_BNP) * 1000,
    ShareholdersEquity_CAIXA = as.numeric(ShareholdersEquity_CAIXA) * 1000,
    ShareholdersEquity_SKANDINAVISKA = as.numeric(ShareholdersEquity_SKANDINAVISKA) * 1000,
    ShareholdersEquity_ABNAMRO = as.numeric(ShareholdersEquity_ABNAMRO) * 1000,
    ShareholdersEquity_BANKOFIRELAND = as.numeric(ShareholdersEquity_BANKOFIRELAND) * 1000,
    ShareholdersEquity_ALPHA = as.numeric(ShareholdersEquity_ALPHA) * 1000,
    ShareholdersEquity_BANCAMONTE = as.numeric(ShareholdersEquity_BANCAMONTE) * 1000
  )







# Bekijk het resultaat om te controleren
print(shareholdersequity)



###STAP 2 MARKET CAPITALIZATION

# Set the path to your dataset
file_path <- "/Users/charlinevanhaute/Documents/Management fin inst/R-code/Data_European banks_14102024.xlsx"
ri_banks <- read_excel(file_path, sheet = "RI_banks")
P_banks <- read_excel(file_path, sheet = "P_banks")

# Clean and format RI_banks
colnames(ri_banks)[1] <- "Date"
ri_banks$Date <- as.Date(ri_banks$Date)
colnames(P_banks)[1] <- "Date"
P_banks$Date <- as.Date(P_banks$Date)


# Selecteer de laatste handelsdag van elk jaar voor elke bank
P_banks_year_end <- P_banks %>%
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>%
  filter(Date == max(Date)) %>%  # Selecteer de laatste handelsdag van elk jaar
  ungroup()
colnames(P_banks_year_end)

"BNP PARIBAS (~E )"
"CAIXABANK (~E )" 
"ABN AMRO BANK (~E )" 
"SKANDINAVISKA ENSKILDA BANKEN A (~E )" 
"BANK OF IRELAND GROUP (~E )"
"ALPHA SERVICES AND HOLDINGS (~E )"
"BANCA MONTE DEI PASCHI (~E )"

P_banks_year_end <- P_banks_year_end %>%
  select(Date, 
         `BNP PARIBAS (~E )`,
         `CAIXABANK (~E )`, 
         `ABN AMRO BANK (~E )`, 
         `SKANDINAVISKA ENSKILDA BANKEN A (~E )`, 
         `BANK OF IRELAND GROUP (~E )`, 
         `ALPHA SERVICES AND HOLDINGS (~E )`,
         `BANCA MONTE DEI PASCHI (~E )`)

# Importeer de data vanuit een Excel-bestand (pas het pad en het blad aan naar jouw data)
financials_BNP<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-10-11-2024-BPN-shares.xlsx")
financials_CAIXA<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-10-11-2024-CAIXA-shares.xlsx")
financials_SKANDINAVISKA<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-10-11-2024-Skandinaviska-shares.xlsx")
financials_ABNAMRO<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-10-11-2024-ABNAMBRO-shares.xlsx")
financials_BANKOFIRELAND<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-10-11-2024-Bankofireland-shares.xlsx")
financials_ALPHA<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-10-11-2024-Alpha-shares.xlsx")
financials_BANCA_MONTE<- read_excel("/Users/charlinevanhaute/Documents/Management fin inst/R-code/CF-Export-10-11-2024 (6).xlsx")


# Definieer een functie om de jaren op te halen en de kolommen aan te passen op basis van de jaren
clean_financials <- function(data, bank_name) {
  # Haal de jaren op uit de "Statement Data" rij
  years <- data %>%
    filter(`Company Fundamentals - Financial Summary` == "Statement Data") %>%
    select(-`Company Fundamentals - Financial Summary`) %>%
    unlist() %>%
    as.integer()
  
  # Gebruik de "Common Shares - Outstanding - Total" rij en pas de kolomnamen aan op basis van de jaren
  cleaned_data <- data %>%
    filter(`Company Fundamentals - Financial Summary` == "Common Shares - Outstanding - Total") %>%
    select(-`Company Fundamentals - Financial Summary`) %>%
    setNames(years) %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = paste0("Shares_", bank_name)) %>%
    mutate(Year = as.integer(Year), !!paste0("Shares_", bank_name) := as.numeric(.data[[paste0("Shares_", bank_name)]]))
  
  return(cleaned_data)
}

# Pas de functie toe voor elke bank
financials_BNP_clean <- clean_financials(financials_BNP, "BNP")
financials_CAIXA_clean <- clean_financials(financials_CAIXA, "CAIXA")
financials_SKANDINAVISKA_clean <- clean_financials(financials_SKANDINAVISKA, "SKANDINAVISKA")
financials_ABNAMRO_clean <- clean_financials(financials_ABNAMRO, "ABNAMRO")
financials_BANKOFIRELAND_clean <- clean_financials(financials_BANKOFIRELAND, "BANKOFIRELAND")
financials_ALPHA_clean <- clean_financials(financials_ALPHA, "ALPHA")
financials_BANCA_MONTE_clean <- clean_financials(financials_BANCA_MONTE, "BANCA_MONTE")

# Bekijk een voorbeeld om te controleren of de data correct is verwerkt
print(financials_BNP_clean)
print(financials_CAIXA_clean)



# Combineer de dataframes in één overzicht
shares_data <- financials_BNP_clean %>%
  left_join(financials_CAIXA_clean, by = "Year") %>%
  left_join(financials_SKANDINAVISKA_clean, by = "Year") %>%
  left_join(financials_ABNAMRO_clean, by = "Year") %>%
  left_join(financials_BANKOFIRELAND_clean, by = "Year") %>%
  left_join(financials_ALPHA_clean, by = "Year") %>%
  left_join(financials_BANCA_MONTE_clean, by = "Year")

# Zet de Shares kolommen om van miljoenen naar eenheden in shares_data
shares_data <- shares_data %>%
  mutate(
    Shares_BNP = as.numeric(Shares_BNP) * 1e6,
    Shares_CAIXA = as.numeric(Shares_CAIXA) * 1e6,
    Shares_SKANDINAVISKA = as.numeric(Shares_SKANDINAVISKA) * 1e6,
    Shares_ABNAMRO = as.numeric(Shares_ABNAMRO) * 1e6,
    Shares_BANKOFIRELAND = as.numeric(Shares_BANKOFIRELAND) * 1e6,
    Shares_ALPHA = as.numeric(Shares_ALPHA) * 1e6,
    Shares_BANCA_MONTE = as.numeric(Shares_BANCA_MONTE) * 1e6
  )

# Bekijk het resultaat om te controleren
print(shares_data)


# Bekijk het gecombineerde data frame
print(shares_data)

shares_data
P_banks_year_end
shareholdersequity


# Voeg het jaar toe aan beide datasets om ze te kunnen koppelen
P_banks_year_end <- P_banks_year_end %>%
  mutate(Year = year(Date))

shares_data <- shares_data %>%
  rename(Year = Year) # Dit is eigenlijk overbodig, je kunt deze regel weglaten

# Controleer of de kolom Year aanwezig is in beide datasets
if (!"Year" %in% colnames(P_banks_year_end)) {
  stop("Kolom 'Year' ontbreekt in P_banks_year_end.")
}
if (!"Year" %in% colnames(shares_data)) {
  stop("Kolom 'Year' ontbreekt in shares_data.")
}

# Combineer P_banks_year_end en shares_data op basis van het jaar
market_cap_data <- P_banks_year_end %>%
  inner_join(shares_data, by = "Year") %>%
  mutate(
    MarketCap_BNP = `BNP PARIBAS (~E )` * Shares_BNP,
    MarketCap_CAIXA = `CAIXABANK (~E )` * Shares_CAIXA,
    MarketCap_SKANDINAVISKA = `SKANDINAVISKA ENSKILDA BANKEN A (~E )` * Shares_SKANDINAVISKA,
    MarketCap_ABNAMRO = `ABN AMRO BANK (~E )` * Shares_ABNAMRO,
    MarketCap_BANKOFIRELAND = `BANK OF IRELAND GROUP (~E )` * Shares_BANKOFIRELAND,
    MarketCap_ALPHA = `ALPHA SERVICES AND HOLDINGS (~E )` * Shares_ALPHA,
    MarketCap_BANCA_MONTE = `BANCA MONTE DEI PASCHI (~E )` * Shares_BANCA_MONTE
  ) %>%
  select(Date, Year, MarketCap_BNP, MarketCap_CAIXA, MarketCap_SKANDINAVISKA,
         MarketCap_ABNAMRO, MarketCap_BANKOFIRELAND, MarketCap_ALPHA, MarketCap_BANCA_MONTE)

## Voeg de market capitalization toe aan het aandeelhoudersvermogen data frame
shareholdersequity

# Voeg een Year-kolom toe aan shareholdersequity op basis van de Date-kolom
shareholdersequity <- shareholdersequity %>%
  mutate(Year = year(Date))

combined_data <- shareholdersequity %>%
  left_join(market_cap_data, by = "Year")

combined_data

# Zorg ervoor dat de ShareholdersEquity en MarketCap kolommen numeriek zijn
combined_data <- combined_data %>%
  mutate(
    ShareholdersEquity_BNP = as.numeric(ShareholdersEquity_BNP),
    ShareholdersEquity_CAIXA = as.numeric(ShareholdersEquity_CAIXA),
    ShareholdersEquity_SKANDINAVISKA = as.numeric(ShareholdersEquity_SKANDINAVISKA),
    ShareholdersEquity_ABNAMRO = as.numeric(ShareholdersEquity_ABNAMRO),
    ShareholdersEquity_BANKOFIRELAND = as.numeric(ShareholdersEquity_BANKOFIRELAND),
    ShareholdersEquity_ALPHA = as.numeric(ShareholdersEquity_ALPHA),
    ShareholdersEquity_BANCAMONTE = as.numeric(ShareholdersEquity_BANCAMONTE),
    MarketCap_BNP = as.numeric(MarketCap_BNP),
    MarketCap_CAIXA = as.numeric(MarketCap_CAIXA),
    MarketCap_SKANDINAVISKA = as.numeric(MarketCap_SKANDINAVISKA),
    MarketCap_ABNAMRO = as.numeric(MarketCap_ABNAMRO),
    MarketCap_BANKOFIRELAND = as.numeric(MarketCap_BANKOFIRELAND),
    MarketCap_ALPHA = as.numeric(MarketCap_ALPHA),
    MarketCap_BANCA_MONTE = as.numeric(MarketCap_BANCA_MONTE)
  )

# Bereken de market-to-book ratio per bank per jaar
market_to_book_data <- combined_data %>%
  mutate(
    MTBR_BNP = MarketCap_BNP / ShareholdersEquity_BNP,
    MTBR_CAIXA = MarketCap_CAIXA / ShareholdersEquity_CAIXA,
    MTBR_SKANDINAVISKA = MarketCap_SKANDINAVISKA / ShareholdersEquity_SKANDINAVISKA,
    MTBR_ABNAMRO = MarketCap_ABNAMRO / ShareholdersEquity_ABNAMRO,
    MTBR_BANKOFIRELAND = MarketCap_BANKOFIRELAND / ShareholdersEquity_BANKOFIRELAND,
    MTBR_ALPHA = MarketCap_ALPHA / ShareholdersEquity_ALPHA,
    MTBR_BANCA_MONTE = MarketCap_BANCA_MONTE / ShareholdersEquity_BANCAMONTE
  ) %>%
  select(Date.x, starts_with("MTBR"))

# Bekijk de market-to-book ratio per jaar per bank
print(market_to_book_data)




###STAP 4 GRAFIEK

library(ggplot2)
library(tidyr)

# Zorg dat je data in een data frame staat (hier wordt aangenomen dat het market_to_book_data heet)
# Zorg dat Date als date wordt gezien
market_to_book_data$Date <- as.Date(market_to_book_data$Date.x)

# Zet de data in lange vorm voor ggplot
market_to_book_long <- market_to_book_data %>%
  pivot_longer(cols = starts_with("MTBR"), names_to = "Bank", values_to = "Market_to_Book_Ratio")

# Maak de grafiek
ggplot(market_to_book_long, aes(x = Date, y = Market_to_Book_Ratio, color = Bank)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Market-to-Book Ratio per Bank per Jaar",
    x = "Jaar",
    y = "Market-to-Book Ratio",
    color = "Bank"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )












