library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

setwd("/Users/siddharth/Downloads/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_370494/")

# Only consider the six countries, header starts on line 5
six_countries <- read.csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_370494.csv",
                          skip = 4) |>
  filter(Country.Name %in% c("Equatorial Guinea", "Ireland", "Qatar",
                             "Russian Federation", "Nauru", "South Africa")) |>
  # Change dimensions such that year is in rows, corresponding to GDP per Capita
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "GDP per Capita (Current USD)") |> 
  mutate(Year = as.numeric(str_remove(Year, "X"))) |>
  # Ignore missing values
  drop_na('GDP per Capita (Current USD)') |> 
  rename(Country = Country.Name) |>
  rename(Code = Country.Code) |>
  select(Country, Code, Year, `GDP per Capita (Current USD)`)

incomes <- 
  read.csv("Metadata_Country_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_370494.csv") |>
  rename(Code = Country.Code) |>
  filter(Code %in% c("GNQ", "IRL", "QAT", "RUS", "NRU", "ZAF")) |>
  select(Code, IncomeGroup)
incomes

# Income category file only indicates country code, map to country name
combined_df <- six_countries |>
  left_join(incomes, by = "Code")

# Create a plot with six lines to represent six different countries 
ggplot(combined_df, aes(x = Year, y = `GDP per Capita (Current USD)`,
                        color = Country)) + 
  geom_line() +
  # Split by income classification
  # Have different y-axis scales to infer ranges of each income category
  facet_wrap(~IncomeGroup, scales = "free_y") +
  labs(title = "GDP per Capita of Six Countries from 1960 to 2024",
       x = "Year", y = "GDP per Capita (Current USD)",
       color = "Country", caption = "World Bank Group") +
  theme_minimal()

# Save the combined data frame
write.csv(combined_df, 
          file = "world_bank_combined_gdp_per_capita_income_data.csv", 
          row.names = FALSE)