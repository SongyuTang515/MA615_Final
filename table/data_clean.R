library(readxl)
library(tidyverse)

gdp <- read_excel("Maldives_GDP.xlsx")
gdp <- gdp %>% select(,-1)
gdp_values <- as.numeric(gdp[4, -1])
years <- seq(2003, 2023)
gdp_data <- data.frame(Year = years, GDP = gdp_values)
gr <- read_excel("Maldives_GDP_growth_rate.xlsx")
gr <- gr %>% select(,-1)
gr_values <- as.numeric(gr[4,-1])
new_years <- seq(2004, 2023)
new_data <- data.frame(Year = new_years, Growth_Rate = gr_values)
combined_data <- merge(gdp_data, new_data, by = "Year", all = TRUE)

write.csv(combined_data, file = "gdp.csv", row.names = FALSE)

cpi <- read_excel("cpi.xlsx")
subset_data <- cpi[8:46, c(1, 2, 7)]
colnames(subset_data) <- c("Year", "CPI", "INFLATION")

write.csv(subset_data, file = "cpi.csv", row.names = FALSE)
