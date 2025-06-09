library(easyCODA)
library(vcd)
library(tidyverse)
library(lubridate)

# Importing Dataframe

df <- read.csv("df_fuel_ckan.csv", 
               header = TRUE,
               stringsAsFactors = FALSE) 


df_3cats <- data.frame(
  Datetime = ymd_hms(df$DATETIME),
  Fossil = df$GAS + df$COAL,
  Renewables = df$WIND + df$HYDRO + df$SOLAR + df$BIOMASS,
  Other = df$NUCLEAR + df$IMPORTS + df$STORAGE + df$OTHER,
  year = year(Datetime),
  month = month(Datetime)
)


# Summarize columns by 'Year'
df_yearly <- aggregate(
  cbind(Fossil, Renewables, Other) ~ year,
  data = df_3cats, 
  FUN = sum
)


# Creating a compositional df
df_yearly_comp <- df_yearly %>%
  mutate(
    total = Fossil + Renewables + Other,
    Fossil_prop = Fossil / total,
    Renewables_prop = Renewables / total,
    Other_prop = Other / total
  )

# Ternary Plot
ternaryplot(
  df_yearly_comp[, c("Fossil_prop", "Renewables_prop", "Other_prop")],
  dimnames = c("% Fossil", "% Renewables", "% Other/Low-carbon"),
  main = "Ternary Plot of Annual Compositions",
  id = df_yearly_comp$year,
  cex = 0.6,
  pch = 19,
  col = "blue"
)




