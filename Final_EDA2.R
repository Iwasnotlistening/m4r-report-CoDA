library(tidyverse)
library(lubridate)
library(dplyr)

# Read the dataframe
df_raw <- read.csv("df_fuel_ckan.csv", stringsAsFactors = FALSE)

df <- df_raw %>% 
  mutate(datetime = ymd_hms(DATETIME)) %>% 
  arrange(datetime) %>% 
  transmute(datetime,
            fossil   = GAS + COAL,
            renewable = WIND + HYDRO + SOLAR + BIOMASS,
            other     = NUCLEAR + IMPORTS + STORAGE + OTHER) %>% 
  mutate(total_mw = fossil + renewable + other,
         across(fossil:other, ~ .x / total_mw)) 

df_posit <- df %>%
  mutate(across(fossil:other, ~ pmax(.x, 1e-5)))

# Translate to ILR
df_ilr <- df_posit %>% 
  transmute(
    datetime,
    b1 = sqrt(2/3) * (log(fossil) - 0.5 * (log(renewable) + log(other))),
    b2 = sqrt(1/2) * (log(renewable) - log(other))
  )

df_30 <- df_ilr %>% 
  mutate(date      = as.Date(datetime),
         block_30d = floor_date(date, unit = "30 days")) %>%
  filter(block_30d >= as.Date("2009-01-01"),
         block_30d <= as.Date("2024-12-31")) %>%
  group_by(block_30d) %>% 
  summarise(across(b1:b2, mean, na.rm = TRUE),
            .groups = "drop")

df_long <- df_30 %>%
  pivot_longer(b1:b2,
               names_to = "coordinate",
               values_to = "value") %>%
  mutate(coordinate = recode(coordinate,
                             b1 = "Fossil vs Non-Fossil",
                             b2 = "Renewable vs Other"))

# Plot b1 and b2 against time.
ggplot(df_long,
       aes(x = block_30d, y = value)) +
  geom_line(colour = "steelblue", size = 0.35) +
  geom_point(size = 1, colour = "steelblue") +
  facet_wrap(~ coordinate, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title   = "Thirty-day mean ILR coordinates (Jan 2009 – Dec 2024)",
       x       = NULL,
       y       = "ILR value (orthonormal scale)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot")







