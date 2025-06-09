library(tidyverse)
library(lubridate)
library(dplyr)

# Read dataframe
df_raw <- read.csv("df_fuel_ckan.csv", stringsAsFactors = FALSE)

df <- df_raw %>% 
  mutate(datetime = ymd_hms(DATETIME)) %>% 
  arrange(datetime) %>% 
  transmute(datetime,
            fossil   = GAS + COAL,
            renewable = WIND + HYDRO + SOLAR + BIOMASS,
            other     = NUCLEAR + IMPORTS + STORAGE + OTHER) %>% 
  mutate(total_mw = fossil + renewable + other,
         # now closed to 1
         across(fossil:other, ~ .x / total_mw)) 

# Count the number of zeros
zero_per_part <- df %>%
  summarise(across(fossil:other, ~ sum(.x == 0L)))

total_zeros <- df %>%
  summarise(across(fossil:other, ~ sum(.x == 0L))) %>%
  rowSums()


df_yearly <- df %>% 
  mutate(year = year(datetime)) %>% 
  filter(between(year, 2009, 2024)) %>%
  group_by(year) %>% 
  summarise(across(fossil:other, mean, na.rm = TRUE),
            .groups = "drop") %>% 
  pivot_longer(fossil:other,
               names_to  = "source",
               values_to = "share")

# 1. Hyearly aggregated plot
ggplot(df_yearly,
       aes(x = year, y = share, fill = source)) +
  geom_area(color = "grey20", size = 0.2, position = "stack") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("fossil"    = "#3182bd",
                               "renewable" = "#31a354",
                               "other"     = "#756bb1"),
                    name = NULL) +
  labs(title = "Mean annual generation mix, Great Britain (2009 – 2024)",
       x = NULL, y = "Average share of total generation") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank())

df_month <- df %>% 
  filter(year(datetime) %in% 2009:2024) %>% 
  mutate(month_date = as.Date(floor_date(datetime, unit = "month"))) %>% 
  group_by(month_date) %>% 
  summarise(across(fossil:other, mean, na.rm = TRUE),
            .groups = "drop") %>% 
  pivot_longer(fossil:other,
               names_to  = "source",
               values_to = "share") %>% 
  arrange(month_date, source)

# 2.monthly aggregated plot
ggplot(df_month,
       aes(x = month_date, y = share, fill = source)) +
  geom_area(position = "stack", colour = "grey20", size = 0.15) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  scale_fill_manual(values = c("fossil"    = "#3182bd",
                               "renewable" = "#31a354",
                               "other"     = "#756bb1"),
                    name = NULL) +
  labs(title = "Monthly composition of generation mix (Jan 2009 – Dec 2024)",
       x = NULL, y = "Mean share in month") +
  theme_minimal(base_size = 11) +
  theme(legend.position   = "right",
        panel.grid.minor = element_blank())

df_daily <- df %>% 
  mutate(date = as.Date(datetime),
         year = year(date)) %>% 
  filter(year %in% c(2016, 2024)) %>%
  group_by(year, date) %>% 
  summarise(across(fossil:other, mean, na.rm = TRUE),
            .groups = "drop") %>% 
  pivot_longer(fossil:other,
               names_to  = "source",
               values_to = "share") %>% 
  arrange(year, date, source)

# 3.Daily aggregated plot
ggplot(df_daily,
       aes(x = date, y = share, fill = source)) +
  geom_area(colour = "grey15", size = 0.1, position = "stack") +
  # optional: add a 7-day smoother for each source
  geom_smooth(aes(colour = source), method = "loess", span = 0.2,
              se = FALSE, linetype = "solid", size = 0.35, alpha = 0.6, 
              data = ~ .x) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = expansion(mult = 0)) +
  scale_fill_manual(values = c(fossil    = "#3182bd",
                               renewable = "#31a354",
                               other     = "#756bb1"),
                    name = NULL) +
  scale_colour_manual(values = c(fossil    = "#08519c",
                                 renewable = "#006d2c",
                                 other     = "#54278f"),
                      guide = "none") +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  labs(title = "Daily mean generation mix overlaid with rolling means",
       subtitle = "Great Britain — comparison of 2016 and 2024",
       x = NULL, y = "Share of total generation") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right",
        strip.text      = element_text(face = "bold"))

target_year <- 2024
df_sub <- df %>% 
  filter(year(datetime) == target_year) 

df_clock <- df_sub %>% 
  
  mutate(slot_time = floor_date(datetime, unit = "30 minutes"),
         hour      = hour(slot_time),
         minute    = minute(slot_time),
         tod_num   = hour + minute / 60) %>%        # 0.00, 0.50, …, 23.5
  group_by(tod_num) %>% 
  summarise(across(fossil:other, mean, na.rm = TRUE),
            .groups = "drop") %>% 
  pivot_longer(fossil:other,
               names_to  = "source",
               values_to = "share") %>% 
  arrange(tod_num, source)



# 4. Hourly plot
ggplot(df_clock, aes(tod_num, share, fill = source)) +
  geom_area(colour = "grey25", size = 0.15, position = "stack") +
  scale_x_continuous(breaks = seq(0, 22, 2),
                     labels = c("0", "2", "4", "6", "8", "10", "12",
                                "14", "16", "18", "20", "22"),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = 0)) +
  scale_fill_manual(values = c(fossil="#3182bd", renewable="#31a354", other="#756bb1"),
                    name = NULL) +
  labs(title = glue::glue("Diurnal composition, {target_year}"),
       x = "Hour of day", y = "Mean share") +
  theme_minimal(base_size = 11) +
  theme(legend.position   = "right",
        panel.grid.minor = element_blank())
